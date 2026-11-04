import { beforeEach, describe, expect, it } from "vitest";
import { render, screen, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import ConfigYamlInspector from "./ConfigYamlInspector";
import { makeRinexData, resetAppStore, store } from "../test/storeHelpers";
import { lastInvokeArgs, mockInvoke } from "../test/tauriMocks";

const YAML = "outputs:\n  root_directory: /out\n";

beforeEach(() => {
  resetAppStore();
});

function readyForFileRun() {
  store().setOutputDir("/out");
  store().setRunName("run1");
  store().setProductsDir("/products");
  store().setRinexPath("/data/ALIC.rnx");
  store().setRinexData(makeRinexData());
}

/** The two commands the panel calls on every refresh. */
function mockPreview(yaml = YAML, exists = false, contents: string | null = null) {
  mockInvoke({
    preview_config_yaml: yaml,
    config_file_status: { path: "/out/ppp_config.yaml", exists, contents },
  });
}

describe("the empty states", () => {
  it("asks for an output directory first", () => {
    render(<ConfigYamlInspector />);
    expect(
      screen.getByText("Choose an output directory to preview the PEA config."),
    ).toBeInTheDocument();
  });

  it("asks for an observation file in file mode", () => {
    store().setOutputDir("/out");
    render(<ConfigYamlInspector />);
    expect(
      screen.getByText("Load a RINEX observation file to preview its PEA config."),
    ).toBeInTheDocument();
  });

  it("asks for a stream in realtime mode", () => {
    store().setOutputDir("/out");
    store().setInputMode("realtime");
    store().updateRealtime({ obs_streams: [] });
    render(<ConfigYamlInspector />);
    expect(
      screen.getByText("Configure an observation stream to preview its PEA config."),
    ).toBeInTheDocument();
  });
});

describe("the preview", () => {
  it("renders the YAML the writer would produce", async () => {
    readyForFileRun();
    mockPreview();
    render(<ConfigYamlInspector />);

    await waitFor(() => expect(screen.getByText(/root_directory/)).toBeInTheDocument());
  });

  it("previews from the shared config inputs, so it cannot drift from a run", async () => {
    readyForFileRun();
    mockPreview();
    render(<ConfigYamlInspector />);

    await waitFor(() => expect(lastInvokeArgs("preview_config_yaml")).toBeDefined());
    expect(lastInvokeArgs("preview_config_yaml")!.inputs).toMatchObject({
      rinex_path: "/data/ALIC.rnx",
      output_dir: "/out/run1",
      marker_name: "ALIC",
    });
  });

  it("shows the generation error instead of a stale preview", async () => {
    readyForFileRun();
    mockInvoke({
      preview_config_yaml: () => Promise.reject(new Error("unknown key")),
      config_file_status: { path: "/out/ppp_config.yaml", exists: false, contents: null },
    });
    render(<ConfigYamlInspector />);

    await waitFor(() =>
      expect(screen.getByText(/Could not generate config/)).toBeInTheDocument(),
    );
  });

  it("regenerates when a config input changes", async () => {
    readyForFileRun();
    mockPreview();
    render(<ConfigYamlInspector />);
    await waitFor(() => expect(screen.getByText(/root_directory/)).toBeInTheDocument());

    mockInvoke({
      preview_config_yaml: "processing_options:\n  mode: kinematic\n",
      config_file_status: { path: "/out/ppp_config.yaml", exists: false, contents: null },
    });
    store().setMode("Kinematic");

    await waitFor(() => expect(screen.getByText(/kinematic/)).toBeInTheDocument());
  });

  it("regenerates on request", async () => {
    readyForFileRun();
    mockPreview();
    render(<ConfigYamlInspector />);
    await waitFor(() => expect(screen.getByText(/root_directory/)).toBeInTheDocument());

    mockInvoke({
      preview_config_yaml: "regenerated: true\n",
      config_file_status: { path: "/out/ppp_config.yaml", exists: false, contents: null },
    });
    await userEvent.click(screen.getByRole("button", { name: /Refresh/ }));

    await waitFor(() => expect(screen.getByText(/regenerated/)).toBeInTheDocument());
  });
});

/**
 * Refresh regenerates (the test above proves that), but the effect already
 * regenerates on every change that feeds the config, so a press nearly always
 * produces identical YAML and nothing on screen moves. These cover the outcome
 * it reports instead.
 */
describe("what Refresh reports", () => {
  const refresh = () => userEvent.click(screen.getByRole("button", { name: /Refresh/ }));

  it("says so when the regenerated config is identical", async () => {
    readyForFileRun();
    mockPreview();
    render(<ConfigYamlInspector />);
    await waitFor(() => expect(screen.getByText(/root_directory/)).toBeInTheDocument());

    await refresh();

    await waitFor(() => expect(screen.getByText("No changes")).toBeInTheDocument());
    expect(screen.queryByText("Updated")).not.toBeInTheDocument();
  });

  it("says it updated when the config actually changed", async () => {
    readyForFileRun();
    mockPreview();
    render(<ConfigYamlInspector />);
    await waitFor(() => expect(screen.getByText(/root_directory/)).toBeInTheDocument());

    mockInvoke({
      preview_config_yaml: "regenerated: true\n",
      config_file_status: { path: "/out/ppp_config.yaml", exists: false, contents: null },
    });
    await refresh();

    await waitFor(() => expect(screen.getByText("Updated")).toBeInTheDocument());
  });

  it("notices a change on disk even when the preview is byte-identical", async () => {
    readyForFileRun();
    mockPreview();
    render(<ConfigYamlInspector />);
    await waitFor(() => expect(screen.getByText(/root_directory/)).toBeInTheDocument());

    // The same YAML, but the file has appeared since — which is the one thing
    // only a manual refresh can pick up.
    mockPreview(YAML, true, YAML);
    await refresh();

    await waitFor(() => expect(screen.getByText("Updated")).toBeInTheDocument());
    expect(screen.getByText("exists on disk")).toBeInTheDocument();
  });

  it("clears the outcome so it reads as a reply, not as status", async () => {
    readyForFileRun();
    mockPreview();
    render(<ConfigYamlInspector />);
    await waitFor(() => expect(screen.getByText(/root_directory/)).toBeInTheDocument());

    await refresh();
    await waitFor(() => expect(screen.getByText("No changes")).toBeInTheDocument());

    await waitFor(
      () => expect(screen.queryByText("No changes")).not.toBeInTheDocument(),
      { timeout: 4000 },
    );
  });
});

describe("the file on disk", () => {
  it("says the config has not been written yet", async () => {
    readyForFileRun();
    mockPreview();
    render(<ConfigYamlInspector />);

    await waitFor(() => expect(screen.getByText("not written yet")).toBeInTheDocument());
    expect(screen.getByText("/out/ppp_config.yaml")).toBeInTheDocument();
  });

  it("says the config already exists", async () => {
    readyForFileRun();
    mockPreview(YAML, true, YAML);
    render(<ConfigYamlInspector />);

    await waitFor(() => expect(screen.getByText("exists on disk")).toBeInTheDocument());
  });

  it("names the realtime config when a stream is configured", async () => {
    store().setOutputDir("/out");
    store().setInputMode("realtime");
    store().updateRealtime({ obs_streams: ["ALIC00AUS0"] });
    mockInvoke({
      preview_config_yaml: YAML,
      config_file_status: { path: "/out/rt_ppp_config.yaml", exists: false, contents: null },
    });
    render(<ConfigYamlInspector />);

    await waitFor(() =>
      expect(screen.getByText("/out/rt_ppp_config.yaml")).toBeInTheDocument(),
    );
  });

  it("tells the backend which config to look for", async () => {
    store().setOutputDir("/out");
    store().setRunName("run1");
    store().setInputMode("realtime");
    store().updateRealtime({ obs_streams: ["ALIC00AUS0"] });
    mockPreview();
    render(<ConfigYamlInspector />);

    await waitFor(() =>
      expect(lastInvokeArgs("config_file_status")).toEqual({
        outputDir: "/out/run1",
        realtime: true,
      }),
    );
  });
});

describe("the overwrite switch", () => {
  it("is on by default and stays quiet", async () => {
    readyForFileRun();
    mockPreview();
    const { container } = render(<ConfigYamlInspector />);

    await waitFor(() => expect(screen.getByText(/root_directory/)).toBeInTheDocument());
    expect(screen.getByRole("checkbox", { name: /Overwrite on run/ })).toBeChecked();
    expect(container.querySelector(".yaml-notice")).toBeNull();
  });

  it("explains that hand edits survive once it is off", async () => {
    readyForFileRun();
    mockPreview();
    render(<ConfigYamlInspector />);
    await waitFor(() => expect(screen.getByText(/root_directory/)).toBeInTheDocument());

    await userEvent.click(screen.getByRole("checkbox", { name: /Overwrite on run/ }));

    expect(store().configOverwrite).toBe(false);
    expect(screen.getByText(/Hand edits are\s+preserved/)).toBeInTheDocument();
  });

  it("warns when the file on disk differs from the preview", async () => {
    readyForFileRun();
    store().setConfigOverwrite(false);
    mockPreview(YAML, true, "something: else\n");
    const { container } = render(<ConfigYamlInspector />);

    await waitFor(() =>
      expect(container.querySelector(".yaml-notice.is-warn")).not.toBeNull(),
    );
    expect(screen.getByText(/will use the file on disk/)).toBeInTheDocument();
  });

  it("does not warn when the file on disk already matches", async () => {
    readyForFileRun();
    store().setConfigOverwrite(false);
    mockPreview(YAML, true, YAML);
    const { container } = render(<ConfigYamlInspector />);

    await waitFor(() => expect(screen.getByText("exists on disk")).toBeInTheDocument());
    expect(container.querySelector(".yaml-notice.is-warn")).toBeNull();
  });
});

describe("copying", () => {
  it("copies the YAML and confirms it", async () => {
    readyForFileRun();
    mockPreview();
    render(<ConfigYamlInspector />);
    await waitFor(() => expect(screen.getByText(/root_directory/)).toBeInTheDocument());

    await userEvent.click(screen.getByRole("button", { name: /Copy/ }));

    expect(navigator.clipboard.writeText).toHaveBeenCalledWith(YAML);
    expect(screen.getByRole("button", { name: /Copied/ })).toBeInTheDocument();
  });
});
