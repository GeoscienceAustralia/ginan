import { beforeEach, describe, expect, it } from "vitest";
import { fireEvent, render, screen } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import ProcessingParamControl from "./ProcessingParamControl";
import type { ParamDef } from "../config/processingParams";
import { resetAppStore, store } from "../test/storeHelpers";

beforeEach(() => {
  resetAppStore();
});

const boolParam: ParamDef = {
  path: "receiver_options.global.models.tides.enable",
  label: "Tidal displacements",
  type: "boolean",
  defaultValue: true,
};

const numberParam: ParamDef = {
  path: "receiver_options.global.elevation_mask",
  label: "Elevation mask",
  type: "number",
  defaultValue: 15,
  min: 0,
  max: 90,
  step: 1,
  unit: "°",
};

const enumParam: ParamDef = {
  path: "satellite_options.global.error_model",
  label: "Error model",
  type: "enum",
  defaultValue: "ELEVATION_DEPENDENT",
  options: [
    { value: "ELEVATION_DEPENDENT", label: "Elevation dependent" },
    { value: "UNIFORM", label: "Uniform" },
  ],
};

const stringParam: ParamDef = {
  path: "some_section.label",
  label: "Label",
  type: "string",
  defaultValue: "default text",
};

const resetButton = () => screen.queryByTitle(/Reset to default/);

describe("rendering the template default", () => {
  it("shows a boolean's default when nothing is stored", () => {
    render(<ProcessingParamControl def={boolParam} />);
    expect(screen.getByRole("checkbox")).toBeChecked();
  });

  it("shows a number's default and its bounds", () => {
    render(<ProcessingParamControl def={numberParam} />);
    const input = screen.getByRole("spinbutton");
    expect(input).toHaveValue(15);
    expect(input).toHaveAttribute("min", "0");
    expect(input).toHaveAttribute("max", "90");
    expect(screen.getByText("°")).toBeInTheDocument();
  });

  it("shows an enum's default and every option", () => {
    render(<ProcessingParamControl def={enumParam} />);
    expect(screen.getByRole("combobox")).toHaveValue("ELEVATION_DEPENDENT");
    expect(screen.getAllByRole("option").map((o) => o.textContent)).toEqual([
      "Elevation dependent",
      "Uniform",
    ]);
  });

  it("shows a string's default", () => {
    render(<ProcessingParamControl def={stringParam} />);
    expect(screen.getByRole("textbox")).toHaveValue("default text");
  });

  it("shows a stored override rather than the default", () => {
    store().setProcessingParam(numberParam.path, 20);
    render(<ProcessingParamControl def={numberParam} />);
    expect(screen.getByRole("spinbutton")).toHaveValue(20);
  });
});

describe("committing a change", () => {
  it("stores a boolean moved off the default", async () => {
    render(<ProcessingParamControl def={boolParam} />);

    await userEvent.click(screen.getByRole("checkbox"));

    expect(store().processingParams).toEqual({ [boolParam.path]: false });
  });

  it("stores an enum choice", async () => {
    render(<ProcessingParamControl def={enumParam} />);

    await userEvent.selectOptions(screen.getByRole("combobox"), "UNIFORM");

    expect(store().processingParams).toEqual({ [enumParam.path]: "UNIFORM" });
  });

  it("stores typed text", async () => {
    render(<ProcessingParamControl def={stringParam} />);

    await userEvent.type(screen.getByRole("textbox"), "!");

    expect(store().processingParams).toEqual({ [stringParam.path]: "default text!" });
  });

  it("stores a typed number", () => {
    render(<ProcessingParamControl def={numberParam} />);

    fireEvent.change(screen.getByRole("spinbutton"), { target: { value: "20" } });

    expect(store().processingParams).toEqual({ [numberParam.path]: 20 });
  });

  it("ignores an unparseable number rather than storing NaN", () => {
    render(<ProcessingParamControl def={numberParam} />);

    fireEvent.change(screen.getByRole("spinbutton"), { target: { value: "abc" } });

    expect(store().processingParams).toEqual({});
  });

  it("drops the key rather than storing the default value back", async () => {
    store().setProcessingParam(boolParam.path, false);
    render(<ProcessingParamControl def={boolParam} />);

    await userEvent.click(screen.getByRole("checkbox"));

    expect(store().processingParams).toEqual({});
  });

  it("falls back to the default when a number field is emptied", async () => {
    store().setProcessingParam(numberParam.path, 20);
    render(<ProcessingParamControl def={numberParam} />);

    await userEvent.clear(screen.getByRole("spinbutton"));

    expect(store().processingParams).toEqual({});
  });
});

describe("the reset affordance", () => {
  it("appears only once the value differs from the default", () => {
    const { rerender } = render(<ProcessingParamControl def={numberParam} />);
    expect(resetButton()).toBeNull();

    store().setProcessingParam(numberParam.path, 20);
    rerender(<ProcessingParamControl def={numberParam} />);
    expect(resetButton()).not.toBeNull();
  });

  it("stays hidden for a value explicitly set back to the default", () => {
    store().setProcessingParam(numberParam.path, 15);
    render(<ProcessingParamControl def={numberParam} />);
    expect(resetButton()).toBeNull();
  });

  it("names the value it will restore", () => {
    store().setProcessingParam(numberParam.path, 20);
    render(<ProcessingParamControl def={numberParam} />);
    expect(resetButton()).toHaveAttribute("title", "Reset to default (15)");
  });

  it("removes the override when pressed", async () => {
    store().setProcessingParam(numberParam.path, 20);
    render(<ProcessingParamControl def={numberParam} />);

    await userEvent.click(resetButton()!);

    expect(store().processingParams).toEqual({});
  });
});
