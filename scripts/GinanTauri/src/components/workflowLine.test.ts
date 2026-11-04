import { describe, expect, it } from "vitest";
import { parseWorkflowLine } from "./WorkflowPanel";

/**
 * The workflow timeline is derived entirely from the raw console strings the
 * backend already emits — there is no structured event contract behind it. That
 * makes this classifier the whole feature, and a regression here silently turns
 * a failed run into an informational "Update".
 */

describe("parseWorkflowLine — message cleanup", () => {
  it("strips ANSI colour codes", () => {
    expect(parseWorkflowLine("\x1b[32mCDDIS connectivity OK.\x1b[0m").message).toBe(
      "CDDIS connectivity OK.",
    );
  });

  it("strips the leading status emoji", () => {
    expect(parseWorkflowLine("✅ PEA finished with exit code 0").message).toBe(
      "PEA finished with exit code 0",
    );
    expect(parseWorkflowLine("❌ PEA error: boom").message).toBe("PEA error: boom");
    expect(parseWorkflowLine("📡 Starting PEA in realtime mode...").message).toBe(
      "Starting PEA in realtime mode...",
    );
  });

  it("strips an [ERR] prefix", () => {
    expect(parseWorkflowLine("[ERR] something broke").message).toBe("something broke");
  });

  it("trims surrounding whitespace", () => {
    expect(parseWorkflowLine("   spaced out   ").message).toBe("spaced out");
  });

  it("substitutes a placeholder rather than rendering an empty row", () => {
    expect(parseWorkflowLine("✅").message).toBe("Workflow update");
    expect(parseWorkflowLine("   ").message).toBe("Workflow update");
  });
});

describe("parseWorkflowLine — status", () => {
  const status = (line: string, active = false) => parseWorkflowLine(line, active).status;

  it("reads an error marker as an error", () => {
    expect(status("❌ PEA error: boom")).toBe("error");
    expect(status("[ERR] segfault")).toBe("error");
    expect(status("✗ download failed")).toBe("error");
  });

  it("reads a non-zero exit code as an error and zero as success", () => {
    expect(status("PEA finished with exit code 1")).toBe("error");
    expect(status("PEA finished with exit code 137")).toBe("error");
    expect(status("✅ PEA finished with exit code 0")).toBe("success");
  });

  it("reads a warning marker as attention", () => {
    expect(status("⚠ GPS: no overlap with BIA")).toBe("warning");
    expect(status("⚠️ BIA fetch failed")).toBe("warning");
  });

  it("reads failure words as an error", () => {
    expect(status("ensure_products failed: disk full")).toBe("error");
    expect(status("Could not prepare file.rnx")).toBe("error");
    expect(status("CDDIS unavailable")).toBe("error");
  });

  it("reads skip markers as skipped", () => {
    expect(status("⏭ nothing to do")).toBe("skipped");
    expect(status("Skipping product download")).toBe("skipped");
    expect(status("🛑 Processing cancelled by user.")).toBe("skipped");
  });

  it("reads completion words as success", () => {
    expect(status("Static support files ready.")).toBe("success");
    expect(status("CDDIS connectivity OK.")).toBe("success");
    expect(status("Already plain RINEX")).toBe("success");
  });

  it("marks an in-flight verb active only on the newest line of a live run", () => {
    expect(status("Downloading IGS0OPSFIN.SP3.gz", true)).toBe("active");
    expect(status("Downloading IGS0OPSFIN.SP3.gz", false)).toBe("info");
  });

  it("falls back to an informational update", () => {
    expect(status("BIA validation for IGS/FIN/OPS:")).toBe("info");
  });

  it("gives every status a human label", () => {
    for (const line of [
      "❌ boom",
      "⚠ careful",
      "⏭ skipped",
      "✅ done",
      "BIA validation for IGS/FIN/OPS:",
    ]) {
      expect(parseWorkflowLine(line).statusLabel.length).toBeGreaterThan(0);
    }
  });
});

describe("parseWorkflowLine — category", () => {
  const category = (line: string) => parseWorkflowLine(line).category;

  it("routes PEA lines to Processing", () => {
    expect(category("⚙️ Starting PEA execution in background...")).toBe("Processing");
    expect(category("🛑 Processing cancelled by user.")).toBe("Processing");
  });

  it("routes product lines to Correction data", () => {
    expect(category("Checking SP3/CLK/BRDC/BIA for observation date(s)")).toBe(
      "Correction data",
    );
    expect(category("date-dependent products ready")).toBe("Correction data");
  });

  it("routes ANTEX and static files to Support files", () => {
    expect(category("ANTEX file: igs20.atx")).toBe("Support files");
    expect(category("Checking static support files in products directory...")).toBe(
      "Support files",
    );
  });

  it("routes connectivity lines to Connection", () => {
    expect(category("CDDIS connectivity OK.")).toBe("Connection");
  });

  it("routes observation-file lines to Observation data", () => {
    expect(category("Preparing ALIC.crx.gz...")).toBe("Observation data");
    expect(category("RINEX header read")).toBe("Observation data");
  });

  it("routes binary lines to Application setup", () => {
    expect(category("Using bundled binary, version 3.0")).toBe("Application setup");
  });

  it("prefers the earlier rule when a line matches more than one", () => {
    // "pea binary" is both Processing and Application setup; the first rule
    // wins, and the run itself is the more useful heading.
    expect(category("pea binary version 3.0")).toBe("Processing");
  });

  it("falls back to Workflow", () => {
    expect(category("Workspace restored from the last session")).toBe("Workflow");
  });

  it("categorises the stripped message, not the raw marker", () => {
    expect(category("✅ CDDIS connectivity OK.")).toBe("Connection");
  });
});
