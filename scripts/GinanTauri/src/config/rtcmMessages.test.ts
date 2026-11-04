import { describe, expect, it } from "vitest";
import { describeMessage } from "./rtcmMessages";

describe("describeMessage", () => {
  it("knows the tabled messages", () => {
    expect(describeMessage("1077")).toMatchObject({ name: "MSM7 GPS", group: "msm", constellation: "GPS" });
    expect(describeMessage(1127)).toMatchObject({ group: "msm", constellation: "BDS" });
    expect(describeMessage("1019").group).toBe("ephemeris");
    expect(describeMessage("1033").group).toBe("station");
    expect(describeMessage("1230").group).toBe("bias");
    expect(describeMessage("1060")).toMatchObject({ group: "ssr", constellation: "GPS" });
    expect(describeMessage("4076").group).toBe("ssr");
  });

  it("derives MSM1-3 and falls back for unknown numbers", () => {
    expect(describeMessage("1073")).toMatchObject({ name: "MSM3 GPS", group: "msm" });
    expect(describeMessage("9999")).toMatchObject({ name: "Message 9999", group: "other", constellation: null });
  });
});
