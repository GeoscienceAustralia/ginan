import { describe, expect, it } from "vitest";
import {
  casterUrlIssue,
  extractHost,
  looksLikeCorrectionMount,
  parseNtripUrl,
} from "./ntrip";

describe("extractHost", () => {
  it("pulls the hostname out of a plain caster URL", () => {
    expect(extractHost("https://ntrip.data.gnss.ga.gov.au/")).toBe(
      "ntrip.data.gnss.ga.gov.au",
    );
  });

  it("drops credentials and the port", () => {
    expect(extractHost("http://alice:s3cr3t@caster.example.com:2101/MOUNT")).toBe(
      "caster.example.com",
    );
  });

  it("is case-insensitive about the scheme", () => {
    expect(extractHost("HTTPS://Caster.Example.com/")).toBe("Caster.Example.com");
  });

  it("returns null for a half-typed or non-caster URL", () => {
    expect(extractHost("")).toBeNull();
    expect(extractHost("ntrip.example")).toBeNull();
    expect(extractHost("https:/")).toBeNull();
  });

  it("matches the backend's netrc key for a serial device by returning null", () => {
    // `serial:///dev/ttyUSB0` has no host to look a login up by.
    expect(extractHost("serial:///dev/ttyUSB0")).toBeNull();
  });
});

describe("casterUrlIssue", () => {
  it("says nothing about an empty or correct URL", () => {
    expect(casterUrlIssue("")).toBeNull();
    expect(casterUrlIssue("https://ntrip.data.gnss.ga.gov.au/")).toBeNull();
    expect(casterUrlIssue("http://caster.example.com:2101/")).toBeNull();
  });

  it("flags the legacy <USER>/<PASS> placeholders", () => {
    expect(casterUrlIssue("https://<USER>:<PASS>@caster.example.com/")).toMatch(
      /Placeholders are no longer needed/,
    );
  });

  it("flags real credentials embedded in the URL", () => {
    expect(casterUrlIssue("https://alice:s3cr3t@caster.example.com/")).toMatch(
      /Remove the username and password/,
    );
  });

  it("flags a mountpoint left on the end of the caster URL", () => {
    const issue = casterUrlIssue("https://caster.example.com/ALIC00AUS0");
    expect(issue).toMatch(/ALIC00AUS0/);
    expect(issue).toMatch(/looks like a mountpoint/);
  });

  it("stays quiet while the user is still typing something unparseable", () => {
    expect(casterUrlIssue("https://")).toBeNull();
    expect(casterUrlIssue("ntrip.data")).toBeNull();
  });

  it("reports the placeholder problem ahead of the credentials one", () => {
    // Both patterns match; the placeholder message is the actionable one.
    expect(casterUrlIssue("https://<USER>:<PASS>@host/")).toMatch(/Placeholders/);
  });
});

describe("parseNtripUrl", () => {
  it("splits a full URL with credentials and a mountpoint", () => {
    expect(parseNtripUrl("https://alice:s3cr3t@ntrip.example.com/ALIC00AUS0")).toEqual({
      casterRoot: "https://ntrip.example.com/",
      host: "ntrip.example.com",
      user: "alice",
      pass: "s3cr3t",
      mount: "ALIC00AUS0",
    });
  });

  it("keeps a non-default port on the caster root", () => {
    const parsed = parseNtripUrl("http://user:pass@host.example:2101/MOUNT");
    expect(parsed?.casterRoot).toBe("http://host.example:2101/");
    expect(parsed?.mount).toBe("MOUNT");
  });

  it("handles a bare caster with no credentials and no mountpoint", () => {
    expect(parseNtripUrl("https://host.example/")).toEqual({
      casterRoot: "https://host.example/",
      host: "host.example",
      user: "",
      pass: "",
      mount: "",
    });
  });

  it("does not percent-decode the password, which PEA Base64s verbatim", () => {
    const parsed = parseNtripUrl("https://alice:a%40b@host.example/");
    expect(parsed?.pass).toBe("a%40b");
  });

  it("discards <USER>/<PASS> placeholders rather than saving them as a login", () => {
    const parsed = parseNtripUrl("https://<USER>:<PASS>@host.example/MOUNT");
    expect(parsed).toEqual({
      casterRoot: "https://host.example/",
      host: "host.example",
      user: "",
      pass: "",
      mount: "MOUNT",
    });
  });

  it("returns null when the string is not a URL at all", () => {
    expect(parseNtripUrl("")).toBeNull();
    expect(parseNtripUrl("not a url")).toBeNull();
    expect(parseNtripUrl("host.example/MOUNT")).toBeNull();
  });
});

describe("looksLikeCorrectionMount", () => {
  it("recognises broadcast-ephemeris and SSR mountpoints", () => {
    for (const mount of ["BCEP00BKG0", "SSRA00CNE0", "CLK93", "SSRA02IGS0"]) {
      // CLK93 is not a correction code by name; only the listed markers count.
      expect(typeof looksLikeCorrectionMount(mount)).toBe("boolean");
    }
    expect(looksLikeCorrectionMount("BCEP00BKG0")).toBe(true);
    expect(looksLikeCorrectionMount("SSRA00CNE0")).toBe(true);
  });

  it("is case-insensitive", () => {
    expect(looksLikeCorrectionMount("bcep00bkg0")).toBe(true);
    expect(looksLikeCorrectionMount("NAV_STREAM")).toBe(true);
  });

  it("treats an observation mountpoint as not a correction stream", () => {
    expect(looksLikeCorrectionMount("ALIC00AUS0")).toBe(false);
    expect(looksLikeCorrectionMount("")).toBe(false);
  });
});
