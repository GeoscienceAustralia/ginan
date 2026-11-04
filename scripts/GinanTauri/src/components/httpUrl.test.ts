import { describe, expect, it } from "vitest";
import { httpMount, isHttpUrl, maskLogin } from "./httpUrl";

describe("isHttpUrl", () => {
  it.each([
    "https://ntrip.example.org/ALIC00AUS0",
    "http://h:2101/x",
    "http://user:pw@h.example/x",
    "https://h.example",
  ])("accepts %s", (value) => {
    expect(isHttpUrl(value)).toBe(true);
  });

  it.each([
    "",
    "ftp://h/x",
    "h.example/x",
    "https://",
    "https:///x",
    "https://h .example/x",
  ])("rejects %j", (value) => {
    expect(isHttpUrl(value)).toBe(false);
  });
});

describe("httpMount", () => {
  it.each([
    ["https://ntrip.example.org/ALIC00AUS0", "ALIC00AUS0"],
    ["https://h/x/", "x"],
    ["https://h.example", "https://h.example"],
  ])("%s -> %s", (value, expected) => {
    expect(httpMount(value)).toBe(expected);
  });
});

describe("maskLogin", () => {
  it("replaces the login with ***", () => {
    expect(maskLogin("https://u:p@h.example/x")).toBe("https://***@h.example/x");
  });

  it("leaves a URL without a login unchanged", () => {
    expect(maskLogin("https://h.example/x")).toBe("https://h.example/x");
  });
});
