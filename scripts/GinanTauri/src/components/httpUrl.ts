/** `http(s)://[login@]host[:port][/path]`, no whitespace. */
export function isHttpUrl(value: string): boolean {
  return /^https?:\/\/([^\s/@]*@)?[^\s/@:]+(:\d+)?(\/\S*)?$/i.test(value);
}

/** Last path segment of a URL: the mountpoint PEA requests (`ALIC00AUS0`). */
export function httpMount(value: string): string {
  const path = value.replace(/^[a-z]+:\/\/[^/]*/i, "");
  return path.split("/").filter(Boolean).pop() ?? value;
}

/** The URL with any `login@` replaced by `***@`, safe for a tooltip. */
export function maskLogin(value: string): string {
  return value.replace(/^([a-z]+:\/\/)[^/]*@/i, "$1***@");
}
