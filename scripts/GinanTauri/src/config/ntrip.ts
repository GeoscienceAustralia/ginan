/**
 * Pure helpers for the NTRIP caster form.
 *
 * Shared by the streaming panel (which needs `extractHost` for its "caster set
 * in Settings" caption) and `NtripCasterSettings`, so neither component imports
 * the other.
 */

/** One row of a caster's sourcetable, as returned by `fetch_ntrip_sourcetable`. */
export interface NtripStream {
  mountpoint: string;
  identifier: string;
  format: string;
  format_details: string;
  carrier: string;
  nav_system: string;
  network: string;
  country: string;
  category: "obs" | "ssr" | "other";
}

/**
 * Pull the hostname out of a caster URL so the credential save/lookup keys by
 * exactly the string the backend will. Returns null when the URL isn't a caster
 * (e.g. `serial:///dev/…`) or is still being typed.
 */
export function extractHost(url: string): string | null {
  const match = url.match(/^[a-z]+:\/\/(?:[^@/]+@)?([^/:]+)/i);
  return match?.[1] ?? null;
}

/**
 * Return a warning when the caster URL carries something that belongs somewhere
 * else, or null when it is fine or still empty. Feedback after the fact, never a
 * caption warning in advance — a correct URL produces no text at all.
 */
export function casterUrlIssue(url: string): string | null {
  if (!url) return null;

  if (url.includes("<USER>") || url.includes("<PASS>")) {
    return "Placeholders are no longer needed - delete <USER>:<PASS>@ from the URL and use Sign in instead.";
  }

  if (/^[a-z]+:\/\/[^@/]+@/i.test(url)) {
    return "Remove the username and password from the URL - use Sign in instead, so they are never written into the generated config.";
  }

  try {
    const u = new URL(url);
    if (u.pathname.length > 1) {
      return `"${u.pathname.replace(/^\//, "")}" looks like a mountpoint. Put it in one of the stream fields, not here; the caster URL is the server address only.`;
    }
  } catch {
    // A half-typed or non-HTTP entry is not an error while the user is still typing.
  }
  return null;
}

export interface ParsedNtripUrl {
  /** scheme + host + optional port + "/", never any credentials */
  casterRoot: string;
  host: string;
  user: string;
  pass: string;
  mount: string; // empty string when none
}

/**
 * Parse a complete NTRIP URL into its parts so they can be distributed across
 * the correct fields. Returns null on parse failure.
 *
 * Accepts e.g.:
 *   https://myuser:mypassword@ntrip.data.gnss.ga.gov.au/ALIC00AUS0
 *   http://user:pass@host:2101/MOUNT
 *   https://host/   (no credentials, no mount)
 *
 * `user`/`pass` are taken exactly as written, without percent-decoding: PEA
 * Base64s them verbatim for the Authorization header (see the note on
 * `inject_ntrip_credentials`), so decoding here would change the password.
 */
export function parseNtripUrl(raw: string): ParsedNtripUrl | null {
  // A URL pasted from a config written by an earlier version of the app may
  // still carry <USER>/<PASS>, which are not legal URL characters. Swap them for something
  // parseable, then discard them — they are not credentials worth saving.
  const hadPlaceholders = raw.includes("<USER>") || raw.includes("<PASS>");
  const working = raw.replace(/<USER>/g, "u").replace(/<PASS>/g, "p");
  let u: URL;
  try {
    u = new URL(working);
  } catch {
    return null;
  }
  if (!u.hostname) return null;

  const user = hadPlaceholders ? "" : u.username || "";
  const pass = hadPlaceholders ? "" : u.password || "";
  const mount = u.pathname.replace(/^\//, "");

  const portPart = u.port ? `:${u.port}` : "";
  const casterRoot = `${u.protocol}//${u.hostname}${portPart}/`;

  return { casterRoot, host: u.hostname, user, pass, mount };
}

/**
 * A mountpoint is a correction stream if its code looks like a broadcast-eph or
 * SSR product. Used to route a mountpoint pulled out of a pasted full URL into
 * the right list.
 */
export function looksLikeCorrectionMount(mount: string): boolean {
  const m = mount.toLowerCase();
  return (
    m.includes("bkg") || m.includes("bcep") || m.includes("ssra") ||
    m.includes("cne") || m.includes("ssr") || m.includes("nav")
  );
}
