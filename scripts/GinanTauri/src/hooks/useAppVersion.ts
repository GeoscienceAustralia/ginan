import { useEffect, useState } from "react";
import { getVersion } from "@tauri-apps/api/app";
// Named import so the bundler keeps only this field, not the dependency list.
import { version as packageVersion } from "../../package.json";

/**
 * The app's version as the installed build reports it.
 *
 * Tauri's `getVersion()` reads tauri.conf.json, which is what the installer was
 * stamped with, so it is preferred. The package.json version is baked in at
 * build time and stands in until that resolves, and for good outside Tauri,
 * e.g. the frontend on its own under `vite dev`.
 */
export function useAppVersion(): string {
  const [version, setVersion] = useState<string>(packageVersion);

  useEffect(() => {
    let cancelled = false;
    getVersion()
      .then((v) => { if (!cancelled) setVersion(v); })
      .catch(() => { /* not running under Tauri: keep the package version */ });
    return () => { cancelled = true; };
  }, []);

  return version;
}
