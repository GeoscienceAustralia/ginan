import { defineConfig } from "vitest/config";
import react from "@vitejs/plugin-react";

/**
 * Test configuration, kept out of `vite.config.ts` so the app build stays a
 * plain Vite config with no test-only fields in it.
 *
 * jsdom rather than happy-dom: the charts reach for canvas, `ResizeObserver`
 * and `matchMedia`, and jsdom's stubs are the ones the setup file patches.
 *
 * Node 25 and later define their own `localStorage` global, which is
 * `undefined` unless Node is started with `--localstorage-file`, and it hides
 * jsdom's. The test workers turn Node's Web Storage off so the code under test
 * gets jsdom's `localStorage`. Older Node versions without the flag skip it.
 */
const nodeWebStorageOff = process.allowedNodeEnvironmentFlags.has("--experimental-webstorage")
  ? ["--no-experimental-webstorage"]
  : [];

export default defineConfig({
  plugins: [react()],
  test: {
    environment: "jsdom",
    poolOptions: {
      forks: { execArgv: nodeWebStorageOff },
      threads: { execArgv: nodeWebStorageOff },
    },
    globals: true,
    setupFiles: ["./src/test/setupTests.ts"],
    css: false,
    include: ["src/**/*.{test,spec}.{ts,tsx}"],
    restoreMocks: true,
    coverage: {
      provider: "v8",
      reporter: ["text", "html", "json-summary", "lcov"],
      reportsDirectory: "coverage",
      include: ["src/**/*.{ts,tsx}"],
      exclude: [
        "src/main.tsx",
        "src/types/**",
        "src/**/*.d.ts",
        "src/test/**",
        "src/**/*.{test,spec}.{ts,tsx}",
      ],
    },
  },
});
