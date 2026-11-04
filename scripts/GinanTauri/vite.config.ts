import { fileURLToPath } from "node:url";
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

export default defineConfig(async ({ command, mode }) => ({
  plugins: [react()],
  clearScreen: false,
  resolve:
    mode === "profiling"
      ? {
          alias: [
            // Times every invoke(); see src/profiling/core.ts.
            {
              find: /^@tauri-apps\/api\/core$/,
              replacement: fileURLToPath(new URL("./src/profiling/core.ts", import.meta.url)),
            },
            // A production build of React drops <Profiler> timings unless it
            // is the profiling build. The dev server already has them.
            ...(command === "build"
              ? [{ find: "react-dom/client", replacement: "react-dom/profiling" }]
              : []),
          ],
        }
      : undefined,
  server: {
    port: 1420,
    strictPort: true,
    watch: {
      ignored: ["**/src-tauri/**"],
    },
  },
}));
