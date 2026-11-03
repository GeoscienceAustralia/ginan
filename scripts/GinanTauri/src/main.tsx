import React, { Profiler } from "react";
import ReactDOM from "react-dom/client";
import App from "./App";
import { applyTitlebarStyle } from "./hooks/useWindowChrome";
import { onRender } from "./profiling/onRender";
import "./App.css";

// `--mode profiling` only (npm run profile). In every other build this
// condition is a constant false and the HUD is left out of the bundle.
if (import.meta.env.MODE === "profiling") void import("./profiling/hud");

// Before the first paint, so the header lays out clear of the window controls
// from the start rather than jumping once they appear.
applyTitlebarStyle();

ReactDOM.createRoot(document.getElementById("root") as HTMLElement).render(
  <React.StrictMode>
    <Profiler id="app" onRender={onRender}>
      <App />
    </Profiler>
  </React.StrictMode>
);
