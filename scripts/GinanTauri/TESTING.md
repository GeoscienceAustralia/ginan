# Testing the Ginan UI

The frontend is covered by [Vitest](https://vitest.dev) running against
[jsdom](https://github.com/jsdom/jsdom), with
[Testing Library](https://testing-library.com/docs/react-testing-library/intro/)
for the component tests. Every pull request that touches this app runs a
typecheck, the whole suite with coverage, and the production build — see `.github/workflows/frontend-tests.yml`. The same workflow also
runs `cargo check` and `cargo test` over `src-tauri`.

## Running

Use npm — `package-lock.json` is the lockfile CI installs from.

```bash
npm ci
npm test               # the whole suite, once
npm run test:watch     # re-runs the affected files as you edit
npm run test:coverage  # writes coverage/ (git-ignored); open coverage/index.html
npm run typecheck      # tsc over src, test files included
```

A single file or a single case:

```bash
npm test -- src/store/appStore.test.ts
npm test -- -t "clamps the sidebar width"
```

## Layout

Tests live beside the code they cover, named `<subject>.test.ts(x)`. A file
that covers one exported function rather than a whole module is named after the
function (`workflowLine.test.ts`, `loadRinexFile.test.tsx`).

`src/test/` holds the harness:

| File | What it is |
| --- | --- |
| `setupTests.ts` | Browser polyfills (`matchMedia`, `ResizeObserver`, `requestAnimationFrame`, a no-op canvas 2D context, pointer capture, `scrollIntoView`, clipboard) and the `vi.mock` calls for every Tauri module. Runs before each test file. |
| `tauriMocks.ts` | The in-memory backend: `invoke`, events, dialogs, the drag-drop channel, the window title and the persistent store, plus the helpers to steer and inspect them. |
| `storeHelpers.ts` | `resetAppStore()`, the `store()` accessor, and fixture builders (`makeRinexData`, `makeObsCodes`, `makeCatalogue`, `makeStreamTestResult`, …). |

## Conventions

**The backend is never real.** Every Tauri module is mocked globally in
`setupTests.ts`, and an `invoke` for a command a test has not declared rejects
rather than returning undefined — so a test stays honest about which backend
calls its subject actually makes. Declare them with `mockInvoke`:

```ts
mockInvoke({
  write_config: "/out/ppp_config.yaml",           // a fixed result
  parse_rinex: (args) => fixtureFor(args.path),   // or a function of the args
  start_pea: () => Promise.reject(new Error("no binary")),
});
```

Then assert on what was sent with `lastInvokeArgs("write_config")` and
`invokeCount("start_pea")`.

**Reset the store.** `useAppStore` is a module singleton, so any suite that
touches it needs `beforeEach(resetAppStore)`. State written through
`localStorage` and the mocked settings file is cleared for you.

**Persisted writes are fire-and-forget.** `persist()` is not awaited by the
store, so a test asserting on `readStore(key)` has to let the microtask queue
drain first (`await new Promise((r) => setTimeout(r, 0))`).

**Buffered events need a frame.** `useTauriEvents` and `useNtripTelemetry`
batch through `requestAnimationFrame`, which the setup file shims to
`setTimeout(0)`. Emit, then `await act(flushFrame)`.

```ts
act(() => emit("pea-line", "epoch 1 solved"));
await act(flushFrame);
expect(store().consoleLines).toEqual(["epoch 1 solved"]);
```

**Query the way a user would** — role, label, visible text — and fall back to a
class or attribute only where the markup gives nothing better. Several fields
in the app carry a `<label>` with no `htmlFor`, so they have no accessible name;
those tests say so in a comment rather than silently reaching for a CSS
selector.

## What is not covered

jsdom has no canvas backend and does no layout. The setup file gives canvases
a no-op 2D context, so the figures (`PosTimeSeriesChart`, `PosScatterChart`,
`PosMapChart` with Leaflet, the `viz/` charts) are tested for mounting,
interaction and their accessible descriptions, and the scales, ticks,
formatting, hit-testing and drawing helpers they are built from are tested
directly (`chartUtils*.test.ts(x)`). What actually lands on the canvas would
need a browser runner to check. Tests that need a size stub
`getBoundingClientRect` locally.

The Rust backend in `src-tauri` has its own `cargo test` suite, run by the
same workflow.
