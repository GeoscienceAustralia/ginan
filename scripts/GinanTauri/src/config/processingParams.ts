/**
 * Processing parameters exposed as UI settings, declared in the order they
 * appear in the generated config.
 *
 * The backend takes these as an open `advanced` map keyed by dotted YAML path,
 * so adding a parameter here is the *only* edit required — no Rust change, no
 * store change, no new control component.
 *
 * Deliberately a curated subset. `default_config.yaml` carries ~180 further
 * hardcoded values, most of them Kalman filter internals (process noise, sigma
 * arrays, chi-square modes). Those stay in the template: they are easy to set
 * to something that silently degrades a solution rather than failing loudly.
 * Anything genuinely needed can still be hand-edited in the YAML — that is what
 * the "Overwrite on run" switch in the inspector is for.
 *
 * `defaultValue` MUST match the template. It is what the UI shows before the
 * user touches anything, and a parameter is only sent to the backend once it
 * differs, so a wrong default here would silently rewrite the config.
 */

export type ParamType = "boolean" | "number" | "enum" | "string";

export interface ParamDef {
  /** Dotted path into the config tree — the backend key. */
  path: string;
  label: string;
  desc?: string;
  type: ParamType;
  /** Must equal the value in default_config.yaml. */
  defaultValue: boolean | number | string;
  /** enum only. */
  options?: { value: string; label: string }[];
  /** number only — advisory bounds for the input. */
  min?: number;
  max?: number;
  step?: number;
  unit?: string;
}

export interface ParamGroup {
  /** Matches the top-level YAML section, so the UI order mirrors the file. */
  label: string;
  desc?: string;
  params: ParamDef[];
}

const ERROR_MODELS = [
  { value: "ELEVATION_DEPENDENT", label: "Elevation dependent" },
  { value: "UNIFORM", label: "Uniform" },
];

/** Ordered to match the generated YAML top to bottom. */
export const PARAM_GROUPS: ParamGroup[] = [
  {
    label: "Satellite options",
    desc: "Measurement error model applied to satellites.",
    params: [
      {
        path: "satellite_options.global.error_model",
        label: "Error model",
        desc: "How measurement noise scales with elevation.",
        type: "enum",
        defaultValue: "ELEVATION_DEPENDENT",
        options: ERROR_MODELS,
      },
      {
        path: "satellite_options.global.code_sigma",
        label: "Code sigma",
        desc: "Standard deviation of code measurements. 0 uses the receiver value.",
        type: "number",
        defaultValue: 0,
        min: 0,
        step: 0.1,
        unit: "m",
      },
      {
        path: "satellite_options.global.phase_sigma",
        label: "Phase sigma",
        desc: "Standard deviation of phase measurements. 0 uses the receiver value.",
        type: "number",
        defaultValue: 0,
        min: 0,
        step: 0.001,
        unit: "m",
      },
    ],
  },
  {
    label: "Receiver options",
    desc: "Observation gating and the physical models applied at the receiver.",
    params: [
      {
        path: "receiver_options.global.elevation_mask",
        label: "Elevation mask",
        desc: "Satellites below this elevation are excluded. Raising it drops noisy low-elevation observations at the cost of geometry.",
        type: "number",
        defaultValue: 15,
        min: 0,
        max: 90,
        step: 1,
        unit: "°",
      },
      {
        path: "receiver_options.global.error_model",
        label: "Error model",
        type: "enum",
        defaultValue: "ELEVATION_DEPENDENT",
        options: ERROR_MODELS,
      },
      {
        path: "receiver_options.global.code_sigma",
        label: "Code sigma",
        type: "number",
        defaultValue: 0.3,
        min: 0,
        step: 0.05,
        unit: "m",
      },
      {
        path: "receiver_options.global.phase_sigma",
        label: "Phase sigma",
        type: "number",
        defaultValue: 0.003,
        min: 0,
        step: 0.001,
        unit: "m",
      },
      {
        path: "receiver_options.global.models.troposphere.enable",
        label: "Troposphere modelling",
        desc: "Correct for signal delay through water vapour.",
        type: "boolean",
        defaultValue: true,
      },
      {
        path: "receiver_options.global.models.tides.enable",
        label: "Tidal displacements",
        desc: "Master switch for the tide models below.",
        type: "boolean",
        defaultValue: true,
      },
      {
        path: "receiver_options.global.models.tides.solid",
        label: "Solid Earth tides",
        type: "boolean",
        defaultValue: true,
      },
      {
        path: "receiver_options.global.models.tides.otl",
        label: "Ocean tide loading",
        desc: "Requires the OLOAD_GO.BLQ table in the products directory.",
        type: "boolean",
        defaultValue: true,
      },
      {
        path: "receiver_options.global.models.tides.atl",
        label: "Atmospheric tide loading",
        desc: "Requires the ALOAD_GO.BLQ table in the products directory.",
        type: "boolean",
        defaultValue: true,
      },
      {
        path: "receiver_options.global.models.tides.opole",
        label: "Ocean pole tides",
        type: "boolean",
        defaultValue: true,
      },
      {
        path: "receiver_options.global.models.tides.spole",
        label: "Solid Earth pole tides",
        type: "boolean",
        defaultValue: true,
      },
      {
        path: "receiver_options.global.models.ionospheric_components.use_2nd_order",
        label: "2nd order ionosphere",
        type: "boolean",
        defaultValue: true,
      },
      {
        path: "receiver_options.global.models.ionospheric_components.use_3rd_order",
        label: "3rd order ionosphere",
        type: "boolean",
        defaultValue: true,
      },
    ],
  },
  {
    label: "Process modes",
    desc: "Which stages of the pipeline run.",
    params: [
      {
        path: "processing_options.process_modes.preprocessor",
        label: "Preprocessor",
        desc: "Quality checks and cycle-slip detection before filtering.",
        type: "boolean",
        defaultValue: true,
      },
      {
        path: "processing_options.process_modes.spp",
        label: "SPP",
        desc: "Single point positioning, used to initialise the filter.",
        type: "boolean",
        defaultValue: true,
      },
      {
        path: "processing_options.process_modes.ppp",
        label: "PPP",
        desc: "Precise point positioning - the main solution. Turning this off leaves only an SPP result.",
        type: "boolean",
        defaultValue: true,
      },
    ],
  },
  {
    label: "Preprocessor",
    params: [
      {
        path: "processing_options.preprocessor.cycle_slips.slip_threshold",
        label: "Cycle slip threshold",
        desc: "Lower values detect more slips, at the risk of false positives.",
        type: "number",
        defaultValue: 0.05,
        min: 0,
        step: 0.01,
      },
      {
        path: "processing_options.preprocessor.preprocess_all_data",
        label: "Preprocess all data",
        type: "boolean",
        defaultValue: true,
      },
    ],
  },
  {
    label: "SPP",
    desc: "Single point positioning and its integrity checks.",
    params: [
      {
        path: "processing_options.spp.max_lsq_iterations",
        label: "Max least-squares iterations",
        type: "number",
        defaultValue: 12,
        min: 1,
        step: 1,
      },
      {
        path: "processing_options.spp.outlier_screening.raim.enable",
        label: "RAIM",
        desc: "Receiver Autonomous Integrity Monitoring - detects and excludes faulty measurements.",
        type: "boolean",
        defaultValue: true,
      },
      {
        path: "processing_options.spp.outlier_screening.max_gdop",
        label: "Max GDOP",
        desc: "Epochs with worse geometric dilution of precision are flagged.",
        type: "number",
        defaultValue: 30,
        min: 1,
        step: 1,
      },
    ],
  },
  {
    label: "PPP filter",
    desc: "Outlier screening inside the Kalman filter.",
    params: [
      {
        path: "processing_options.ppp_filter.outlier_screening.chi_square.enable",
        label: "Chi-square test",
        type: "boolean",
        defaultValue: false,
      },
      {
        path: "processing_options.ppp_filter.outlier_screening.prefit.max_iterations",
        label: "Prefit max iterations",
        desc: "Measurements excluded by prefit checks before the filter runs.",
        type: "number",
        defaultValue: 3,
        min: 0,
        step: 1,
      },
      {
        path: "processing_options.ppp_filter.outlier_screening.prefit.sigma_check",
        label: "Prefit sigma check",
        type: "boolean",
        defaultValue: true,
      },
      {
        path: "processing_options.ppp_filter.outlier_screening.postfit.max_iterations",
        label: "Postfit max iterations",
        type: "number",
        defaultValue: 10,
        min: 0,
        step: 1,
      },
      {
        path: "processing_options.ppp_filter.outlier_screening.postfit.sigma_check",
        label: "Postfit sigma check",
        type: "boolean",
        defaultValue: true,
      },
    ],
  },
  {
    label: "Error handling",
    desc: "What happens to measurements and states flagged as erroneous.",
    params: [
      {
        path: "processing_options.model_error_handling.meas_deweighting.enable",
        label: "Deweight bad measurements",
        type: "boolean",
        defaultValue: true,
      },
      {
        path: "processing_options.model_error_handling.state_deweighting.enable",
        label: "Deweight bad states",
        type: "boolean",
        defaultValue: true,
      },
      {
        path: "processing_options.model_error_handling.ambiguities.phase_reject_limit",
        label: "Phase reject limit",
        desc: "Reset an ambiguity after this many large fractional residuals.",
        type: "number",
        defaultValue: 2,
        min: 1,
        step: 1,
      },
      {
        path: "processing_options.model_error_handling.ambiguities.reset_on.lli",
        label: "Reset ambiguity on LLI",
        desc: "Loss of Lock Indicator flagged in the RINEX file.",
        type: "boolean",
        defaultValue: true,
      },
      {
        path: "processing_options.model_error_handling.ambiguities.reset_on.gf",
        label: "Reset ambiguity on GF",
        desc: "Geometry-free slip test.",
        type: "boolean",
        defaultValue: true,
      },
      {
        path: "processing_options.model_error_handling.ambiguities.reset_on.mw",
        label: "Reset ambiguity on MW",
        desc: "Melbourne-Wübbena slip test.",
        type: "boolean",
        defaultValue: true,
      },
      {
        path: "processing_options.model_error_handling.ambiguities.reset_on.scdia",
        label: "Reset ambiguity on SCDIA",
        type: "boolean",
        defaultValue: true,
      },
    ],
  },
];

/** Flat lookup for defaults and validation. */
export const PARAMS_BY_PATH: Record<string, ParamDef> = Object.fromEntries(
  PARAM_GROUPS.flatMap((g) => g.params).map((p) => [p.path, p]),
);

/** One scalar leaf found in the config template by the backend. */
export interface DiscoveredParam {
  path: string;
  value_type: "boolean" | "number" | "string";
  default_value: boolean | number | string;
}

/** "processing_options.spp.max_lsq_iterations" → "Max lsq iterations" */
function labelFromPath(path: string): string {
  const leaf = path.split(".").pop() ?? path;
  const words = leaf.replace(/_/g, " ").trim();
  return words.charAt(0).toUpperCase() + words.slice(1);
}

/** Top-level YAML section, used to group uncurated parameters. */
function sectionOf(path: string): string {
  const top = path.split(".")[0] ?? "other";
  return labelFromPath(top);
}

/**
 * Merge the curated table with whatever the backend found in the template.
 *
 * Curated definitions win: they carry real labels, help text, units and bounds.
 * Anything discovered but not curated is appended as its own group per config
 * section, labelled from its path — that is what makes a config option added by
 * a future PEA release show up here without a code change.
 *
 * A curated parameter whose path has disappeared from the template is dropped,
 * so a removed PEA option stops being offered rather than writing a key the
 * engine no longer understands.
 */
export function mergeParamGroups(discovered: DiscoveredParam[]): ParamGroup[] {
  if (discovered.length === 0) return PARAM_GROUPS;

  const known = new Set(discovered.map((d) => d.path));

  const curated = PARAM_GROUPS.map((g) => ({
    ...g,
    params: g.params.filter((p) => known.has(p.path)),
  })).filter((g) => g.params.length > 0);

  const uncurated = discovered.filter((d) => !PARAMS_BY_PATH[d.path]);
  if (uncurated.length === 0) return curated;

  // Preserve template order within each section.
  const bySection = new Map<string, ParamDef[]>();
  for (const d of uncurated) {
    const section = sectionOf(d.path);
    const def: ParamDef = {
      path: d.path,
      label: labelFromPath(d.path),
      // The full path is the only reliable description we have for something
      // nobody has written help text for yet.
      desc: d.path,
      type: d.value_type,
      defaultValue: d.default_value,
    };
    const list = bySection.get(section) ?? [];
    list.push(def);
    bySection.set(section, list);
  }

  const extraGroups: ParamGroup[] = [...bySection.entries()].map(
    ([section, params]) => ({
      label: `${section} - other parameters`,
      desc: "Detected in the PEA config template. No description available yet.",
      params,
    }),
  );

  return [...curated, ...extraGroups];
}

/**
 * Only parameters that differ from their template default are sent to the
 * backend. Keeping the map minimal means the generated YAML stays close to the
 * shipped template, so a diff shows what the user actually changed.
 *
 * `defaults` must cover auto-discovered parameters too, not just the curated
 * table — otherwise a change to a discovered parameter is silently dropped on
 * the way to the backend. Callers pass the merged set; the curated table is the
 * fallback for the brief window before discovery returns.
 */
export function changedParams(
  values: Record<string, unknown>,
  defaults: Record<string, { defaultValue: unknown }> = PARAMS_BY_PATH,
): Record<string, unknown> {
  const out: Record<string, unknown> = {};
  for (const [path, value] of Object.entries(values)) {
    const def = defaults[path];
    // Unknown path: the template no longer has it, or discovery hasn't run.
    // Send it rather than dropping a value the user explicitly set.
    if (!def) {
      out[path] = value;
      continue;
    }
    if (value !== def.defaultValue) out[path] = value;
  }
  return out;
}

/** Flat path → def lookup over merged groups, for changedParams. */
export function defsByPath(groups: ParamGroup[]): Record<string, ParamDef> {
  return Object.fromEntries(groups.flatMap((g) => g.params).map((p) => [p.path, p]));
}
