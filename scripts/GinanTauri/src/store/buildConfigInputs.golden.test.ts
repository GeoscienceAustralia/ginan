/**
 * Golden proof that the realtime inputs a run sends to the backend do not
 * change unnoticed. Every scenario drives the store
 * through its stream-list API (`updateRealtime`, `setStreamSerial`, ...) and
 * freezes the exact JSON of `buildConfigInputs()` plus the `serialByDevice`
 * map the run hands to `start_pea_realtime`.
 *
 * Do not edit the literals. A diff here means the Rust contract moved.
 */
import { beforeEach, describe, expect, it } from "vitest";
import { buildConfigInputs, serialByDevice, useAppStore } from "./appStore";
import { makeRinexData, resetAppStore, store } from "../test/storeHelpers";
import { DEFAULT_SERIAL_SETTINGS, type SerialSettings } from "../types";

beforeEach(() => {
  resetAppStore();
  store().setOutputDir("/out");
  store().setProductsDir("/products");
});

const s9600: SerialSettings = { ...DEFAULT_SERIAL_SETTINGS, baud: 9600, parity: "even", stop: 2 };
const s4800: SerialSettings = { ...DEFAULT_SERIAL_SETTINGS, baud: 4800, flow: "rtscts", flush: false };

/** What a run reads: the config sent to `write_config` and the serial map. */
function snapshot(): string {
  const s = useAppStore.getState();
  return JSON.stringify({
    inputs: buildConfigInputs("/out"),
    serial: serialByDevice(
      [s.realtime.obs_streams, s.streamSerial.obs],
      [s.realtime.nav_streams, s.streamSerial.corr],
    ),
  });
}

const SCENARIOS: Record<string, () => string[]> = {
  defaultRealtime: () => {
    store().setInputMode("realtime");
    return [snapshot()];
  },

  // The panel's remove / add edits: the list change then the settings change.
  mixedReorderRemove: () => {
    store().setInputMode("realtime");
    const out: string[] = [];
    store().updateRealtime({ obs_streams: ["ALIC00AUS0", "serial:///dev/ttyUSB0", "serial:///dev/ttyUSB1"] });
    store().setStreamSerial("obs", [DEFAULT_SERIAL_SETTINGS, s9600, s4800]);
    store().updateRealtime({ nav_streams: ["BCEP00BKG0", "serial:///dev/ttyACM0", "SSRA00CNE0"] });
    store().setStreamSerial("corr", [DEFAULT_SERIAL_SETTINGS, s4800, DEFAULT_SERIAL_SETTINGS]);
    out.push(snapshot());
    // reorder obs: [ttyUSB1, ALIC, ttyUSB0]
    store().updateRealtime({ obs_streams: ["serial:///dev/ttyUSB1", "ALIC00AUS0", "serial:///dev/ttyUSB0"] });
    store().setStreamSerial("obs", [s4800, DEFAULT_SERIAL_SETTINGS, s9600]);
    out.push(snapshot());
    // remove the first corr entry
    store().updateRealtime({ nav_streams: ["serial:///dev/ttyACM0", "SSRA00CNE0"] });
    store().setStreamSerial("corr", [s4800, DEFAULT_SERIAL_SETTINGS]);
    out.push(snapshot());
    // remove the middle obs entry
    store().updateRealtime({ obs_streams: ["serial:///dev/ttyUSB1", "serial:///dev/ttyUSB0"] });
    store().setStreamSerial("obs", [s4800, s9600]);
    out.push(snapshot());
    return out;
  },

  serialOnly: () => {
    store().setInputMode("realtime");
    store().updateRealtime({ obs_streams: ["serial:///dev/ttyUSB0"], nav_streams: [] });
    store().setStreamSerial("obs", [s9600]);
    return [snapshot()];
  },

  placeholders: () => {
    store().setInputMode("realtime");
    store().updateRealtime({ obs_streams: ["", "serial://"], nav_streams: [""] });
    return [snapshot()];
  },

  duplicateSerialDevice: () => {
    store().setInputMode("realtime");
    store().updateRealtime({
      obs_streams: ["serial:///dev/ttyACM0"],
      nav_streams: ["serial:///dev/ttyACM0"],
    });
    store().setStreamSerial("obs", [s9600]);
    store().setStreamSerial("corr", [s4800]);
    return [snapshot()];
  },

  customCaster: () => {
    store().setInputMode("realtime");
    store().setCasterUrl("https://caster.example/");
    const out = [snapshot()];
    store().updateRealtime({ obs_streams: ["serial:///dev/ttyUSB0"], nav_streams: [] });
    out.push(snapshot());
    store().updateRealtime({ obs_streams: ["MOUNT0"], nav_streams: ["SSR0"] });
    out.push(snapshot());
    return out;
  },

  // Realtime knobs that are not streams ride along unchanged.
  knobs: () => {
    store().setInputMode("realtime");
    store().updateRealtime({ wait_next_epoch: 4, max_rec_latency: 0.5, record_rinex: true, ssr_antenna_offset: "COM" });
    return [snapshot()];
  },

  fileMode: () => {
    const out: string[] = [];
    store().setInputMode("realtime");
    store().updateRealtime({ obs_streams: ["serial:///dev/ttyUSB0"] });
    store().setStreamSerial("obs", [s9600]);
    store().setInputMode("file");
    store().setRinexPath("/data/ALIC00AUS_R_20240020000_01D_30S_MO.rnx");
    store().setRinexData(makeRinexData());
    out.push(snapshot());
    // and back into realtime: the file is gone, the default streams are back
    store().setInputMode("realtime");
    out.push(snapshot());
    return out;
  },
};

const GOLDEN: Record<string, string[]> = {
  defaultRealtime: [
    "{\"inputs\":{\"rinex_path\":\"\",\"rinex_paths\":[],\"output_dir\":\"/out\",\"products_dir\":\"/products\",\"marker_name\":\"UNKN\",\"receiver_type\":\"\",\"antenna_type\":\"\",\"antenna_offset\":[0,0,0],\"approx_position\":null,\"mode\":\"Static\",\"constellations\":[\"GPS\",\"GAL\",\"GLO\"],\"start_epoch\":\"\",\"end_epoch\":\"\",\"epoch_interval\":30,\"output_pos\":true,\"output_gpx\":false,\"output_trace\":false,\"output_snx\":false,\"output_spp\":true,\"constellation_obs_codes\":{\"gps\":[],\"gal\":[],\"glo\":[],\"bds\":[],\"qzs\":[]},\"ppp_provider\":\"IGS\",\"ppp_series\":\"FIN\",\"ppp_project\":\"OPS\",\"advanced\":{},\"realtime\":{\"caster_url\":\"https://ntrip.data.gnss.ga.gov.au/\",\"obs_streams\":[\"ALIC00AUS0\"],\"nav_streams\":[\"BCEP00BKG0\",\"SSRA00CNE0\"],\"ssr_antenna_offset\":\"APC\",\"wait_next_epoch\":10,\"max_rec_latency\":1,\"require_obs\":false,\"record_rtcm\":true,\"record_decoded\":true,\"record_rinex\":false}},\"serial\":{}}",
  ],
  mixedReorderRemove: [
    "{\"inputs\":{\"rinex_path\":\"\",\"rinex_paths\":[],\"output_dir\":\"/out\",\"products_dir\":\"/products\",\"marker_name\":\"UNKN\",\"receiver_type\":\"\",\"antenna_type\":\"\",\"antenna_offset\":[0,0,0],\"approx_position\":null,\"mode\":\"Static\",\"constellations\":[\"GPS\",\"GAL\",\"GLO\"],\"start_epoch\":\"\",\"end_epoch\":\"\",\"epoch_interval\":30,\"output_pos\":true,\"output_gpx\":false,\"output_trace\":false,\"output_snx\":false,\"output_spp\":true,\"constellation_obs_codes\":{\"gps\":[],\"gal\":[],\"glo\":[],\"bds\":[],\"qzs\":[]},\"ppp_provider\":\"IGS\",\"ppp_series\":\"FIN\",\"ppp_project\":\"OPS\",\"advanced\":{},\"realtime\":{\"caster_url\":\"https://ntrip.data.gnss.ga.gov.au/\",\"obs_streams\":[\"ALIC00AUS0\",\"serial:///dev/ttyUSB0\",\"serial:///dev/ttyUSB1\"],\"nav_streams\":[\"BCEP00BKG0\",\"serial:///dev/ttyACM0\",\"SSRA00CNE0\"],\"ssr_antenna_offset\":\"APC\",\"wait_next_epoch\":10,\"max_rec_latency\":1,\"require_obs\":false,\"record_rtcm\":true,\"record_decoded\":true,\"record_rinex\":false}},\"serial\":{\"/dev/ttyUSB0\":{\"baud\":9600,\"data\":8,\"parity\":\"even\",\"stop\":2,\"flow\":\"none\",\"flush\":true,\"flush_duration_ms\":0},\"/dev/ttyUSB1\":{\"baud\":4800,\"data\":8,\"parity\":\"none\",\"stop\":1,\"flow\":\"rtscts\",\"flush\":false,\"flush_duration_ms\":0},\"/dev/ttyACM0\":{\"baud\":4800,\"data\":8,\"parity\":\"none\",\"stop\":1,\"flow\":\"rtscts\",\"flush\":false,\"flush_duration_ms\":0}}}",
    "{\"inputs\":{\"rinex_path\":\"\",\"rinex_paths\":[],\"output_dir\":\"/out\",\"products_dir\":\"/products\",\"marker_name\":\"UNKN\",\"receiver_type\":\"\",\"antenna_type\":\"\",\"antenna_offset\":[0,0,0],\"approx_position\":null,\"mode\":\"Static\",\"constellations\":[\"GPS\",\"GAL\",\"GLO\"],\"start_epoch\":\"\",\"end_epoch\":\"\",\"epoch_interval\":30,\"output_pos\":true,\"output_gpx\":false,\"output_trace\":false,\"output_snx\":false,\"output_spp\":true,\"constellation_obs_codes\":{\"gps\":[],\"gal\":[],\"glo\":[],\"bds\":[],\"qzs\":[]},\"ppp_provider\":\"IGS\",\"ppp_series\":\"FIN\",\"ppp_project\":\"OPS\",\"advanced\":{},\"realtime\":{\"caster_url\":\"https://ntrip.data.gnss.ga.gov.au/\",\"obs_streams\":[\"serial:///dev/ttyUSB1\",\"ALIC00AUS0\",\"serial:///dev/ttyUSB0\"],\"nav_streams\":[\"BCEP00BKG0\",\"serial:///dev/ttyACM0\",\"SSRA00CNE0\"],\"ssr_antenna_offset\":\"APC\",\"wait_next_epoch\":10,\"max_rec_latency\":1,\"require_obs\":false,\"record_rtcm\":true,\"record_decoded\":true,\"record_rinex\":false}},\"serial\":{\"/dev/ttyUSB1\":{\"baud\":4800,\"data\":8,\"parity\":\"none\",\"stop\":1,\"flow\":\"rtscts\",\"flush\":false,\"flush_duration_ms\":0},\"/dev/ttyUSB0\":{\"baud\":9600,\"data\":8,\"parity\":\"even\",\"stop\":2,\"flow\":\"none\",\"flush\":true,\"flush_duration_ms\":0},\"/dev/ttyACM0\":{\"baud\":4800,\"data\":8,\"parity\":\"none\",\"stop\":1,\"flow\":\"rtscts\",\"flush\":false,\"flush_duration_ms\":0}}}",
    "{\"inputs\":{\"rinex_path\":\"\",\"rinex_paths\":[],\"output_dir\":\"/out\",\"products_dir\":\"/products\",\"marker_name\":\"UNKN\",\"receiver_type\":\"\",\"antenna_type\":\"\",\"antenna_offset\":[0,0,0],\"approx_position\":null,\"mode\":\"Static\",\"constellations\":[\"GPS\",\"GAL\",\"GLO\"],\"start_epoch\":\"\",\"end_epoch\":\"\",\"epoch_interval\":30,\"output_pos\":true,\"output_gpx\":false,\"output_trace\":false,\"output_snx\":false,\"output_spp\":true,\"constellation_obs_codes\":{\"gps\":[],\"gal\":[],\"glo\":[],\"bds\":[],\"qzs\":[]},\"ppp_provider\":\"IGS\",\"ppp_series\":\"FIN\",\"ppp_project\":\"OPS\",\"advanced\":{},\"realtime\":{\"caster_url\":\"https://ntrip.data.gnss.ga.gov.au/\",\"obs_streams\":[\"serial:///dev/ttyUSB1\",\"ALIC00AUS0\",\"serial:///dev/ttyUSB0\"],\"nav_streams\":[\"serial:///dev/ttyACM0\",\"SSRA00CNE0\"],\"ssr_antenna_offset\":\"APC\",\"wait_next_epoch\":10,\"max_rec_latency\":1,\"require_obs\":false,\"record_rtcm\":true,\"record_decoded\":true,\"record_rinex\":false}},\"serial\":{\"/dev/ttyUSB1\":{\"baud\":4800,\"data\":8,\"parity\":\"none\",\"stop\":1,\"flow\":\"rtscts\",\"flush\":false,\"flush_duration_ms\":0},\"/dev/ttyUSB0\":{\"baud\":9600,\"data\":8,\"parity\":\"even\",\"stop\":2,\"flow\":\"none\",\"flush\":true,\"flush_duration_ms\":0},\"/dev/ttyACM0\":{\"baud\":4800,\"data\":8,\"parity\":\"none\",\"stop\":1,\"flow\":\"rtscts\",\"flush\":false,\"flush_duration_ms\":0}}}",
    "{\"inputs\":{\"rinex_path\":\"\",\"rinex_paths\":[],\"output_dir\":\"/out\",\"products_dir\":\"/products\",\"marker_name\":\"UNKN\",\"receiver_type\":\"\",\"antenna_type\":\"\",\"antenna_offset\":[0,0,0],\"approx_position\":null,\"mode\":\"Static\",\"constellations\":[\"GPS\",\"GAL\",\"GLO\"],\"start_epoch\":\"\",\"end_epoch\":\"\",\"epoch_interval\":30,\"output_pos\":true,\"output_gpx\":false,\"output_trace\":false,\"output_snx\":false,\"output_spp\":true,\"constellation_obs_codes\":{\"gps\":[],\"gal\":[],\"glo\":[],\"bds\":[],\"qzs\":[]},\"ppp_provider\":\"IGS\",\"ppp_series\":\"FIN\",\"ppp_project\":\"OPS\",\"advanced\":{},\"realtime\":{\"caster_url\":\"https://ntrip.data.gnss.ga.gov.au/\",\"obs_streams\":[\"serial:///dev/ttyUSB1\",\"serial:///dev/ttyUSB0\"],\"nav_streams\":[\"serial:///dev/ttyACM0\",\"SSRA00CNE0\"],\"ssr_antenna_offset\":\"APC\",\"wait_next_epoch\":10,\"max_rec_latency\":1,\"require_obs\":false,\"record_rtcm\":true,\"record_decoded\":true,\"record_rinex\":false}},\"serial\":{\"/dev/ttyUSB1\":{\"baud\":4800,\"data\":8,\"parity\":\"none\",\"stop\":1,\"flow\":\"rtscts\",\"flush\":false,\"flush_duration_ms\":0},\"/dev/ttyUSB0\":{\"baud\":9600,\"data\":8,\"parity\":\"even\",\"stop\":2,\"flow\":\"none\",\"flush\":true,\"flush_duration_ms\":0},\"/dev/ttyACM0\":{\"baud\":4800,\"data\":8,\"parity\":\"none\",\"stop\":1,\"flow\":\"rtscts\",\"flush\":false,\"flush_duration_ms\":0}}}",
  ],
  serialOnly: [
    "{\"inputs\":{\"rinex_path\":\"\",\"rinex_paths\":[],\"output_dir\":\"/out\",\"products_dir\":\"/products\",\"marker_name\":\"UNKN\",\"receiver_type\":\"\",\"antenna_type\":\"\",\"antenna_offset\":[0,0,0],\"approx_position\":null,\"mode\":\"Static\",\"constellations\":[\"GPS\",\"GAL\",\"GLO\"],\"start_epoch\":\"\",\"end_epoch\":\"\",\"epoch_interval\":30,\"output_pos\":true,\"output_gpx\":false,\"output_trace\":false,\"output_snx\":false,\"output_spp\":true,\"constellation_obs_codes\":{\"gps\":[],\"gal\":[],\"glo\":[],\"bds\":[],\"qzs\":[]},\"ppp_provider\":\"IGS\",\"ppp_series\":\"FIN\",\"ppp_project\":\"OPS\",\"advanced\":{},\"realtime\":{\"caster_url\":\"\",\"obs_streams\":[\"serial:///dev/ttyUSB0\"],\"nav_streams\":[],\"ssr_antenna_offset\":\"APC\",\"wait_next_epoch\":10,\"max_rec_latency\":1,\"require_obs\":false,\"record_rtcm\":true,\"record_decoded\":true,\"record_rinex\":false}},\"serial\":{\"/dev/ttyUSB0\":{\"baud\":9600,\"data\":8,\"parity\":\"even\",\"stop\":2,\"flow\":\"none\",\"flush\":true,\"flush_duration_ms\":0}}}",
  ],
  placeholders: [
    "{\"inputs\":{\"rinex_path\":\"\",\"rinex_paths\":[],\"output_dir\":\"/out\",\"products_dir\":\"/products\",\"marker_name\":\"UNKN\",\"receiver_type\":\"\",\"antenna_type\":\"\",\"antenna_offset\":[0,0,0],\"approx_position\":null,\"mode\":\"Static\",\"constellations\":[\"GPS\",\"GAL\",\"GLO\"],\"start_epoch\":\"\",\"end_epoch\":\"\",\"epoch_interval\":30,\"output_pos\":true,\"output_gpx\":false,\"output_trace\":false,\"output_snx\":false,\"output_spp\":true,\"constellation_obs_codes\":{\"gps\":[],\"gal\":[],\"glo\":[],\"bds\":[],\"qzs\":[]},\"ppp_provider\":\"IGS\",\"ppp_series\":\"FIN\",\"ppp_project\":\"OPS\",\"advanced\":{},\"realtime\":{\"caster_url\":\"https://ntrip.data.gnss.ga.gov.au/\",\"obs_streams\":[\"\",\"serial://\"],\"nav_streams\":[\"\"],\"ssr_antenna_offset\":\"APC\",\"wait_next_epoch\":10,\"max_rec_latency\":1,\"require_obs\":false,\"record_rtcm\":true,\"record_decoded\":true,\"record_rinex\":false}},\"serial\":{\"\":{\"baud\":115200,\"data\":8,\"parity\":\"none\",\"stop\":1,\"flow\":\"none\",\"flush\":true,\"flush_duration_ms\":0}}}",
  ],
  duplicateSerialDevice: [
    "{\"inputs\":{\"rinex_path\":\"\",\"rinex_paths\":[],\"output_dir\":\"/out\",\"products_dir\":\"/products\",\"marker_name\":\"UNKN\",\"receiver_type\":\"\",\"antenna_type\":\"\",\"antenna_offset\":[0,0,0],\"approx_position\":null,\"mode\":\"Static\",\"constellations\":[\"GPS\",\"GAL\",\"GLO\"],\"start_epoch\":\"\",\"end_epoch\":\"\",\"epoch_interval\":30,\"output_pos\":true,\"output_gpx\":false,\"output_trace\":false,\"output_snx\":false,\"output_spp\":true,\"constellation_obs_codes\":{\"gps\":[],\"gal\":[],\"glo\":[],\"bds\":[],\"qzs\":[]},\"ppp_provider\":\"IGS\",\"ppp_series\":\"FIN\",\"ppp_project\":\"OPS\",\"advanced\":{},\"realtime\":{\"caster_url\":\"\",\"obs_streams\":[\"serial:///dev/ttyACM0\"],\"nav_streams\":[\"serial:///dev/ttyACM0\"],\"ssr_antenna_offset\":\"APC\",\"wait_next_epoch\":10,\"max_rec_latency\":1,\"require_obs\":false,\"record_rtcm\":true,\"record_decoded\":true,\"record_rinex\":false}},\"serial\":{\"/dev/ttyACM0\":{\"baud\":9600,\"data\":8,\"parity\":\"even\",\"stop\":2,\"flow\":\"none\",\"flush\":true,\"flush_duration_ms\":0}}}",
  ],
  customCaster: [
    "{\"inputs\":{\"rinex_path\":\"\",\"rinex_paths\":[],\"output_dir\":\"/out\",\"products_dir\":\"/products\",\"marker_name\":\"UNKN\",\"receiver_type\":\"\",\"antenna_type\":\"\",\"antenna_offset\":[0,0,0],\"approx_position\":null,\"mode\":\"Static\",\"constellations\":[\"GPS\",\"GAL\",\"GLO\"],\"start_epoch\":\"\",\"end_epoch\":\"\",\"epoch_interval\":30,\"output_pos\":true,\"output_gpx\":false,\"output_trace\":false,\"output_snx\":false,\"output_spp\":true,\"constellation_obs_codes\":{\"gps\":[],\"gal\":[],\"glo\":[],\"bds\":[],\"qzs\":[]},\"ppp_provider\":\"IGS\",\"ppp_series\":\"FIN\",\"ppp_project\":\"OPS\",\"advanced\":{},\"realtime\":{\"caster_url\":\"https://caster.example/\",\"obs_streams\":[\"ALIC00AUS0\"],\"nav_streams\":[\"BCEP00BKG0\",\"SSRA00CNE0\"],\"ssr_antenna_offset\":\"APC\",\"wait_next_epoch\":10,\"max_rec_latency\":1,\"require_obs\":false,\"record_rtcm\":true,\"record_decoded\":true,\"record_rinex\":false}},\"serial\":{}}",
    "{\"inputs\":{\"rinex_path\":\"\",\"rinex_paths\":[],\"output_dir\":\"/out\",\"products_dir\":\"/products\",\"marker_name\":\"UNKN\",\"receiver_type\":\"\",\"antenna_type\":\"\",\"antenna_offset\":[0,0,0],\"approx_position\":null,\"mode\":\"Static\",\"constellations\":[\"GPS\",\"GAL\",\"GLO\"],\"start_epoch\":\"\",\"end_epoch\":\"\",\"epoch_interval\":30,\"output_pos\":true,\"output_gpx\":false,\"output_trace\":false,\"output_snx\":false,\"output_spp\":true,\"constellation_obs_codes\":{\"gps\":[],\"gal\":[],\"glo\":[],\"bds\":[],\"qzs\":[]},\"ppp_provider\":\"IGS\",\"ppp_series\":\"FIN\",\"ppp_project\":\"OPS\",\"advanced\":{},\"realtime\":{\"caster_url\":\"\",\"obs_streams\":[\"serial:///dev/ttyUSB0\"],\"nav_streams\":[],\"ssr_antenna_offset\":\"APC\",\"wait_next_epoch\":10,\"max_rec_latency\":1,\"require_obs\":false,\"record_rtcm\":true,\"record_decoded\":true,\"record_rinex\":false}},\"serial\":{\"/dev/ttyUSB0\":{\"baud\":115200,\"data\":8,\"parity\":\"none\",\"stop\":1,\"flow\":\"none\",\"flush\":true,\"flush_duration_ms\":0}}}",
    "{\"inputs\":{\"rinex_path\":\"\",\"rinex_paths\":[],\"output_dir\":\"/out\",\"products_dir\":\"/products\",\"marker_name\":\"UNKN\",\"receiver_type\":\"\",\"antenna_type\":\"\",\"antenna_offset\":[0,0,0],\"approx_position\":null,\"mode\":\"Static\",\"constellations\":[\"GPS\",\"GAL\",\"GLO\"],\"start_epoch\":\"\",\"end_epoch\":\"\",\"epoch_interval\":30,\"output_pos\":true,\"output_gpx\":false,\"output_trace\":false,\"output_snx\":false,\"output_spp\":true,\"constellation_obs_codes\":{\"gps\":[],\"gal\":[],\"glo\":[],\"bds\":[],\"qzs\":[]},\"ppp_provider\":\"IGS\",\"ppp_series\":\"FIN\",\"ppp_project\":\"OPS\",\"advanced\":{},\"realtime\":{\"caster_url\":\"https://caster.example/\",\"obs_streams\":[\"MOUNT0\"],\"nav_streams\":[\"SSR0\"],\"ssr_antenna_offset\":\"APC\",\"wait_next_epoch\":10,\"max_rec_latency\":1,\"require_obs\":false,\"record_rtcm\":true,\"record_decoded\":true,\"record_rinex\":false}},\"serial\":{}}",
  ],
  knobs: [
    "{\"inputs\":{\"rinex_path\":\"\",\"rinex_paths\":[],\"output_dir\":\"/out\",\"products_dir\":\"/products\",\"marker_name\":\"UNKN\",\"receiver_type\":\"\",\"antenna_type\":\"\",\"antenna_offset\":[0,0,0],\"approx_position\":null,\"mode\":\"Static\",\"constellations\":[\"GPS\",\"GAL\",\"GLO\"],\"start_epoch\":\"\",\"end_epoch\":\"\",\"epoch_interval\":30,\"output_pos\":true,\"output_gpx\":false,\"output_trace\":false,\"output_snx\":false,\"output_spp\":true,\"constellation_obs_codes\":{\"gps\":[],\"gal\":[],\"glo\":[],\"bds\":[],\"qzs\":[]},\"ppp_provider\":\"IGS\",\"ppp_series\":\"FIN\",\"ppp_project\":\"OPS\",\"advanced\":{},\"realtime\":{\"caster_url\":\"https://ntrip.data.gnss.ga.gov.au/\",\"obs_streams\":[\"ALIC00AUS0\"],\"nav_streams\":[\"BCEP00BKG0\",\"SSRA00CNE0\"],\"ssr_antenna_offset\":\"COM\",\"wait_next_epoch\":4,\"max_rec_latency\":0.5,\"require_obs\":false,\"record_rtcm\":true,\"record_decoded\":true,\"record_rinex\":true}},\"serial\":{}}",
  ],
  fileMode: [
    "{\"inputs\":{\"rinex_path\":\"/data/ALIC00AUS_R_20240020000_01D_30S_MO.rnx\",\"rinex_paths\":[\"/data/ALIC00AUS_R_20240020000_01D_30S_MO.rnx\"],\"output_dir\":\"/out\",\"products_dir\":\"/products\",\"marker_name\":\"ALIC\",\"receiver_type\":\"SEPT POLARX5 \",\"antenna_type\":\"LEIAR25.R3      NONE\",\"antenna_offset\":[0.0015,0,0],\"approx_position\":[-4052052.7,4212836,-2545105],\"mode\":\"Static\",\"constellations\":[\"GPS\",\"GAL\"],\"start_epoch\":\"2024-01-02 00:00:00\",\"end_epoch\":\"2024-01-02 23:59:30\",\"epoch_interval\":30,\"output_pos\":true,\"output_gpx\":false,\"output_trace\":false,\"output_snx\":false,\"output_spp\":true,\"constellation_obs_codes\":{\"gps\":[\"L1C\",\"L2W\",\"L5Q\"],\"gal\":[\"L1X\",\"L5X\"],\"glo\":[],\"bds\":[],\"qzs\":[]},\"ppp_provider\":\"IGS\",\"ppp_series\":\"FIN\",\"ppp_project\":\"OPS\",\"advanced\":{}},\"serial\":{}}",
    "{\"inputs\":{\"rinex_path\":\"\",\"rinex_paths\":[],\"output_dir\":\"/out\",\"products_dir\":\"/products\",\"marker_name\":\"ALIC\",\"receiver_type\":\"SEPT POLARX5 \",\"antenna_type\":\"LEIAR25.R3      NONE\",\"antenna_offset\":[0.0015,0,0],\"approx_position\":null,\"mode\":\"Static\",\"constellations\":[\"GPS\",\"GAL\"],\"start_epoch\":\"2024-01-02 00:00:00\",\"end_epoch\":\"2024-01-02 23:59:30\",\"epoch_interval\":30,\"output_pos\":true,\"output_gpx\":false,\"output_trace\":false,\"output_snx\":false,\"output_spp\":true,\"constellation_obs_codes\":{\"gps\":[\"L1C\",\"L2W\",\"L5Q\"],\"gal\":[\"L1X\",\"L5X\"],\"glo\":[],\"bds\":[],\"qzs\":[]},\"ppp_provider\":\"IGS\",\"ppp_series\":\"FIN\",\"ppp_project\":\"OPS\",\"advanced\":{},\"realtime\":{\"caster_url\":\"https://ntrip.data.gnss.ga.gov.au/\",\"obs_streams\":[\"ALIC00AUS0\"],\"nav_streams\":[\"BCEP00BKG0\",\"SSRA00CNE0\"],\"ssr_antenna_offset\":\"APC\",\"wait_next_epoch\":10,\"max_rec_latency\":1,\"require_obs\":false,\"record_rtcm\":true,\"record_decoded\":true,\"record_rinex\":false}},\"serial\":{}}",
  ],
};

describe("buildConfigInputs / serialByDevice golden output", () => {
  for (const [name, run] of Object.entries(SCENARIOS)) {
    it(name, () => {
      const got = run();
      expect(got).toEqual(GOLDEN[name]);
    });
  }
});
