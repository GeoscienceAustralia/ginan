import type { MessageGroup } from "../types/streams";

export interface RtcmMessageInfo {
  name: string;
  description: string;
  group: MessageGroup;
  /** "GPS" | "GAL" | "GLO" | "BDS" | "QZS" | null */
  constellation: string | null;
}

export const RTCM_MESSAGES: Record<string, RtcmMessageInfo> = {};

const add = (no: number, name: string, description: string, group: MessageGroup, constellation: string | null) => {
  RTCM_MESSAGES[String(no)] = { name, description, group, constellation };
};

// MSM: base number per constellation, +4..+7 = MSM4..MSM7.
const MSM_BASES: Array<[string, string, number]> = [
  ["GPS", "GPS", 1070],
  ["GLO", "GLONASS", 1080],
  ["GAL", "Galileo", 1090],
  ["QZS", "QZSS", 1110],
  ["BDS", "BeiDou", 1120],
];
const MSM_DETAIL: Record<number, string> = {
  4: "pseudorange and phaseRange with CNR",
  5: "pseudorange, phaseRange, Doppler and CNR",
  6: "high-resolution pseudorange and phaseRange with CNR",
  7: "high-resolution pseudorange, phaseRange, Doppler and CNR",
};
for (const [code, full, base] of MSM_BASES) {
  for (const level of [4, 5, 6, 7]) {
    add(base + level, `MSM${level} ${code}`, `${full} Multiple Signal Message ${level}: ${MSM_DETAIL[level]}.`, "msm", code);
  }
}

add(1019, "GPS Ephemeris", "GPS broadcast ephemeris (LNAV).", "ephemeris", "GPS");
add(1020, "GLONASS Ephemeris", "GLONASS broadcast ephemeris.", "ephemeris", "GLO");
add(1042, "BeiDou Ephemeris", "BeiDou broadcast ephemeris.", "ephemeris", "BDS");
add(1044, "QZSS Ephemeris", "QZSS broadcast ephemeris.", "ephemeris", "QZS");
add(1045, "Galileo F/NAV Ephemeris", "Galileo F/NAV broadcast ephemeris.", "ephemeris", "GAL");
add(1046, "Galileo I/NAV Ephemeris", "Galileo I/NAV broadcast ephemeris.", "ephemeris", "GAL");

add(1005, "Station ARP", "Stationary antenna reference point (ECEF position).", "station", null);
add(1006, "Station ARP + Height", "Stationary antenna reference point with antenna height.", "station", null);
add(1007, "Antenna Descriptor", "Antenna descriptor and setup ID.", "station", null);
add(1008, "Antenna Descriptor + Serial", "Antenna descriptor, setup ID and serial number.", "station", null);
add(1033, "Receiver + Antenna Descriptors", "Receiver type, firmware and serial plus antenna descriptor.", "station", null);
add(1230, "GLONASS Code-Phase Biases", "GLONASS L1/L2 code-phase biases.", "bias", "GLO");

// SSR: six messages per constellation (orbit, clock, combined, code bias, URA, high-rate clock).
const SSR_KINDS: Array<[string, string]> = [
  ["Orbit Corrections", "orbit corrections"],
  ["Clock Corrections", "clock corrections"],
  ["Orbit + Clock Corrections", "combined orbit and clock corrections"],
  ["Code Biases", "code biases"],
  ["URA", "user range accuracy"],
  ["High-Rate Clock", "high-rate clock corrections"],
];
const SSR_BASES: Array<[string, string, number]> = [
  ["GPS", "GPS", 1057],
  ["GLO", "GLONASS", 1063],
  ["GAL", "Galileo", 1240],
  ["QZS", "QZSS", 1246],
  ["SBS", "SBAS", 1252],
  ["BDS", "BeiDou", 1258],
];
for (const [code, full, base] of SSR_BASES) {
  SSR_KINDS.forEach(([label, desc], i) => {
    add(base + i, `${code} SSR ${label}`, `${full} SSR ${desc}.`, "ssr", code === "SBS" ? null : code);
  });
}
add(1264, "SSR VTEC", "SSR vertical total electron content.", "ssr", null);
add(4076, "IGS SSR", "IGS State Space Representation (proposal) container.", "ssr", null);

/** MSM1-3 and other numbers not tabled above still get a useful label. */
export function describeMessage(msgNo: string | number): RtcmMessageInfo {
  const key = String(msgNo).trim();
  const hit = RTCM_MESSAGES[key];
  if (hit) return hit;
  const n = Number(key);
  for (const [code, full, base] of MSM_BASES) {
    if (n > base && n <= base + 7) {
      return {
        name: `MSM${n - base} ${code}`,
        description: `${full} Multiple Signal Message ${n - base}.`,
        group: "msm",
        constellation: code,
      };
    }
  }
  if (n >= 1240 && n <= 1270) return { name: `SSR ${key}`, description: "State Space Representation correction.", group: "ssr", constellation: null };
  return {
    name: key ? `Message ${key}` : "Unknown",
    description: "RTCM message type not in the built-in table.",
    group: "other",
    constellation: null,
  };
}
