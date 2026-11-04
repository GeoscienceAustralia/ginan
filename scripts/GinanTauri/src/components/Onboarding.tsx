import { useEffect, useState } from "react";
import { invoke } from "@tauri-apps/api/core";
import { useAppStore } from "../store/appStore";
import { usePickers } from "../hooks/usePickers";
import PathControl from "./PathControl";
import NtripCredentialsForm from "./NtripCredentialsForm";
import {
  EarthdataCredentialFields,
  EarthdataLinks,
  saveAndTestCddisCredentials,
} from "./EarthdataCredentials";
import {
  KeyIcon,
  PackageIcon,
  CheckIcon,
  AlertIcon,
  ShieldIcon,
  ChevronLeftIcon,
  ChevronRightIcon,
  FileIcon,
  SatelliteIcon,
} from "./icons";
import ginanLogo from "../assets/ginan-logo.png";

/**
 * First-run setup. Covers the two things the app cannot do anything useful
 * without — Earthdata credentials for the CDDIS product mirror, and a directory
 * to cache products into — and hands over to the guided tour at the end.
 *
 * It owns the credentials prompt for the duration: App's startup validation
 * deliberately holds off while `onboardingStatus` is "active", so a user
 * working through this screen never gets the standalone modal thrown over it.
 *
 * Every step past the welcome is skippable. Skipping credentials is the same
 * decision as cancelling the credentials modal — offline mode, no product
 * downloads — and the banner under the navbar keeps saying so afterwards.
 */

type StepKey = "welcome" | "credentials" | "livestream" | "products" | "done";

/**
 * Steps shown for this run. The livestream step only appears once the user
 * says they will process live streams — an NTRIP caster account is
 * irrelevant to someone working from RINEX files, and asking everyone for one
 * would make the common path longer for no reason.
 */
function stepsFor(livestream: boolean): { key: StepKey; label: string }[] {
  return [
    { key: "welcome", label: "Welcome" },
    { key: "credentials", label: "Credentials" },
    ...(livestream
      ? [{ key: "livestream" as StepKey, label: "Real-time data stream" }]
      : []),
    { key: "products", label: "Products" },
    { key: "done", label: "Finish" },
  ];
}

export default function Onboarding() {
  const onboardingStatus = useAppStore((s) => s.onboardingStatus);
  const completeOnboarding = useAppStore((s) => s.completeOnboarding);
  const startTour = useAppStore((s) => s.startTour);

  const [index, setIndex] = useState(0);
  const [wantsLivestream, setWantsLivestream] = useState(false);
  const STEPS = stepsFor(wantsLivestream);

  // This component stays mounted for the life of the app and merely renders
  // nothing once setup is done, so replaying it from Settings has to be told to
  // go back to the first step — otherwise it reopens on "Finish".
  useEffect(() => {
    if (onboardingStatus === "active") setIndex(0);
  }, [onboardingStatus]);

  if (onboardingStatus !== "active") return null;

  const step = STEPS[index];
  const back = () => setIndex((i) => Math.max(0, i - 1));
  const next = () => setIndex((i) => Math.min(STEPS.length - 1, i + 1));

  const finish = (withTour: boolean) => {
    completeOnboarding();
    if (withTour) startTour("welcome");
  };

  return (
    <div
      className="onboarding"
      // Covers the whole window, header included, so the backdrop is what
      // drags it.
      data-tauri-drag-region
      role="dialog"
      aria-modal="true"
      aria-label="Set up Ginan UI"
    >
      <div className="onboarding-card">
        <ol className="onboarding-rail" aria-label="Setup progress">
          {STEPS.map((s, i) => (
            <li
              key={s.key}
              className={
                "onboarding-rail-step" +
                (i === index ? " is-current" : "") +
                (i < index ? " is-done" : "")
              }
              aria-current={i === index ? "step" : undefined}
            >
              <span className="onboarding-rail-mark" aria-hidden="true">
                {i < index ? <CheckIcon size={11} /> : i + 1}
              </span>
              <span className="onboarding-rail-label">{s.label}</span>
            </li>
          ))}
        </ol>

        <div className="onboarding-body">
          {step.key === "welcome" && (
            <WelcomeStep
              wantsLivestream={wantsLivestream}
              onChoose={setWantsLivestream}
            />
          )}
          {step.key === "credentials" && <CredentialsStep />}
          {step.key === "livestream" && <LivestreamStep />}
          {step.key === "products" && <ProductsStep />}
          {step.key === "done" && <DoneStep />}
        </div>

        <div className="onboarding-actions">
          {index > 0 ? (
            <button className="btn-ghost" onClick={back}>
              <ChevronLeftIcon size={14} /> Back
            </button>
          ) : (
            <span />
          )}

          <div className="onboarding-actions-right">
            {step.key === "welcome" && (
              <button className="btn-primary" onClick={next}>
                Get started <ChevronRightIcon size={14} />
              </button>
            )}

            {(step.key === "credentials" ||
              step.key === "livestream" ||
              step.key === "products") && (
              <>
                <button className="btn-ghost" onClick={next}>
                  Skip for now
                </button>
                <button className="btn-primary" onClick={next}>
                  Continue <ChevronRightIcon size={14} />
                </button>
              </>
            )}

            {step.key === "done" && (
              <>
                <button className="btn-ghost" onClick={() => finish(false)}>
                  Skip the tour
                </button>
                <button className="btn-primary" onClick={() => finish(true)}>
                  Show me around <ChevronRightIcon size={14} />
                </button>
              </>
            )}
          </div>
        </div>
      </div>
    </div>
  );
}

function WelcomeStep({
  wantsLivestream,
  onChoose,
}: {
  wantsLivestream: boolean;
  onChoose: (v: boolean) => void;
}) {
  return (
    <div className="onboarding-step">
      <img className="onboarding-logo" src={ginanLogo} alt="" />
      <h2>Welcome to Ginan UI</h2>
      <p className="onboarding-lede">
        Ginan works out precise positions from GNSS data. Give it a file or a
        real-time data stream, choose a few settings, and it does the rest.
      </p>

      {/* Picking livestream adds a step for the NTRIP caster account, which is
          a different login from Earthdata and easy to be caught out by. */}
      <div className="onboarding-choice" role="group" aria-label="What will you process?">
        <button
          type="button"
          className={`onboarding-choice-card${!wantsLivestream ? " is-selected" : ""}`}
          aria-pressed={!wantsLivestream}
          onClick={() => onChoose(false)}
        >
          <FileIcon size={16} />
          <strong>Files</strong>
          <span>Data files you already have on this computer.</span>
        </button>
        <button
          type="button"
          className={`onboarding-choice-card${wantsLivestream ? " is-selected" : ""}`}
          aria-pressed={wantsLivestream}
          onClick={() => onChoose(true)}
        >
          <SatelliteIcon size={16} />
          <strong>Real-time data stream</strong>
          <span>Real-time data over the internet. Needs a stream login.</span>
        </button>
      </div>
      <ul className="onboarding-list">
        <li>
          <span className="onboarding-list-mark" aria-hidden="true">
            <KeyIcon size={14} />
          </span>
          <div>
            <strong>A NASA Earthdata login</strong>
            <p>Free to create. It's where the correction files come from.</p>
          </div>
        </li>
        <li>
          <span className="onboarding-list-mark" aria-hidden="true">
            <PackageIcon size={14} />
          </span>
          <div>
            <strong>A folder for downloads</strong>
            <p>Somewhere to keep those files so they only download once.</p>
          </div>
        </li>
        <li>
          <span className="onboarding-list-mark" aria-hidden="true">
            <CheckIcon size={14} />
          </span>
          <div>
            <strong>Then a quick look around</strong>
            <p>We'll show you where everything is.</p>
          </div>
        </li>
      </ul>
      <p className="onboarding-foot">
        You don't need to install anything else. Ginan keeps itself up to date.
      </p>
    </div>
  );
}

/**
 * Same two commands as CddisCredentialsModal — save to netrc, then prove the
 * credentials work — but inline, and it checks for credentials that already
 * exist so a reinstall over a populated netrc isn't asked to type them again.
 */
function CredentialsStep() {
  const setOfflineMode = useAppStore((s) => s.setOfflineMode);
  const bumpCddisEpoch = useAppStore((s) => s.bumpCddisEpoch);

  const [username, setUsername] = useState("");
  const [password, setPassword] = useState("");
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [connected, setConnected] = useState(false);
  const [checking, setChecking] = useState(true);

  useEffect(() => {
    let cancelled = false;
    invoke<{ ok: boolean; why: string | null }>("validate_cddis_credentials")
      .then((status) => {
        if (cancelled) return;
        setConnected(status.ok);
        setOfflineMode(!status.ok);
        setChecking(false);
      })
      .catch(() => {
        if (cancelled) return;
        // Same reasoning as the startup check in App: a check that cannot run
        // is reported as no connection rather than silently presenting as one.
        setOfflineMode(true);
        setChecking(false);
      });
    return () => {
      cancelled = true;
    };
  }, [setOfflineMode]);

  const save = async () => {
    setBusy(true);
    setError(null);
    try {
      await saveAndTestCddisCredentials(username, password);
      setOfflineMode(false);
      setConnected(true);
      // Re-runs the connectivity → static products → ANTEX chain now that there
      // are credentials to run it with.
      bumpCddisEpoch();
    } catch (e) {
      setError(typeof e === "string" ? e : String(e));
    } finally {
      setBusy(false);
    }
  };

  return (
    <div className="onboarding-step is-left">
      <span className="onboarding-badge" aria-hidden="true">
        <KeyIcon size={17} />
      </span>
      <h2>Connect to NASA Earthdata</h2>
      <p className="onboarding-lede">
        To work out a precise position, Ginan needs correction files that say
        where the satellites really were. They come from NASA's archive, and
        you need a free account to download them.
      </p>
      <p className="onboarding-lede">
        Your login is saved on this computer and only ever sent to NASA. You
        can skip this, but downloads won't work until you sign in.
      </p>

      {checking ? (
        <p className="onboarding-note">
          <span className="spinner" /> Looking for a saved login...
        </p>
      ) : connected ? (
        <p className="onboarding-note is-ok">
          <CheckIcon size={14} /> You're signed in.
        </p>
      ) : (
        <>
          <EarthdataLinks className="onboarding-links" />

          <EarthdataCredentialFields
            username={username}
            password={password}
            onUsernameChange={setUsername}
            onPasswordChange={setPassword}
            disabled={busy}
          />

          {error && (
            <p className="onboarding-note is-error">
              <AlertIcon size={14} /> {error}
            </p>
          )}

          <div className="onboarding-inline-action">
            <button
              className="btn-secondary"
              onClick={save}
              disabled={busy || !username.trim() || !password}
            >
              <ShieldIcon size={14} /> {busy ? "Verifying..." : "Save & test"}
            </button>
          </div>
        </>
      )}
    </div>
  );
}

/**
 * NTRIP caster credentials, shown only when the user picked Livestream on the
 * welcome step.
 *
 * Skippable like every other step: the caster sign-in also lives on
 * Settings › Credentials, and the real-time panel links there.
 */
function LivestreamStep() {
  return (
    <div className="onboarding-step is-left">
      <h2>Real-time data stream credentials</h2>
      <p className="onboarding-lede">
        Real-time processing pulls corrections from an NTRIP caster -
        <code> ntrip.data.gnss.ga.gov.au</code> by default. That account is
        separate from the Earthdata login on the previous step.
      </p>
      <NtripCredentialsForm compact />
      <p className="onboarding-foot">
        You can add or change these later in Settings › Credentials.
      </p>
    </div>
  );
}

function ProductsStep() {
  const productsDir = useAppStore((s) => s.productsDir);
  const { pickProductsDir } = usePickers();

  return (
    <div className="onboarding-step is-left">
      <span className="onboarding-badge" aria-hidden="true">
        <PackageIcon size={17} />
      </span>
      <h2>Choose a products folder</h2>
      <p className="onboarding-lede">
        This is where the correction files get saved. Ginan reuses them, so
        each one only downloads once. Pick an empty folder with a few GB free.
      </p>

      <PathControl
        stacked
        value={productsDir || null}
        empty="Not set"
        onBrowse={pickProductsDir}
      />

      {productsDir ? (
        <p className="onboarding-note is-ok">
          <CheckIcon size={14} /> Downloads will be saved here.
        </p>
      ) : (
        <p className="onboarding-note">
          You can pick one later in Settings, but you won't be able to process
          anything until you do.
        </p>
      )}
    </div>
  );
}

function DoneStep() {
  const productsDir = useAppStore((s) => s.productsDir);
  const offlineMode = useAppStore((s) => s.offlineMode);

  return (
    <div className="onboarding-step">
      <span className="onboarding-badge is-ok" aria-hidden="true">
        <CheckIcon size={18} />
      </span>
      <h2>You're set up</h2>
      <p className="onboarding-lede">
        That's the setup saved, and we won't ask again. Here's where you
        landed:
      </p>

      <ul className="onboarding-summary">
        <li className={offlineMode ? "is-warn" : "is-ok"}>
          {offlineMode ? <AlertIcon size={13} /> : <CheckIcon size={13} />}
          <span>
            {offlineMode
              ? "Not signed in, so downloads won't work yet"
              : "Signed in to NASA Earthdata"}
          </span>
        </li>
        <li className={productsDir ? "is-ok" : "is-warn"}>
          {productsDir ? <CheckIcon size={13} /> : <AlertIcon size={13} />}
          <span>
            {productsDir
              ? `Downloads go to ${productsDir}`
              : "No download folder yet, pick one in Settings"}
          </span>
        </li>
      </ul>

      <p className="onboarding-foot">
        You can change either in Settings. The ? button at the top brings this
        tour back whenever you want it.
      </p>
    </div>
  );
}
