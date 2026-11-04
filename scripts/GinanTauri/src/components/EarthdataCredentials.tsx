import { invoke } from "@tauri-apps/api/core";

const EARTHDATA_LOGIN = "https://urs.earthdata.nasa.gov/";
const EARTHDATA_SIGNUP = "https://urs.earthdata.nasa.gov/users/new";

/**
 * Pieces shared by the two places that ask for NASA Earthdata credentials:
 * the standalone `CddisCredentialsModal` and the onboarding credentials step.
 */

/** Save the credentials to netrc, then prove they work. Rejects with the backend's message. */
export async function saveAndTestCddisCredentials(username: string, password: string): Promise<void> {
  await invoke("save_cddis_credentials", { username, password });
  await invoke("test_cddis_connection");
}

/**
 * The login and sign-up links. The URLs are spelled out rather than hidden
 * behind link text — the webview has no browser plugin, so a user may need to
 * copy them.
 */
export function EarthdataLinks({ className }: { className: string }) {
  return (
    <p className={className}>
      No account yet? Make one at{" "}
      <a href={EARTHDATA_SIGNUP} target="_blank" rel="noreferrer">
        urs.earthdata.nasa.gov/users/new
      </a>
      . If you already have one, sign in at{" "}
      <a href={EARTHDATA_LOGIN} target="_blank" rel="noreferrer">
        urs.earthdata.nasa.gov
      </a>
      .
    </p>
  );
}

interface CredentialFieldsProps {
  username: string;
  password: string;
  onUsernameChange: (value: string) => void;
  onPasswordChange: (value: string) => void;
  disabled: boolean;
}

/** The username and password rows. */
export function EarthdataCredentialFields({
  username,
  password,
  onUsernameChange,
  onPasswordChange,
  disabled,
}: CredentialFieldsProps) {
  return (
    <>
      <div className="field-row">
        <label>Username</label>
        <input
          type="text"
          autoComplete="username"
          value={username}
          onChange={(e) => onUsernameChange(e.target.value)}
          disabled={disabled}
        />
      </div>
      <div className="field-row">
        <label>Password</label>
        <input
          type="password"
          autoComplete="current-password"
          value={password}
          onChange={(e) => onPasswordChange(e.target.value)}
          disabled={disabled}
        />
      </div>
    </>
  );
}
