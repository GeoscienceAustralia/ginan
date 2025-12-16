# app/utils/cddis_email.py
"""
Utilities for managing the EMAIL used by the CDDIS flow and for quick connectivity/auth checks.

This module is used by the UI credential flow to:
  • Read/Write the EMAIL value (env var first, then a local CDDIS.env file).
  • Derive the email/username from `.netrc/_netrc` when the user only saved Earthdata credentials.
  • Test connectivity to cddis.nasa.gov and verify Earthdata authentication via requests.

Notes:
  - This module does not present UI; it is called by UI dialogs/controllers.
  - File locations are platform-aware and compatible with Windows/macOS/Linux.
"""

from __future__ import annotations
import os
import platform
import time
from pathlib import Path
from typing import Tuple
import netrc
import requests

ENV_FILE = Path(__file__).resolve().parent / "CDDIS.env"
EMAIL_KEY = "EMAIL"

# ------------------------------
#  Select the .netrc/_netrc path (for compatibility with different implementations)
# ------------------------------
def _pick_netrc() -> Path:
    """
    Select a `.netrc`-style credential file path to use.

    Returns:
      Path: Resolved path to the preferred credential file.

    Example:
      >>> isinstance(_pick_netrc(), Path)
      True
    """
    try:
        from app.utils.cddis_credentials import netrc_path as _netrc_path  # type: ignore
    except Exception:
        _netrc_path = None
    if _netrc_path:
        try:
            return _netrc_path()
        except Exception:
            pass
    try:
        from app.utils.cddis_credentials import netrc_candidates as _netrc_candidates  # type: ignore
        cands = _netrc_candidates()
        for p in cands:
            if p.exists():
                return p
        return cands[0]
    except Exception:
        if platform.system().lower().startswith("win"):
            return Path(os.environ.get("USERPROFILE", str(Path.home()))) / ".netrc"
        return Path.home() / ".netrc"

def read_email() -> str | None:
    """
    Read the EMAIL used by CDDIS utilities.

    Returns:
      str | None: EMAIL value if found. Lookup order: env var EMAIL → CDDIS.env → None.

    Example:
      >>> os.environ.pop("EMAIL", None)
      >>> _ = ENV_FILE.write_text('EMAIL="user@example.com"\\n', encoding="utf-8")
      >>> read_email()
      'user@example.com'
    """
    v = os.environ.get(EMAIL_KEY, "").strip()
    if v:
        return v
    if ENV_FILE.exists():
        for line in ENV_FILE.read_text(encoding="utf-8").splitlines():
            s = line.strip()
            if not s or s.startswith("#"):
                continue
            k, _, val = s.partition("=")
            if k.strip() == EMAIL_KEY:
                return val.strip().strip('"').strip("'")
    return None

def write_email(email: str) -> Path:
    """
    Persist the EMAIL value to `CDDIS.env` and update the process env.

    Arguments:
      email (str): Email address to store.

    Returns:
      Path: Path to the written CDDIS.env file.

    Example:
      >>> p = write_email("user@example.com")
      >>> p.exists()
      True
      >>> read_email()
      'user@example.com'
    """
    ENV_FILE.parent.mkdir(parents=True, exist_ok=True)
    ENV_FILE.write_text(f'{EMAIL_KEY}="{email}"\n', encoding="utf-8")
    os.environ[EMAIL_KEY] = email
    return ENV_FILE

def get_username_from_netrc(prefer_host: str = "urs.earthdata.nasa.gov") -> Tuple[bool, str]:
    """
    Read the username from `.netrc/_netrc`, assuming username equals EMAIL.

    Arguments:
      prefer_host (str): Primary host to query in netrc (fallback to cddis.nasa.gov).

    Returns:
      tuple[bool, str]: (True, username) if found; otherwise (False, reason).

    Example:
      >>> ok, val = get_username_from_netrc()  # doctest: +SKIP
      >>> ok in (True, False)
      True
    """
    p = _pick_netrc()
    if not p.exists():
        return False, f"no netrc at {p}"
    try:
        n = netrc.netrc(p)
        auth = n.authenticators(prefer_host) or n.authenticators("cddis.nasa.gov")
        if not auth or not auth[0]:
            return False, f"no authenticators for {prefer_host} or cddis.nasa.gov in {p}"
        return True, auth[0]
    except Exception as e:
        return False, f"parse netrc failed: {e}"

def ensure_email_from_netrc(prefer_host: str = "urs.earthdata.nasa.gov") -> Tuple[bool, str]:
    """
    Ensure that EMAIL is available, deriving it from netrc if necessary.

    Arguments:
      prefer_host (str): Primary host to read username from in netrc.

    Returns:
      tuple[bool, str]: (True, email) if resolved; otherwise (False, reason).

    Example:
      >>> ok, email = ensure_email_from_netrc()  # doctest: +SKIP
      >>> ok in (True, False)
      True
    """
    existing = read_email()
    if existing:
        os.environ[EMAIL_KEY] = existing
        return True, existing
    ok, user = get_username_from_netrc(prefer_host=prefer_host)
    if not ok:
        return False, user
    write_email(user)
    return True, user

def get_netrc_auth() -> tuple[str, str] | None:
    """
    Retrieve (username, password) from `.netrc/_netrc` for Earthdata auth.

    Returns:
      tuple[str, str] | None: (username, password) if found; otherwise None.

    Example:
      >>> creds = get_netrc_auth()  # doctest: +SKIP
      >>> creds is None or isinstance(creds, tuple)
      True
    """
    p = _pick_netrc()
    if not p.exists():
        return None
    n = netrc.netrc(p)
    for host in ("cddis.nasa.gov", "urs.earthdata.nasa.gov"):
        auth = n.authenticators(host)
        if auth and auth[0] and auth[2]:
            return (auth[0], auth[2])
    return None

def test_cddis_connection(timeout: int = 15) -> tuple[bool, str]:
    """
    Test CDDIS connectivity and Earthdata authentication in two phases.

    Arguments:
      timeout (int): Overall timeout in seconds for the restricted request phase.

    Returns:
      tuple[bool, str]: (True, 'AUTH OK, took X.XXX seconds') on success; otherwise (False, reason).

    Example:
      >>> ok, msg = test_cddis_connection()  # doctest: +SKIP
      >>> ok in (True, False)
      True
    """
    print("Testing connectivity to cddis.nasa.gov...")
    start_time = time.perf_counter()
    r = requests.get("https://cddis.nasa.gov/robots.txt", timeout=(5, timeout))
    if r.status_code != 200:
        return False, f"HTTP {r.status_code} on robots.txt"
    print(f"Connectivity OK. Took {time.perf_counter() - start_time:.3f} seconds\nTesting authentication using .netrc...")

    start_time = time.perf_counter()
    creds = get_netrc_auth()
    if not creds:
        return False, "no usable credentials in .netrc"
    session = requests.Session()
    session.auth = creds
    url = "https://cddis.nasa.gov/archive/00readme"
    resp = session.get(url, timeout=(5, timeout), allow_redirects=True)
    head = resp.text[:1200]
    if resp.status_code == 200 and "Earthdata Login" not in head:
        return True, f"AUTH OK, took {time.perf_counter() - start_time:.3f} seconds"
    return False, f"HTTP {resp.status_code} or login page returned"

if __name__ == "__main__":
    print(test_cddis_connection())