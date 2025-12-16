# app/utils/cddis_credentials.py
from __future__ import annotations
import os, platform, stat, shutil
from pathlib import Path
import netrc

URS = "urs.earthdata.nasa.gov"
CDDIS = "cddis.nasa.gov"

def _win_user_home() -> Path:
    """
    Return the Windows user home path.

    Returns:
      Path: Path to the current user's home directory on Windows; falls back to Path.home() if env var is missing.

    Example:
      >>> isinstance(_win_user_home(), Path)
      True
    """
    return Path(os.environ.get("USERPROFILE", str(Path.home())))

def netrc_candidates() -> tuple[Path, ...]:
    """
    Return possible credential file paths on this OS.

    Returns:
      tuple[Path, ...]: Candidate paths to search/write `.netrc`-style credentials. On Windows: (%USERPROFILE%\\.netrc, %USERPROFILE%\\_netrc); on macOS/Linux: (~/.netrc,).

    Example:
      >>> tuple(map(lambda p: p.name, netrc_candidates()))  # doctest: +ELLIPSIS
      ('...netrc',) or ('...netrc', '_netrc')
    """
    if platform.system().lower().startswith("win"):
        return (_win_user_home() / ".netrc", _win_user_home() / "_netrc")
    return (Path.home() / ".netrc",)

def _write_text_secure(p: Path, content: str) -> None:
    """
    Write text to a file, applying secure permissions on non-Windows.

    Arguments:
      p (Path): Target file path.
      content (str): File content to write (UTF-8).
    """
    p.write_text(content, encoding="utf-8")
    if not platform.system().lower().startswith("win"):
        os.chmod(p, stat.S_IRUSR | stat.S_IWUSR)  # 0600

def save_earthdata_credentials(username: str, password: str) -> tuple[Path, ...]:
    """
    Save Earthdata credentials for both URS and CDDIS hosts.

    Arguments:
      username (str): Earthdata (URS/CDDIS) account username.
      password (str): Earthdata (URS/CDDIS) account password.

    Returns:
      tuple[Path, ...]: The list of credential files written. Also sets environment variable NETRC to the preferred file.

    Example:
      >>> paths = save_earthdata_credentials("user", "pass")  # doctest: +SKIP
      >>> len(paths) >= 1
      True
    """
    content = (
        f"machine {URS}   login {username} password {password}\n"
        f"machine {CDDIS} login {username} password {password}\n"
    )
    written: list[Path] = []
    for p in netrc_candidates():
        _write_text_secure(p, content)
        written.append(p)
    os.environ["NETRC"] = str(written[0])
    return tuple(written)

def _ensure_windows_mirror() -> None:
    """
    Ensure .netrc exists by mirroring _netrc on Windows if necessary.
    """
    if not platform.system().lower().startswith("win"):
        return
    dot, under = _win_user_home() / ".netrc", _win_user_home() / "_netrc"
    if under.exists() and not dot.exists():
        try:
            shutil.copyfile(under, dot)
        except Exception:
            pass

def validate_netrc(required=(URS, CDDIS)) -> tuple[bool, str]:
    """
    Validate presence and completeness of Earthdata credentials.

    Arguments:
      required (tuple[str, ...]): Hostnames that must have valid entries.

    Returns:
      tuple[bool, str]: If valid, (True, path-to-netrc). If invalid, (False, reason).

    Example:
      >>> ok, info = validate_netrc()  # doctest: +SKIP
      >>> ok in (True, False)
      True
    """
    _ensure_windows_mirror()
    candidates = netrc_candidates()
    p = next((c for c in candidates if c.exists()), candidates[0])
    if not p.exists():
        return False, f"not found: {p}"
    try:
        n = netrc.netrc(p)
        for host in required:
            auth = n.authenticators(host)
            if not auth or not auth[0] or not auth[2]:
                return False, f"missing credentials for {host} in {p}"
        os.environ["NETRC"] = str(p)
        return True, str(p)
    except Exception as e:
        return False, f"invalid netrc {p}: {e}"