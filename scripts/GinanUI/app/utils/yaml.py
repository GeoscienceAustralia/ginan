from ruamel.yaml import YAML
from ruamel.yaml.comments import CommentedSeq, CommentedMap
from ruamel.yaml.scalarstring import PlainScalarString
from pathlib import Path
import tempfile
import os

from scripts.GinanUI.app.utils.logger import Logger

"""
YAML utilities for the Ginan-UI application.

This module provides safe wrappers around ruamel.yaml to ensure that Python
objects (e.g., pathlib.Path, lists, strings) are always serialised and
deserialised in a consistent way.

Key functions:
- load_yaml(file_path):    Load YAML into memory, converting path-like strings
                           into pathlib.Path objects where appropriate.
- write_yaml(file_path):   Write YAML safely. Falls back to normalising values
                           if ruamel.yaml raises a RepresenterError.
- update_yaml_values():    Update values in-place, preserving comments/formatting.
- normalise_yaml_value():  Normalise a single value (Path → PlainScalarString,
                           list → CommentedSeq, str → PlainScalarString).
- _normalise_inplace():    Internal helper to recursively normalise an entire
                           config tree in-place. Used as a safety net in write_yaml().

Conventions:
- Leading underscore (_) marks helpers intended for internal use only.
- Public functions (no underscore) are part of the module’s stable API and
  should be used by other parts of the application.
"""

# Configure YAML parser
yaml = YAML()
yaml.preserve_quotes = True
yaml.indent(mapping=4, sequence=4, offset=4)
yaml.width = 4096  # Avoid line wrapping
yaml.default_flow_style = False  # Use block-style lists


def _convert_paths(obj):
    """Recursively convert plain strings that look like filesystem paths into Path objects."""
    if isinstance(obj, dict):
        return {k: _convert_paths(v) for k, v in obj.items()}
    elif isinstance(obj, list):
        return [_convert_paths(v) for v in obj]
    elif isinstance(obj, (PlainScalarString, str)):
        s = str(obj)
        # heuristic: treat as path if it looks like one
        if "/" in s or s.startswith(".") or s.startswith("~") or os.path.isabs(s):
            return Path(s).expanduser()
        return s
    else:
        return obj

def load_yaml(file_path: Path) -> CommentedMap:
    """
    Load a YAML file and return its contents, preserving structure and comments.
    Paths are left as plain strings for consistency.
    """
    with file_path.open('r', encoding='utf-8') as f:
        data = yaml.load(f)
        if data is None:
            raise ValueError(f"Failed to parse or empty YAML file: {file_path}")
        return _normalise_inplace(data)  # ✅ ensure values are normalised immediately

def write_yaml(file_path: Path, config, debug: bool = False):
    """
    Write a YAML config dictionary to file with clean formatting.
    All Path objects are normalised to plain strings before dumping.
    """
    # Proactively normalise everything
    _normalise_inplace(config)

    with file_path.open('w', encoding='utf-8') as f:
        yaml.dump(config, f)

    if debug:
        with tempfile.NamedTemporaryFile(mode='w+', delete=False, suffix=".yaml") as tmp_file:
            yaml.dump(config, tmp_file)
            tmp_file.seek(0)
            Logger.console("[DEBUG] YAML OUTPUT (from temp file):\n" + tmp_file.read())

def update_yaml_values(file_path: Path, updates: list[tuple[str, str]]):
    """
    Update several YAML keys in-place without destroying comments or formatting.
    Values are passed through normalise_yaml_value() for safety.
    """
    with file_path.open('r', encoding='utf-8') as f:
        data = yaml.load(f)
        if data is None:
            raise ValueError(f"Failed to parse YAML from {file_path}")

    for key_path, new_value in updates:
        keys = key_path.split(".")
        node = data
        for k in keys[:-1]:
            if k not in node:
                raise KeyError(f"Path segment '{k}' not found in {key_path}")
            node = node[k]

        final_key = keys[-1]
        if final_key not in node:
            raise KeyError(f"Final key '{final_key}' not found in {key_path}")

        # Normalise
        node[final_key] = normalise_yaml_value(new_value)

    with file_path.open("w", encoding='utf-8') as f:
        yaml.dump(data, f)


def normalise_yaml_value(val):
    """
    Ensure values are safe for ruamel.yaml dumping:
    - Path → PlainScalarString
    - str (no newlines) → PlainScalarString
    - list → CommentedSeq with block style
    """
    if isinstance(val, Path):
        return PlainScalarString(str(val))
    elif isinstance(val, str) and "\n" not in val:
        return PlainScalarString(val)
    elif isinstance(val, list) and not isinstance(val, CommentedSeq):
        seq = CommentedSeq(val)
        seq.fa.set_block_style()
        return seq
    return val

def _normalise_inplace(obj):
    """
    Recursively normalise values in-place using normalise_yaml_value().
    Intended for internal use as a safety net in write_yaml() and load_yaml().
    """
    if isinstance(obj, dict):
        for k, v in list(obj.items()):
            obj[k] = normalise_yaml_value(v)
            _normalise_inplace(obj[k])
    elif isinstance(obj, list):
        for i, v in enumerate(list(obj)):
            obj[i] = normalise_yaml_value(v)
            _normalise_inplace(obj[i])
    return obj
