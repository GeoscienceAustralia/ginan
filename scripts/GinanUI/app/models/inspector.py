"""
GinanYAMLInspector model for Ginan-UI.

Provides the data and I/O layer that creates the GinanYAMLInspector integration:
It ensures that the inspector's HTML asset exists (auto-generating it via "pea -Y 4"
when missing), builds the JavaScript that auto-imports the current config and
intercepts the inspector's "Save file" button via QWebChannel, sanitising the
YAML text emitted by the inspector, deep-merging it onto the existing
ppp_generated.yaml so keys the inspector does not know about are preserved, and
writing the result back to disk with a clean-write fallback when ruamel.yaml's
comment-preserving output cannot be re-parsed.

The owning controller is responsible for all UI presentation, including
showing the inspector dialog and surfacing any errors raised here.
"""

import re
import subprocess
from pathlib import Path
from ruamel.yaml import YAML as RuamelYAML
from scripts.GinanUI.app.utils.common_dirs import GENERATED_YAML, INSPECTOR_HTML_PATH
from scripts.GinanUI.app.utils.logger import Logger
from scripts.GinanUI.app.utils.yaml import load_yaml, write_yaml


class Inspector:
    """
    Model for the GinanYAMLInspector integration.

    Exposes a small, controller-facing API: ensure the HTML asset exists,
    read the current config text, build the JS that wires auto-import and the save intercept,
    and merge / save the inspector output

    Arguments:
      executable: Path-like to the PEA executable, used only when the inspector
                  HTML must be auto-generated via "pea -Y 4". Optional - if not
                  provided, ensure_inspector_html() will fail gracefully when
                  the HTML is missing.
    """

    def __init__(self, executable=None):
        self.executable = executable

    #region Inspector HTML Generation

    def ensure_inspector_html(self) -> bool:
        """
        Ensure the GinanYAMLInspector HTML file exists at INSPECTOR_HTML_PATH

        Attempts to generate it. If pea is not available or
        the generation fails, logs a warning but does not raise - the caller must
        check the return value and inform the user.

        Returns:
          bool: True if the inspector HTML file exists (or was just generated).
        """
        if INSPECTOR_HTML_PATH.exists():
            return True

        Logger.workflow("🔧 GinanYAMLInspector not found - Attempting to generate...")

        if self.executable is None:
            Logger.workflow("⚠️ PEA executable not provided - cannot auto-generate GinanYAMLInspector HTML.")
            return False

        try:
            INSPECTOR_HTML_PATH.parent.mkdir(parents=True, exist_ok=True)

            # Run pea -Y <level> which writes GinanYAMLInspector.html to the working directory
            result = subprocess.run(
                [str(self.executable), "-Y", "4"],
                capture_output=True,
                text=True,
                cwd=str(INSPECTOR_HTML_PATH.parent),
                timeout=30,
            )

            if result.returncode != 0:
                Logger.workflow(f"⚠️ 'pea -Y 4' exited with code {result.returncode}: {result.stderr.strip()}")

            # pea writes GinanYAMLInspector.html into its working directory
            generated = INSPECTOR_HTML_PATH.parent / "GinanYamlInspector.html"
            if generated.exists():
                if generated != INSPECTOR_HTML_PATH:
                    generated.rename(INSPECTOR_HTML_PATH)
                Logger.workflow(f"✅ GinanYAMLInspector HTML generated: {INSPECTOR_HTML_PATH}")
                return True

            Logger.workflow("⚠️ 'pea -Y 4' ran but GinanYAMLInspector.html was not found in the output directory")
            return False

        except FileNotFoundError:
            Logger.workflow("⚠️ PEA executable not found - cannot auto-generate GinanYAMLInspector HTML")
            return False
        except subprocess.TimeoutExpired:
            Logger.workflow("⚠️ 'pea -Y 4' timed out after 30 seconds - cannot auto-generate GinanYAMLInspector")
            return False
        except Exception as e:
            Logger.workflow(f"⚠️ Failed to generate GinanYAMLInspector HTML: {e}")
            return False

    #endregion

    #region Import Config to Inspector

    @staticmethod
    def read_current_config() -> str:
        """
        Read the current ppp_generated.yaml content for inspector auto-import

        Returns the raw YAML text, or an empty string if the file cannot be read
        Failures are logged but never raised - the caller can still open the
        inspector with an empty pre-load if reading the config fails

        Returns:
          str: Raw YAML text from ppp_generated.yaml, or "" on any error
        """
        try:
            return GENERATED_YAML.read_text(encoding="utf-8")
        except Exception as e:
            Logger.workflow(f"⚠️ Could not read config for GinanYAMLInspector auto-import: {e}")
            return ""

    #endregion

    #region JS Builder

    @staticmethod
    def build_ginan_ui_js(yaml_content: str) -> str:
        """
        Build the JavaScript that is injected into the GinanYAMLInspector page when it
        is opened from Ginan-UI

        Two things are wired up:
          1. Auto-import - the current ppp_generated.yaml content is passed in as a JS
             string and fed directly to the inspector's file-input change handler so the
             inspector pre-populates its fields without requiring user interaction
          2. Save intercept - the "Save file" (#create) button's default download action
             is replaced with a call to bridge.saveYaml() over QWebChannel, routing the
             generated YAML text back to Python for merging and writing to disk

        The YAML content is embedded directly as a string literal rather than fetched
        via file:// URL - this sidesteps Qt WebEngine's local-content security policy
        which blocks cross-origin file:// fetches in practice

        Arguments:
          yaml_content (str): The raw text of ppp_generated.yaml to pre-load

        Returns:
          str: JavaScript source ready to pass to QWebEnginePage.runJavaScript()
        """
        # Escape the YAML text so it is safe to embed inside a JS template literal
        # Backticks, backslashes and ${...} are the only characters that need escaping
        # inside a JS template literal
        escaped_yaml = (
            yaml_content
            .replace("\\", "\\\\")
            .replace("`", "\\`")
            .replace("${", "\\${")
        )

        js = f"""
            (function() {{

            // 1. Auto-import ppp_generated.yaml
            var yamlText = `{escaped_yaml}`;
        
            function doImport() {{
                var input = document.getElementById("inputfile");
                if (!input) {{
                    console.warn("GinanYAMLInspector (Ginan-UI): #inputfile not found, retrying...");
                    setTimeout(doImport, 200);
                    return;
                }}
                var file = new File([yamlText], "ppp_generated.yaml", {{ type: "text/plain" }});
                var dt   = new DataTransfer();
                dt.items.add(file);
                input.files = dt.files;
                input.dispatchEvent(new Event("change"));
                console.log("GinanYAMLInspector (Ginan-UI): auto-imported ppp_generated.yaml");
            }}
            doImport();
        
            // 2. Intercept "Save file" → QWebChannel bridge
            // qt.webChannelTransport is injected by Qt before the page loads (via
            // QWebEnginePage.scripts()), so it is guaranteed to exist here
            new QWebChannel(qt.webChannelTransport, function(channel) {{
                var bridge = channel.objects.bridge;
                if (!bridge) {{
                    console.warn("GinanYAMLInspector (Ginan-UI): bridge not found in QWebChannel");
                    return;
                }}
        
                var createBtn = document.getElementById("create");
                if (!createBtn) {{
                    console.warn("GinanYAMLInspector (Ginan-UI): #create button not found");
                    return;
                }}
        
                // Use capture phase so this listener fires before the original download handler
                createBtn.addEventListener("click", function(evt) {{
                    evt.stopImmediatePropagation();
                    evt.preventDefault();
                    var textbox = document.getElementById("textbox");
                    var yaml    = textbox ? textbox.value : "";
                    if (!yaml || yaml === "generated yaml file") {{
                        alert('Please click "Generate yaml" first to produce the YAML content before saving.');
                        return;
                    }}
                    bridge.saveYaml(yaml);
                }}, true);
        
                console.log("GinanYAMLInspector (Ginan-UI): Save file button intercepted via QWebChannel");
            }});
        
        }})();
        """
        return js

    #endregion

    #region YAML Transformations

    @staticmethod
    def deep_merge(base: dict, override: dict) -> dict:
        """
        Recursively merge *override* onto *base*, returning *base* modified in-place

        Keys present in *base* but absent from *override* are left untouched, which is
        exactly the behaviour needed when merging a partial inspector export back onto
        the full ppp_generated.yaml so that keys the inspector does not know about (e.g.
        constellation blocks that were not checked, trace output flags, etc.) are preserved.

        Lists are replaced wholesale - the inspector always emits a complete list for any
        key it writes, so element-level merging would produce duplicates.

        Arguments:
          base (dict):     The existing full config (will be mutated).
          override (dict): The partial config from the inspector export.

        Returns:
          dict: *base* after merging.
        """
        for key, override_val in override.items():
            if key in base and isinstance(base[key], dict) and isinstance(override_val, dict):
                Inspector.deep_merge(base[key], override_val)
            else:
                base[key] = override_val
        return base

    @staticmethod
    def fix_inspector_yaml(yaml_text: str) -> str:
        """
        Pre-process the raw YAML text from the GinanYAMLInspector

        GinanYAMLInspector copies the values directly from its HTML input fields into the
        generated YAML without adding quotes to them. Two classes of value break ruamel.yaml:

        1. Wildcard glob patterns containing "*" (e.g. *.CLK, BRDC*, *_ocean.BLQ)
           In YAML, a bare "*" is the alias indicator, so ruamel raises
           "found undefined alias" when it encounters these unquoted

        2. Trailing whitespace on lines, which can confuse ruamel's indentation
           parser in certain edge cases

        Arguments:
          yaml_text (str): Raw YAML string from the inspector textbox

        Returns:
          str: Sanitised YAML string safe to pass to ruamel.yaml.load()
        """
        # Strip trailing whitespace from every line
        yaml_text = "\n".join(line.rstrip() for line in yaml_text.splitlines())

        # Quote wildcard patterns in flow sequences: [*.CLK] -> ['*.CLK']
        def _quote_flow_wildcards(m):
            items = m.group(1).split(',')
            fixed = []
            for item in items:
                s = item.strip()
                if s and '*' in s and not (s.startswith('"') or s.startswith("'")):
                    s = f"'{s}'"
                fixed.append(s)
            return '[' + ', '.join(fixed) + ']'

        yaml_text = re.sub(r'\[([^\]]*\*[^\]]*)\]', _quote_flow_wildcards, yaml_text)

        # Quote wildcard patterns in block list items: "    - *.CLK" -> "    - '*.CLK'"
        def _quote_block_wildcards(m):
            indent, value = m.group(1), m.group(2)
            if '*' in value and not (value.startswith('"') or value.startswith("'")):
                value = f"'{value}'"
            return f"{indent}- {value}"

        yaml_text = re.sub(
            r'^(\s*)- (\S*\*\S*)$',
            _quote_block_wildcards,
            yaml_text,
            flags=re.MULTILINE,
        )

        return yaml_text

    @staticmethod
    def fix_written_yaml(yaml_text: str) -> str:
        """
        Repair YAML text that ruamel.yaml has written incorrectly

        ruamel.yaml occasionally collapses a parent mapping key and its first child
        onto a single line, e.g.:

            outputs: metadata:
                config_description: ...

        instead of the correct:

            outputs:
                metadata:
                    config_description: ...

        This produces "mapping values are not allowed here" when the file is
        subsequently parsed. The fix splits any line that contains two mapping
        keys separated by a space (i.e. "key1: key2:") into separate indented
        lines, restoring the correct block-mapping structure

        Arguments:
          yaml_text (str): YAML text as written by ruamel.yaml

        Returns:
          str: Corrected YAML text
        """
        fixed_lines = []
        for line in yaml_text.splitlines():
            # Detect lines of the form "<indent>key1: key2:" where key2 is itself
            # a bare mapping key (no value after it, or only whitespace/comment)
            # Pattern: optional spaces, plain-scalar key, ": ", another plain-scalar
            # key ending with ":" and nothing else (or just a comment)
            m = re.match(
                r'^(\s*)([A-Za-z0-9_]+):\s+([A-Za-z0-9_]+):\s*(#.*)?$',
                line
            )
            if m:
                outer_indent = m.group(1)
                outer_key    = m.group(2)
                inner_key    = m.group(3)
                comment      = m.group(4) or ""
                inner_indent = outer_indent + "    "
                fixed_lines.append(f"{outer_indent}{outer_key}:")
                fixed_lines.append(f"{inner_indent}{inner_key}:{(' ' + comment) if comment else ''}")
            else:
                fixed_lines.append(line)
        return "\n".join(fixed_lines)

    #endregion

    #region Export Config from Inspector

    def merge_and_save(self, yaml_text: str) -> dict:
        """
        Sanitise, merge, and write the inspector's YAML output to ppp_generated.yaml

        The inspector only emits keys that are checked in its UI, so a direct overwrite
        would discard keys that Ginan-UI depends on (constellation blocks, trace flags,
        etc.). Instead, the inspector output is deep-merged onto the existing
        ppp_generated.yaml so unchecked / unknown keys are preserved

        After merging, the result is validated before being committed locally
        If the comment-preserving write produces a file that ruamel cannot re-read
        (such as a known ruamel edge case with certain comment / value combinations), a clean write
        without comment preservation is used as a fallback so the save will always succeeds

        Arguments:
          yaml_text (str): Raw YAML string produced by the inspector's "Generate yaml" step

        Returns:
          dict: The merged config dict that was written to disk. The caller can use
                this to repopulate UI fields if reloading from disk fails.

        Raises:
          ValueError: If yaml_text is empty / whitespace-only, or if the inspector
                      output does not parse to a YAML mapping.
          Exception:  Any other failure during parse / merge / write / validate is
                      re-raised so the caller can surface it to the user.
        """
        if not yaml_text or not yaml_text.strip():
            Logger.workflow("⚠️ GinanYAMLInspector returned empty YAML - saving aborted")
            raise ValueError("Inspector returned empty YAML")

        # Pre-process the inspector output: quote wildcard glob patterns that
        # would be misread as YAML aliases, and strip trailing whitespace.
        sanitised_text = self.fix_inspector_yaml(yaml_text)

        # Parse the inspector output
        _yaml = RuamelYAML()
        inspector_data = _yaml.load(sanitised_text)
        if not isinstance(inspector_data, dict):
            raise ValueError("Inspector YAML did not parse to a mapping")

        # Load the existing full config so we can merge onto it
        if GENERATED_YAML.exists():
            existing_data = load_yaml(GENERATED_YAML)
        else:
            existing_data = {}

        # Deep-merge: inspector values win, but keys absent from the inspector are kept
        merged = self.deep_merge(existing_data, inspector_data)

        # Write with comment preservation and validate the result is re-parseable
        # ruamel.yaml has an edge case where it collapses a parent key and its
        # first child onto the same line (e.g. "outputs: metadata:"), producing
        # a file that cannot be re-parsed. _fix_written_yaml detects and repairs
        # this before we validate, so the corrected content is what ends up on disk
        GENERATED_YAML.parent.mkdir(parents = True, exist_ok = True)
        write_yaml(GENERATED_YAML, merged)

        # Read back, repair any collapsed-key lines, and rewrite
        written_text = GENERATED_YAML.read_text(encoding = "utf-8")
        fixed_text   = self.fix_written_yaml(written_text)
        if fixed_text != written_text:
            GENERATED_YAML.write_text(fixed_text, encoding = "utf-8")

        # Validate the (possibly repaired) file is re-parseable
        try:
            load_yaml(GENERATED_YAML)
        except Exception:
            # Fall back: write a clean copy without ruamel's comment machinery
            _clean_yaml = RuamelYAML()
            _clean_yaml.default_flow_style = False
            _clean_yaml.indent(mapping=4, sequence=4, offset=4)
            _clean_yaml.width = 4096
            with GENERATED_YAML.open("w", encoding="utf-8") as f:
                _clean_yaml.dump(dict(merged), f)
            # Final validation — if this also fails, surface the error
            load_yaml(GENERATED_YAML)

        Logger.workflow(f"✅ GinanYAMLInspector config saved to: {GENERATED_YAML}")
        return merged

    #endregion
