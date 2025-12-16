import subprocess, shutil
from pathlib import Path


def compile_ui():
    """
    Compile the Qt `.ui` file into a Python module and fix its resource import.

    Converts `main_window.ui` into `main_window_ui.py` using `pyside6-uic`,
    then updates the logo import line for correct resource loading.

    Raises:
      ImportError: If `pyside6-uic` is not found.

    Example:
      >>> compile_ui()
      UI compiled successfully.
    """

    # File paths
    ui_file = Path(__file__).parent.parent / "views" / "main_window.ui"
    output_file = Path(__file__).parent.parent / "views" / "main_window_ui.py"

    # Ensure compiler exists
    if shutil.which("pyside6-uic"):
        with open(output_file, 'w') as f:
            f.write("# This file is auto-generated. Do not edit.\n")
        result = subprocess.run(["pyside6-uic", ui_file, "-o", output_file], capture_output=True)
        if result.returncode != 0:
            print(f"Error compiling UI: {result.stderr.decode()}")
        else:
            print("UI compiled successfully.")
            print(result.stdout.decode())
    else:
        raise ImportError("Ensure pyside6-uic is installed and available on PATH.")

    # Manually fix the file path to the logo resource
    with open(output_file, 'r') as f:
        lines = f.readlines()
        for i, line in enumerate(lines):
            if line == "import ginan_logo_rc\n":
                lines[i] = "from scripts.GinanUI.app.resources import ginan_logo_rc"
                break
    with open(output_file, 'w') as f:
        f.writelines(lines)

if __name__ == "__main__":
    compile_ui()