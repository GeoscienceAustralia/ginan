"""
Controller for the Output configuration tab.

Manages the following UI widgets:
  - POS output checkbox (Positioning Solution file)
  - GPX output checkbox (GPS Exchange Format file)
  - TRACE output checkbox (trace log file)
  - SNX output checkbox (SINEX file)

This controller is intentionally minimal to allow easy expansion as
new output formats or options are added in the future.
"""

from __future__ import annotations

class OutputConfigController:
    """
    Manages the Output configuration tab: output file type checkboxes.

    Arguments:
      ui: The main window UI instance.
      input_ctrl: The parent InputController instance (for accessing shared state).
    """

    def __init__(self, ui, input_ctrl):
        """
        Initialise output config tab bindings.

        Arguments:
          ui: The main window UI instance.
          input_ctrl: The parent InputController that owns shared state.
        """
        self.ui = ui
        self.ctrl = input_ctrl  # parent InputController

    #region UI Tooltips

    def setup_tooltips(self):
        """
        Set up tooltips for all Output config tab widgets.
        """
        self.ui.posCheckbox.setToolTip(
            "Enable / disable Ginan (PEA) PPP Processing outputting a Positioning Solution (.POS) file"
        )
        self.ui.gpxCheckbox.setToolTip(
            "Enable / disable Ginan (PEA) PPP Processing outputting a GPS Exchange Format (.GPX) file"
        )
        self.ui.traceCheckbox.setToolTip(
            "Enable / disable Ginan (PEA) PPP Processing outputting a trace log (.TRACE) file"
        )
        self.ui.snxCheckbox.setToolTip(
            "Enable / disable Ginan (PEA) PPP Processing outputting a Solution Independent (.SNX) file"
        )

    #endregion

    #region Output Toggles

    def get_output_toggles(self) -> tuple[bool, bool, bool, bool]:
        """
        Read the current state of the output checkboxes.

        Returns:
          tuple[bool, bool, bool, bool]: (gpx_output, pos_output, trace_output_network, snx_output)
        """
        gpx_output = self.ui.gpxCheckbox.isChecked() if hasattr(self.ui, "gpxCheckbox") else True
        pos_output = self.ui.posCheckbox.isChecked() if hasattr(self.ui, "posCheckbox") else True
        trace_output_network = self.ui.traceCheckbox.isChecked() if hasattr(self.ui, "traceCheckbox") else False
        snx_output = self.ui.snxCheckbox.isChecked() if hasattr(self.ui, "snxCheckbox") else True
        return gpx_output, pos_output, trace_output_network, snx_output

    #endregion

    #region Reset to Defaults

    def reset_to_defaults(self):
        """
        Reset all Output config tab fields to their default states.
        """
        if hasattr(self.ui, 'posCheckbox'):
            self.ui.posCheckbox.setChecked(True)
        if hasattr(self.ui, 'gpxCheckbox'):
            self.ui.gpxCheckbox.setChecked(True)
        if hasattr(self.ui, 'traceCheckbox'):
            self.ui.traceCheckbox.setChecked(False)
        if hasattr(self.ui, 'snxCheckbox'):
            self.ui.snxCheckbox.setChecked(False)

    #endregion