# app/controllers/__init__.py
"""
Controller layer for the Ginan-UI application.

Controllers coordinate between the UI (views) and the backend (models / utils).
Each controller is responsible for a specific area of the UI:

  InputController               - Parent controller: top-level buttons (Observations,
                                  Output, Show Config, Process, Stop All, CDDIS
                                  Credentials, User Manual, Reset Config), shared state,
                                  and ExtractedInputs dataclass.

  GeneralConfigController       - General config tab: mode, constellations multi-select,
                                  PPP provider / project / series, receiver / antenna types,
                                  time window, data interval, antenna offset, apriori
                                  position. Also owns background workflows for CDDIS
                                  archive scanning and SINEX file validation.

  ConstellationConfigController - Constellations config tab: observation code list widgets
                                  with drag-drop reordering and checkboxes, BIA code
                                  priority fetching and validation styling,
                                  placeholder / status labels.

  OutputConfigController        - Output config tab: POS, GPX, TRACE, SNX file output
                                  checkboxes.

  VisualisationController       - Visualisation panel: embedded HTML plot display,
                                  external browser opening, plot selector combo box.
"""
