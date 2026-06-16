"""
Unified logging system for Ginan-UI

This module provides a thread-safe logging interface that can passes
messages to different UI channels ("Workflow" or "Console" at the moment) via Qt signals.
Must be initialised with a MainWindow instance
before use; falls back to stdout if uninitialised.

Usage:
    # In main_window.py initialisation:
    Logger.initialise(main_window_instance)

    # Anywhere in your code:
    Logger.workflow("Message for workflow")
    Logger.console("Message for console")
    Logger.both("Message for both channels")
"""

from PySide6.QtCore import QObject, Signal
from typing import Optional

class LoggerSignals(QObject):
    """Signal container for thread-safe logging"""
    workflow_signal = Signal(str)
    console_signal = Signal(str)

class Logger:
    """
    Static logger class for easy logging throughout the application.

    All methods are thread-safe and can be called from worker threads.
    """
    _signals: Optional[LoggerSignals] = None
    _main_window = None

    @classmethod
    def initialise(cls, main_window):
        """
        Initialise the logger with the main window instance.

        :param main_window: MainWindow instance with log_message method
        """
        cls._main_window = main_window
        cls._signals = LoggerSignals()

        # Connect signals to main window's log_message method
        cls._signals.workflow_signal.connect(
            lambda msg: main_window.log_message(msg, channel = "workflow")
        )
        cls._signals.console_signal.connect(
            lambda msg: main_window.log_message(msg, channel = "console")
        )

    @classmethod
    def workflow(cls, message: str):
        """
        Log a message to the "Workflow" widget.
        Thread-safe.

        :param message: Message to log
        """
        if cls._signals is None:
            print(f"[Logger not initialised - workflow] {message}")
            return

        # Simply emit the signal - Qt handles thread safety automatically
        cls._signals.workflow_signal.emit(message)

    @classmethod
    def console(cls, message: str):
        """
        Log a message to the "Console" widget.
        Thread-safe.

        :param message: Message to log
        """
        if cls._signals is None:
            print(f"[Logger not initialised - console] {message}")
            return

        # Simply emit the signal - Qt handles thread safety automatically
        cls._signals.console_signal.emit(message)

    @classmethod
    def both(cls, message: str):
        """
        Log a message to both "Workflow" and "Console" widgets.
        Thread-safe.

        :param message: Message to log
        """
        cls.workflow(message)
        cls.console(message)

    @classmethod
    def is_initialised(cls) -> bool:
        """Check if the logger has been initialised"""
        return cls._signals is not None