"""
Unified logging system for Ginan-UI

This module provides a thread-safe logging interface that can passes
messages to different UI channels ("terminal" or "console" at the moment) via Qt signals.

Usage:
    # In main_window.py initialisation:
    Logger.initialise(main_window_instance)
    
    # Anywhere in your code:
    Logger.terminal("Message for terminal")
    Logger.console("Message for console")
    Logger.both("Message for both channels")
"""

from PySide6.QtCore import QObject, Signal
from typing import Optional


class LoggerSignals(QObject):
    """Signal container for thread-safe logging"""
    terminal_signal = Signal(str)
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
        cls._signals.terminal_signal.connect(
            lambda msg: main_window.log_message(msg, channel = "terminal")
        )
        cls._signals.console_signal.connect(
            lambda msg: main_window.log_message(msg, channel = "console")
        )

    @classmethod
    def terminal(cls, message: str):
        """
        Log a message to the terminal widget.
        Thread-safe.

        :param message: Message to log
        """
        if cls._signals is None:
            print(f"[Logger not initialised - terminal] {message}")
            return

        # Simply emit the signal - Qt handles thread safety automatically
        cls._signals.terminal_signal.emit(message)

    @classmethod
    def console(cls, message: str):
        """
        Log a message to the console widget.
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
        Log a message to both terminal and console widgets.
        Thread-safe.

        :param message: Message to log
        """
        cls.terminal(message)
        cls.console(message)

    @classmethod
    def is_initialised(cls) -> bool:
        """Check if the logger has been initialised"""
        return cls._signals is not None