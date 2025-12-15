from PySide6.QtWidgets import QLabel, QGraphicsOpacityEffect, QPushButton
from PySide6.QtCore import QTimer, QPropertyAnimation, QEasingCurve, Qt, QEvent
from PySide6.QtGui import QFont


class Toast(QLabel):
    """
    A non-blocking toast notification that appears at the bottom of the window,
    fades in, stays visible, then fades out automatically.
    """

    def __init__(self, parent=None):
        super().__init__(parent)

        # Makes it a child widget so it moves with the program
        self.setWindowFlags(Qt.WindowType.Widget)
        self.setAttribute(Qt.WidgetAttribute.WA_ShowWithoutActivating)

        # Styling
        self.setStyleSheet("""
            QLabel {
                background-color: #2c5d7c;
                color: #ffffff;
                padding: 14px 40px 14px 24px;
                border-radius: 8px;
                font-size: 13px;
                font-weight: 500;
                border: 1px solid rgba(255, 255, 255, 0.2);
            }
        """)
        self.setAlignment(Qt.AlignmentFlag.AlignCenter)

        # Font
        font = QFont()
        font.setPointSize(12)
        font.setWeight(QFont.Weight.Medium)
        self.setFont(font)

        # Close button
        self.close_button = QPushButton("×", self)
        self.close_button.setStyleSheet("""
            QPushButton {
                background-color: transparent;
                color: rgba(255, 255, 255, 0.7);
                border: none;
                font-size: 20px;
                font-weight: bold;
                padding: 0px;
                margin: 0px;
            }
            QPushButton:hover {
                color: rgba(255, 255, 255, 1.0);
                background-color: rgba(255, 255, 255, 0.1);
            }
        """)
        self.close_button.setFixedSize(24, 24)
        self.close_button.setCursor(Qt.CursorShape.PointingHandCursor)
        self.close_button.clicked.connect(self._on_close_clicked)
        self.close_button.hide()

        # Opacity effect for fade animations
        self.opacity_effect = QGraphicsOpacityEffect(self)
        self.setGraphicsEffect(self.opacity_effect)
        self.opacity_effect.setOpacity(0.0)

        # Animation for fade in and out
        self.fade_animation = QPropertyAnimation(self.opacity_effect, b"opacity")
        self.fade_animation.setDuration(300)  # 300ms fade
        self.fade_animation.setEasingCurve(QEasingCurve.Type.InOutQuad)

        # Timer for auto-hide
        self.hide_timer = QTimer(self)
        self.hide_timer.setSingleShot(True)
        self.hide_timer.timeout.connect(self._fade_out)

        # Track if fade_out signal is connected
        self._fade_connected = False

        # Install event filter on parent to reposition on resize
        if parent:
            parent.installEventFilter(self)

    def eventFilter(self, obj, event):
        """Reposition toast when parent window is resized or moved."""
        if obj == self.parent() and event.type() in (QEvent.Type.Resize, QEvent.Type.Move):
            if self.isVisible():
                self._update_position()
        return super().eventFilter(obj, event)

    def _update_position(self):
        """Update the toast position to stay centered at bottom of the program."""
        if self.parent():
            parent_rect = self.parent().rect()
            x = (parent_rect.width() - self.width()) // 2
            y = parent_rect.height() - self.height() - 50  # 50px from bottom
            self.move(x, y)

            # Position close button at top-right corner of toast
            self.close_button.move(
                self.width() - self.close_button.width() - 8,  # 8px from right edge
                8  # 8px from top edge
            )

    def show_message(self, message: str, duration: int = 3000):
        """
        Show a toast message for a specified duration.

        Arguments:
            message (str): Text to display
            duration (int): How long to show the message in milliseconds (default 3000ms = 3s)
        """
        # Stop any ongoing animation and timer to prevent flashing
        self.fade_animation.stop()
        self.hide_timer.stop()

        # Disconnect any previous finished signal connections (only if connected)
        if self._fade_connected:
            try:
                self.fade_animation.finished.disconnect()
                self._fade_connected = False
            except:
                pass

        # Reset width constraints so the toast can resize for new message
        self.setMinimumWidth(0)
        self.setMaximumWidth(16777215)  # Qt's default maximum - just really big

        self.setText(message)
        self.adjustSize()

        # Ensure a minimum width
        if self.width() < 250:
            self.setFixedWidth(250)

        # Position at bottom-center of parent window
        self._update_position()

        # Show close button
        self.close_button.show()
        self.close_button.raise_()

        # Fade in
        self.show()
        self.raise_()  # Bring to front
        self.fade_animation.setStartValue(self.opacity_effect.opacity())  # Start from current opacity
        self.fade_animation.setEndValue(1.0)
        self.fade_animation.start()

        # Schedule fade out
        self.hide_timer.start(duration)

    def _on_close_clicked(self):
        """Handle close button click - fade out immediately."""
        self._fade_out()

    def _fade_out(self):
        """Fade out the toast and hide it."""
        self.fade_animation.stop()

        # Hide close button
        self.close_button.hide()

        # Disconnect previous connections to avoid multiple hide() calls
        if self._fade_connected:
            try:
                self.fade_animation.finished.disconnect()
                self._fade_connected = False
            except:
                pass

        self.fade_animation.setStartValue(self.opacity_effect.opacity())
        self.fade_animation.setEndValue(0.0)
        self.fade_animation.finished.connect(self.hide)
        self._fade_connected = True
        self.fade_animation.start()


def show_toast(parent, message: str, duration: int = 3000):
    """
    User feedback function to show a small toast notification at the bottom of the program.

    Arguments:
        parent: Parent widget (typically "main_window.py)
        message (str): Message to display
        duration (int): Display duration in milliseconds (default 3000ms)

    Returns:
        Toast: The toast instance (not required, but this does allow handling of the toast instance)

    Example:
        show_toast(self.main_window, "Scanning CDDIS archive...", 5000)
    """
    # Reuse existing toast if available
    if not hasattr(parent, '_toast_widget'):
        parent._toast_widget = Toast(parent)

    parent._toast_widget.show_message(message, duration)
    return parent._toast_widget