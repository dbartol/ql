from enum import Enum, IntEnum
import sys
from typing import NoReturn

from location import Location

class Severity(IntEnum):
    WARNING = 1
    ERROR = 2
    FATAL_ERROR = 3

    def text(self) -> str:
        if self == Severity.WARNING:
            return 'warning'
        elif self == Severity.ERROR:
            return 'error'
        elif self == Severity.FATAL_ERROR:
            return 'fatal error'
        else:
            return '<unknown severity>'

class Diagnostic:
    def __init__(self, severity: Severity, message: str):
        self.severity = severity
        self.message = message

def emit_diagnostic(diagnostic: Diagnostic, location: Location, min_severity: Severity, args):
    location_string = f"{location.file}({location.start_line},{location.start_column}): "
    severity = max(min_severity, diagnostic.severity)
    severity_string = f"{severity.text()}: "
    message_string = diagnostic.message.format_map(args)
    full_string = f"{location_string}{severity_string}{message_string}"
    print(full_string, file=sys.stderr)
    if severity >= Severity.ERROR:
        raise Exception('Fatal error.')

def emit_error(diagnostic: Diagnostic, location: Location, **kwargs) -> NoReturn:
    emit_diagnostic(diagnostic, location, Severity.ERROR, kwargs)
    raise Exception('Unreached')
