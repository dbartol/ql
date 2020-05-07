import os
from typing import Tuple

Span = Tuple[int, int]

class Location:
    def __init__(self, file: str, line: int, span: Span) -> None:
        self.file: str = file.replace('/', os.sep)
        self.start_line: int = line
        self.end_line: int = line
        self.start_column: int = span[0]
        self.end_column: int = span[1]
