#!/usr/bin/env python3

from enum import Enum, IntEnum
import json
import os
from os import path
import re
import sys
import tempfile
from typing import List, Tuple

Span = Tuple[int, int]

begin_generated_line_re = re.compile('^//\{\{')
end_generated_line_re = re.compile('^//\}\}')

template_directive_re = re.compile('(//#)(.*)$')

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

class Location:
    def __init__(self, file: str, line: int, span: Span):
        self.file = file.replace('/', os.sep)
        self.start_line = line
        self.end_line = line
        self.start_column = span[0]
        self.end_column = span[1]

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

def emit_error(diagnostic: Diagnostic, location: Location, **kwargs):
    emit_diagnostic(diagnostic, location, Severity.ERROR, kwargs)

def chdir_repo_root():
    root_path = path.join(path.dirname(path.abspath(__file__)), '..')
    os.chdir(root_path)

def strip_generated_lines(seed_file_path: str, master_file_path: str):
    with open(seed_file_path, 'r', encoding='utf-8') as seed_file:
        with open(master_file_path, 'w', encoding='utf-8') as master_file:
            in_generated_code = False
            for line in seed_file:
                if in_generated_code:
                    if end_generated_line_re.match(line):
                        in_generated_code = False
                else:
                    if begin_generated_line_re.match(line):
                        in_generated_code = True
                    else:
                        master_file.write(line)

def create_master_file(group, template: str, group_temp_root: str) -> str:
    print(f"group.directory = {group['directory']}")
    print(f"template = {template}")

    seed_file_path = path.join(group['directory'], template)
    master_file_path = path.join(group_temp_root, template)
    strip_generated_lines(seed_file_path, master_file_path)

    print(f"Created master file at '{master_file_path}'.")

    return master_file_path

class NodeKind(Enum):
    ERROR = '<error>'
    WHITESPACE = '<whitespace>'
    EOL = '<end of line>'
    ID = '<identifier>'
    OPEN_ANGLE = '<'
    CLOSE_ANGLE = '>'
    DOT = '.'
    COMMA = ','
    AS_KEYWORD = 'as'
    IMPORT_KEYWORD = 'import'
    MODULE_KEYWORD = 'module'
    PRIVATE_KEYWORD = 'private'
    MODULE_DECLARATION = '<module declaration>'
    MODULE_IMPORT = '<module import>'
    MODULE_PARAMETER = '<module parameter'
    AS_CLAUSE = '<as clause>'
    DOTTED_ID = '<dotted identifier>'

class Node:
    def __init__(self, kind: NodeKind):
        self.kind = kind

class Token(Node):
    def __init__(self, kind: NodeKind, location: Location, text: str):
        super().__init__(kind)
        self.location = location
        self.text = text

class WhitespaceToken(Token):
    def __init__(self, location: Location, text: str):
        super().__init__(NodeKind.WHITESPACE, location, text)

class IdToken(Token):
    def __init__(self, location: Location, text: str):
        super().__init__(NodeKind.ID, location, text)

class UniqueToken(Token):
    def __init__(self, location: Location, text: str):
        super().__init__(NodeKind(text), location, text)

class EOLToken(Token):
    def __init__(self, location: Location):
        super().__init__(NodeKind.EOL, location, NodeKind.EOL.value)

class ErrorToken(Token):
    def __init__(self, location: Location, text: str):
        super().__init__(NodeKind.ERROR, location, text)

unexpected_token_error = Diagnostic(Severity.ERROR, "Unexpected token '{token}'.")

def emit_unexpected_token_error(token: 'Token'):
    emit_error(unexpected_token_error, token.location, token=token.text)

module_arg_count_mismatch_error = Diagnostic(Severity.ERROR,
    "Module expected {param_count} arguments, but {arg_count} were provided for instantiation in '{instantiation}'.")

def emit_module_arg_count_mismatch_error(location: Location, param_count: int, arg_count: int,
    instantiation: str):

    emit_error(module_arg_count_mismatch_error, location, param_count=param_count,
        arg_count=arg_count, instantiation=instantiation)

class TokenStream:
    token_pattern = re.compile(r"(?P<whitespace>(?:[ \t]+)|//#)|(?P<unique>(?:[<>,.]|as|import|module))|(?P<id>[A-Za-z][A-Za-z0-9_]*)|(?P<error>.)")

    def __init__(self, line: str, file: str, line_number: int):
        self.tokens: List[Token] = [
            token for token in (
                TokenStream.create_token(token_match, file, line_number)
                for token_match in TokenStream.token_pattern.finditer(line)
            )
            if not token.kind == NodeKind.WHITESPACE
        ]
        self.tokens.append(EOLToken(Location(file, line_number, (len(line), len(line)))))
        self.current_index = 0
    
    def create_token(match, file: str, line_number: int) -> Token:
        whitespace = match.group('whitespace')
        if whitespace:
            location = Location(file, line_number, match.span('whitespace'))
            return WhitespaceToken(location, whitespace)
        id = match.group('id')
        if id:
            location = Location(file, line_number, match.span('id'))
            return IdToken(location, id)
        unique = match.group('unique')
        if unique:
            location = Location(file, line_number, match.span('unique'))
            return UniqueToken(location, unique)
        error = match.group('error')
        location = Location(file, line_number, match.span('error'))
        error_token = ErrorToken(location, error)
        emit_unexpected_token_error(error_token)

    def seek(self, position: int):
        self.current_index = position

    def position(self) -> int:
        return self.current_index

    def peek(self) -> Token:
        return self.tokens[self.current_index]

    def read(self) -> Token:
        index = self.current_index
        if not self.done():
            self.current_index = self.current_index + 1
        return self.tokens[index]

    def done(self) -> bool:
        return self.current_index == len(self.tokens) - 1

class ModuleParameterNode(Node):
    def __init__(self, id_token: IdToken):
        super().__init__(NodeKind.MODULE_PARAMETER)
        self.id_token = id_token

class ModuleDeclarationNode(Node):
    def __init__(self, module_token: UniqueToken, open_angle_token: UniqueToken,
        parameters: List[ModuleParameterNode], close_angle_token: UniqueToken):

        super().__init__(NodeKind.MODULE_DECLARATION)
        self.module_token = module_token
        self.open_angle_token = open_angle_token
        self.parameters = parameters
        self.close_angle_token = close_angle_token

class ModuleImportNode(Node):
    def __init__(self):
        super().__init__(NodeKind.MODULE_IMPORT)

class AsClauseNode(Node):
    def __init__(self, as_token: UniqueToken, id_token: IdToken):
        super().__init__(NodeKind.AS_CLAUSE)
        self.as_token = as_token
        self.id_token = id_token

class DottedIdNode(Node):
    def __init__(self, components: List[IdToken]):
        super().__init__(NodeKind.DOTTED_ID)
        self.components = components

class Parser:
    def __init__(self, line: str, file: str, line_number: int):
        self.token_stream = TokenStream(line, file, line_number)

    def parse(self) -> Node:
        peek = self.token_stream.peek()
        if peek.kind == NodeKind.PRIVATE_KEYWORD or peek.kind == NodeKind.IMPORT_KEYWORD:
            return self.parse_import()
        if peek.kind == NodeKind.MODULE_KEYWORD:
            return self.parse_module_declaration()
        emit_unexpected_token_error(peek)

    def optional(self, kind: str) -> Token:
        peek = self.token_stream.peek()
        if peek.kind == kind:
            return self.token_stream.read()
        else:
            return None

    def expect(self, kind: str) -> Token:
        peek = self.token_stream.peek()
        if peek.kind == kind:
            return self.token_stream.read()
        else:
            emit_unexpected_token_error(peek)
    
    def parse_module_import(self) -> ModuleImportNode:
        private_token = self.optional(NodeKind.MODULE_KEYWORD)
        import_token = self.expect(NodeKind.IMPORT_KEYWORD)
        return None

    def parse_optional_as_clause(self) -> AsClauseNode:
        as_token = self.optional(NodeKind.AS_KEYWORD)
        if as_token:
            id_token = self.expect(NodeKind.ID)
            return AsClauseNode(as_token, id_token)
        else:
            return None

    def parse_module_parameter_list(self) -> List[ModuleParameterNode]:
        parameters: List[ModuleParameterNode] = []
        while True:
            parameters.append(self.parse_module_parameter())
            comma = self.optional(NodeKind.COMMA)
            if not comma:
                return parameters

    def parse_dotted_id(self) -> DottedIdNode:
        components: List[IdToken] = []
        while True:
            components.append(self.expect(NodeKind.ID))
            dot = self.optional('.')
            if not dot:
                return components

    def parse_module_parameter(self) -> ModuleParameterNode:
        return ModuleParameterNode(self.expect(NodeKind.ID))

    def parse_module_declaration(self) -> ModuleDeclarationNode:
        module_token = self.expect(NodeKind.MODULE_KEYWORD)
        open_angle_token = self.expect(NodeKind.OPEN_ANGLE)

        parameters = self.parse_module_parameter_list()

        close_angle_token = self.expect(NodeKind.CLOSE_ANGLE)
        self.expect(NodeKind.EOL)

        return ModuleDeclarationNode(module_token, open_angle_token, parameters, close_angle_token)

class Instantiator:
    def __init__(self, template, instantiation, master_file_path: str):
        self.template = template
        self.instantiation = instantiation
        self.master_file_path = master_file_path
        self.instantiation_file_path = path.join(instantiation['directory'], template)

    def process_template_directive(self, line: str):
        parser = Parser(line, self.master_file_path, self.line_number)
        node = parser.parse()
        if node.kind == NodeKind.MODULE_DECLARATION:
            self.process_module_declaration(node)
        elif node.kind == NodeKind.MODULE_IMPORT:
            self.process_module_import(node)

    def process_module_import(self, node: ModuleImportNode):
        pass

    def process_module_declaration(self, node: ModuleDeclarationNode):
        args = self.instantiation['args']

        # Check for an argument count mismatch
        if len(args) != len(node.parameters):
            emit_module_arg_count_mismatch_error(node.module_token.location, len(node.parameters),
                len(args), self.instantiation['directory'])

        self.instantiation_file.write('//{{\n')
        for index, arg in enumerate(args):
            resolved_arg = self.resolve_module_instantiations(arg)
            self.instantiation_file.write(f"private import {resolved_arg} as {node.parameters[index].id_token.text}\n")
        self.instantiation_file.write('//}}\n')
    
    def resolve_module_instantiations(self, module: str) -> str:
        return module

    def instantiate(self):
        with open(self.master_file_path, 'r', encoding='utf-8') as master_file:
            os.makedirs(path.dirname(self.instantiation_file_path), 0o777, True)
            with open(self.instantiation_file_path, 'w', encoding='utf-8') as instantiation_file:
                self.instantiation_file = instantiation_file
                self.line_number = 0
                for line in master_file:
                    self.line_number = self.line_number + 1
                    instantiation_file.write(line)
                    m = template_directive_re.match(line)
                    if m:
                        self.process_template_directive(m.group(0))

def create_instantiation_files(master_file_path: str, group, template: str):
    for instantiation in group['instantiations']:
        instantiator = Instantiator(template, instantiation, master_file_path)
        instantiator.instantiate()

def sync_group(group, temp_root: str):
    group_temp_root = path.join(temp_root, group['directory'])
    os.makedirs(group_temp_root, 0o777, True)
    print(group_temp_root)

    for template in group['templates']:
        master_file_path = create_master_file(group, template, group_temp_root)
        create_instantiation_files(master_file_path, group, template)

chdir_repo_root()

print(os.getcwd())
with open('config/templates.json', 'r', encoding='utf-8') as templates_file:
    templates = json.load(templates_file)

temp_root = tempfile.mkdtemp('pyram')
temp_root = 'C:/Src/Repro/pyram'

for group in templates['groups']:
    sync_group(group, temp_root)