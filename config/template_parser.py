import re
from enum import Enum
from typing import List, Match, NoReturn, Optional, Set, Tuple, cast

from diagnostics import Diagnostic, Severity, emit_error
from location import Location

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
    MODULE_PARAMETER = '<module parameter>'
    MODULE_SPECIFIER = '<module specifier>'
    MODULE_ARGUMENT_LIST = '<module argument list>'
    AS_CLAUSE = '<as clause>'
    DOTTED_ID = '<dotted identifier>'

class Node:
    def __init__(self, kind: NodeKind, location: Location):
        self.kind: NodeKind = kind
        self.location: Location = location

class Token(Node):
    def __init__(self, kind: NodeKind, location: Location, text: str):
        super().__init__(kind, location)
        self.text: str = text

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

def emit_unexpected_token_error(token: 'Token') -> NoReturn:
    emit_error(unexpected_token_error, token.location, token=token.text)

module_arg_count_mismatch_error = Diagnostic(Severity.ERROR,
    "Module expected {param_count} arguments, but {arg_count} were provided for instantiation in '{instantiation}'.")

def emit_module_arg_count_mismatch_error(location: Location, param_count: int, arg_count: int,
    instantiation: str):

    emit_error(module_arg_count_mismatch_error, location, param_count=param_count,
        arg_count=arg_count, instantiation=instantiation)

keywords: Set[str] = { 'as', 'import', 'module', 'private' }

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
    
    @staticmethod
    def create_token(match: Match[str], file: str, line_number: int) -> Token:
        whitespace = match.group('whitespace')
        if whitespace:
            location = Location(file, line_number, match.span('whitespace'))
            return WhitespaceToken(location, whitespace)
        id = match.group('id')
        if id:
            location = Location(file, line_number, match.span('id'))
            if id in keywords:
                return UniqueToken(location, id)
            else:
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

class Nonterminal(Node):
    def __init__(self, kind: NodeKind, location: Location) -> None:
        super().__init__(kind, location)

class ModuleParameterNode(Nonterminal):
    def __init__(self, id_token: IdToken):
        super().__init__(NodeKind.MODULE_PARAMETER, id_token.location)
        self.id_token = id_token

class ModuleDeclarationNode(Nonterminal):
    def __init__(self, module_token: UniqueToken, open_angle_token: UniqueToken,
        parameters: List[ModuleParameterNode], close_angle_token: UniqueToken):

        super().__init__(NodeKind.MODULE_DECLARATION, module_token.location)
        self.module_token: UniqueToken = module_token
        self.open_angle_token: UniqueToken = open_angle_token
        self.parameters: List[ModuleParameterNode] = parameters
        self.close_angle_token: UniqueToken = close_angle_token

class ModuleArgumentListNode(Nonterminal):
    def __init__(self, open_angle_token: UniqueToken, args: List['ModuleSpecifierNode'],
        close_angle_token: UniqueToken) -> None:

        super().__init__(NodeKind.MODULE_ARGUMENT_LIST, open_angle_token.location)
        self.open_angle_token: UniqueToken = open_angle_token
        self.args: List[ModuleSpecifierNode] = args
        self.close_angle_token: UniqueToken = close_angle_token

class ModuleSpecifierNode(Nonterminal):
    def __init__(self, id: 'DottedIdNode', arg_list: Optional[ModuleArgumentListNode]) -> None:
        super().__init__(NodeKind.MODULE_SPECIFIER, id.location)
        self.id: 'DottedIdNode' = id
        self.arg_list: Optional[ModuleArgumentListNode] = arg_list

class AsClauseNode(Nonterminal):
    def __init__(self, as_token: UniqueToken, id_token: IdToken):
        super().__init__(NodeKind.AS_CLAUSE, as_token.location)
        self.as_token: UniqueToken = as_token
        self.id_token: IdToken = id_token

class ModuleImportNode(Nonterminal):
    def __init__(self, private_token: Optional[UniqueToken], import_token: UniqueToken,
        module: ModuleSpecifierNode, as_clause: Optional[AsClauseNode]):

        super().__init__(NodeKind.MODULE_IMPORT, private_token.location if not private_token is None else import_token.location)
        self.private_token: Optional[UniqueToken] = private_token
        self.import_token: UniqueToken = import_token
        self.module: ModuleSpecifierNode = module
        self.as_clause: Optional[AsClauseNode] = as_clause

class DottedIdNode(Nonterminal):
    def __init__(self, components: List[IdToken]):
        super().__init__(NodeKind.DOTTED_ID, components[0].location)
        self.components: List[IdToken] = components
        self.dotted_id: str = '.'.join([id_token.text for id_token in components])

class Parser:
    def __init__(self, line: str, file: str, line_number: int):
        self.token_stream = TokenStream(line, file, line_number)

    def parse(self) -> Node:
        peek = self.token_stream.peek()
        if peek.kind == NodeKind.PRIVATE_KEYWORD or peek.kind == NodeKind.IMPORT_KEYWORD:
            return self.parse_module_import()
        if peek.kind == NodeKind.MODULE_KEYWORD:
            return self.parse_module_declaration()
        emit_unexpected_token_error(peek)

    def optional(self, kind: NodeKind) -> Optional[Token]:
        peek = self.token_stream.peek()
        if peek.kind == kind:
            return self.token_stream.read()
        else:
            return None

    def optional_unique(self, kind: NodeKind) -> Optional[UniqueToken]:
        return cast(Optional[UniqueToken], self.optional(kind))

    def expect(self, kind: NodeKind) -> Token:
        peek = self.token_stream.peek()
        if peek.kind == kind:
            return self.token_stream.read()
        else:
            emit_unexpected_token_error(peek)
    
    def expect_unique(self, kind: NodeKind) -> UniqueToken:
        return cast(UniqueToken, self.expect(kind))

    def parse_module_import(self) -> ModuleImportNode:
        # ModuleImport : private? import ModuleSpecifier AsClause?
        private_token: Optional[UniqueToken] = cast(Optional[UniqueToken], self.optional(NodeKind.PRIVATE_KEYWORD))
        import_token: UniqueToken = cast(UniqueToken, self.expect(NodeKind.IMPORT_KEYWORD))
        module: ModuleSpecifierNode = self.parse_module_specifier()
        as_clause: Optional[AsClauseNode] = self.parse_optional_as_clause()

        return ModuleImportNode(private_token, import_token, module, as_clause)

    def parse_module_specifier(self) -> ModuleSpecifierNode:
        # ModuleSpecifier : DottedId ModuleArgumentList?
        id: DottedIdNode = self.parse_dotted_id()
        arg_list: Optional[ModuleArgumentListNode] = self.parse_optional_module_argument_list()

        return ModuleSpecifierNode(id, arg_list)

    def parse_module_argument_list(self) -> ModuleArgumentListNode:
        # ModuleArgumentList : < (ModuleSpecifier ,)* ModuleSpecifier >
        open_angle_token = self.expect_unique(NodeKind.OPEN_ANGLE)
        args: List[ModuleSpecifierNode] = self.parse_module_specifier_list()
        close_angle_token = self.expect_unique(NodeKind.CLOSE_ANGLE)

        return ModuleArgumentListNode(open_angle_token, args, close_angle_token)

    def parse_optional_module_argument_list(self) -> Optional[ModuleArgumentListNode]:
        if self.token_stream.peek().kind == NodeKind.OPEN_ANGLE:
            return self.parse_module_argument_list()
        else:
            return None

    def parse_module_specifier_list(self) -> List[ModuleSpecifierNode]:
        args: List[ModuleSpecifierNode] = []
        while True:
            args.append(self.parse_module_specifier())
            comma = self.optional(NodeKind.COMMA)
            if not comma:
                return args

    def parse_optional_as_clause(self) -> Optional[AsClauseNode]:
        as_token = self.optional_unique(NodeKind.AS_KEYWORD)
        if as_token:
            id_token = cast(IdToken, self.expect(NodeKind.ID))
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
            components.append(cast(IdToken, self.expect(NodeKind.ID)))
            dot = self.optional(NodeKind.DOT)
            if not dot:
                return DottedIdNode(components)

    def parse_module_parameter(self) -> ModuleParameterNode:
        return ModuleParameterNode(cast(IdToken, self.expect(NodeKind.ID)))

    def parse_module_declaration(self) -> ModuleDeclarationNode:
        module_token = self.expect_unique(NodeKind.MODULE_KEYWORD)
        open_angle_token = self.expect_unique(NodeKind.OPEN_ANGLE)

        parameters = self.parse_module_parameter_list()

        close_angle_token = self.expect_unique(NodeKind.CLOSE_ANGLE)
        self.expect(NodeKind.EOL)

        return ModuleDeclarationNode(module_token, open_angle_token, parameters, close_angle_token)

class TemplateHandler:
    def on_source_line(self, line: str, location: Location) -> None:
        pass

    def on_module_declaration_directive(self, line: str, location: Location, decl: ModuleDeclarationNode) -> None:
        pass

    def on_module_import_directive(self, line: str, location: Location, imp: ModuleImportNode) -> None:
        pass

    def on_instantiation_comment(self, line: str, location: Location, template: Tuple[str, str]) -> None:
        pass

    def on_begin_generated_code(self, line: str, location: Location) -> None:
        pass

    def on_end_generated_code(self, line: str, location: Location) -> None:
        pass

    def on_generated_code(self, line: str, location: Location) -> None:
        pass

    def on_end_of_file(self) -> None:
        pass

begin_generated_line_re = re.compile('^///\{\{')
end_generated_line_re = re.compile('^///\}\}')

template_directive_re = re.compile('^(///#)(.*)$')
instantiation_comment_re = re.compile('^(?:///!)(?:.*)(?P<template_pack>[A-Za-z0-9-_]+):(?P<template_module>[A-Za-z0-9_]+)(?:.*)$')

def parse_source_file(file_path: str, handler: TemplateHandler) -> None:
    with open(file_path, 'r', encoding='utf-8') as file:
        in_generated_code: bool = False
        line_number: int = 1

        for line in file:
            location: Location = Location(file_path, line_number, (1, len(line)))
            if in_generated_code:
                if end_generated_line_re.match(line):
                    handler.on_end_generated_code(line, location)
                    in_generated_code = False
                else:
                    handler.on_generated_code(line, location)
            else:
                if begin_generated_line_re.match(line):
                    handler.on_begin_generated_code(line, location)
                    in_generated_code = True
                else:
                    capture = template_directive_re.match(line)
                    if capture:
                        parser: Parser = Parser(capture.group(2), file_path, line_number)
                        node: Node = parser.parse()
                        if node.kind == NodeKind.MODULE_DECLARATION:
                            handler.on_module_declaration_directive(line, location, cast(ModuleDeclarationNode, node))
                        elif node.kind == NodeKind.MODULE_IMPORT:
                            handler.on_module_import_directive(line, location, cast(ModuleImportNode, node))
                    else:
                        capture = instantiation_comment_re.match(line)
                        if capture:
                            handler.on_instantiation_comment(line, location, (capture.group('template_pack'), capture.group('template_module')))
                        else:
                            handler.on_source_line(line, location)
            line_number += 1
