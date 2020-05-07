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

