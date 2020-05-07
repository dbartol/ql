import glob
import hashlib
import os
import shutil
from typing import Callable, Dict, IO, List, Optional, Tuple, cast
from ruamel.yaml import YAML

import diagnostics
from diagnostics import Diagnostic, Severity
from location import Location
import template_parser
from template_parser import DottedIdNode, ModuleDeclarationNode, ModuleImportNode, ModuleSpecifierNode, TemplateHandler, parse_source_file

yaml = YAML(typ='safe')

multiple_module_declarations_error = Diagnostic(Severity.ERROR, "Multiple module declarations in source file.")
module_declaration_after_imports_error = Diagnostic(Severity.ERROR, "Module declaration must precede any module imports.")
template_module_must_be_file_module_error = Diagnostic(Severity.ERROR, "Only file modules can have arguments.")
multiple_instantiation_comments_error = Diagnostic(Severity.ERROR, "Multiple instantiation comments in source file.")
dependent_package_not_found_error = Diagnostic(Severity.ERROR, "Dependent package not found.")
module_not_found_error = Diagnostic(Severity.ERROR, "Module not found.")
module_is_not_a_template_error = Diagnostic(Severity.ERROR, "Module is not a template.")
wrong_number_of_template_arguments_error = Diagnostic(Severity.ERROR, "Wrong number of template arguments.")
instantiated_module_must_be_file_module_error = Diagnostic(Severity.ERROR, "Instantiated module must be file module.")

def path_to_dotted_name(file_path: str) -> str:
    return os.path.splitext(file_path)[0].replace('\\', '/').replace('/', '.')

def transform_file(input_path: str, output_path: str, op: Callable[[IO[str]], TemplateHandler]) -> None:
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    with open(output_path, 'w', encoding='utf-8') as output_file:
        handler = op(output_file)
        parse_source_file(input_path, handler)

class StripHandler(TemplateHandler):
    def __init__(self, output_file: IO[str]) -> None:
        self.output_file = output_file

    def on_source_line(self, line: str, location: Location) -> None:
        self.output_file.write(line)

    def on_module_declaration_directive(self, line: str, location: Location, decl: ModuleDeclarationNode) -> None:
        self.output_file.write(line)

    def on_module_import_directive(self, line: str, location: Location, imp: ModuleImportNode) -> None:
        self.output_file.write(line)

    def on_instantiation_comment(self, line: str, location: Location, template: Tuple[str, str]) -> None:
        pass

    def on_begin_generated_code(self, line: str, location: Location) -> None:
        pass

    def on_end_generated_code(self, line: str, location: Location) -> None:
        pass

    def on_generated_code(self, line: str, location: Location) -> None:
        pass

class ModuleReference:
    def __init__(self, location: Location) -> None:
        self.location: Location = location

class FileModuleReference(ModuleReference):
    def __init__(self, location: Location, source_file: 'SourceFile') -> None:
        super().__init__(location)
        self.source_file: SourceFile = source_file

class InstantiatedModuleReference(ModuleReference):
    def __init__(self, location: Location, template: FileModuleReference, arguments: List[ModuleReference]) -> None:
        super().__init__(location)
        self.template: FileModuleReference = template
        self.arguments: List[ModuleReference] = arguments

class ModuleParameterReference(ModuleReference):
    def __init__(self, location: Location, id: str) -> None:
        super().__init__(location)
        self.id: str = id

class InstantiationHandler(TemplateHandler):
    def __init__(self, output_file: IO[str], source_file: 'SourceFile', context: 'Package', module_arguments: List['SourceFile']) -> None:
        self.output_file: IO[str] = output_file
        self.source_file: SourceFile = source_file
        self.context = context
        self.module_arguments = module_arguments

    def on_source_line(self, line: str, location: Location) -> None:
        self.output_file.write(line)

    def on_module_declaration_directive(self, line: str, location: Location, decl: ModuleDeclarationNode) -> None:
        # Generate the instantiation comment.
        self.output_file.write(f"///! {self.source_file.package.id}:{self.source_file.name}\n")

        # Copy the original module directive.
        self.output_file.write(line)

        # Generate the imports for the module parameters.
        self.output_file.write('///{\n')
        for (parameter, argument) in zip(self.source_file.module_parameters, self.module_arguments):
            self.output_file.write(f"private import {argument.name} as {parameter}\n")
        self.output_file.write('///}\n')

    def on_module_import_directive(self, line: str, location: Location, imp: ModuleImportNode) -> None:
        self.output_file.write(line)
        self.output_file.write('///{\n')
        if not imp.private_token is None:
            self.output_file.write('private ')
        module_reference: ModuleReference = self.__bind_module_reference(imp.module)
        bound_module: FileModuleReference = self.__instantiate_module_reference(module_reference)
        self.output_file.write(f"import {bound_module.source_file.name}\n")
        self.output_file.write('')
        self.output_file.write('///}\n')

    def on_instantiation_comment(self, line: str, location: Location, template: Tuple[str, str]) -> None:
        pass

    def on_begin_generated_code(self, line: str, location: Location) -> None:
        pass

    def on_end_generated_code(self, line: str, location: Location) -> None:
        pass

    def on_generated_code(self, line: str, location: Location) -> None:
        pass

    def __bind_module_reference(self, module_node: ModuleSpecifierNode) -> ModuleReference:
        if module_node.arg_list is None:
            return self.__bind_simple_module_reference(module_node.id)
        else:
            return self.__bind_instantiated_module_reference(module_node.id, module_node.arg_list.args)

    def __bind_module_argument(self, module_node: ModuleSpecifierNode) -> 'SourceFile':
        module_reference = self.__instantiate_module_reference(self.__bind_module_reference(module_node))
        if isinstance(module_reference, FileModuleReference):
            return module_reference.source_file
        else:
            diagnostics.emit_error(instantiated_module_must_be_file_module_error, module_node.location)

    def __bind_instantiated_module_reference(self, id: DottedIdNode, args: List[ModuleSpecifierNode]) -> ModuleReference:
        template_reference = self.__bind_simple_module_reference(id)
        if isinstance(template_reference, FileModuleReference):
            template = template_reference.source_file
            if len(template.module_parameters) == 0:
                diagnostics.emit_error(module_is_not_a_template_error, id.location)
            elif len(template.module_parameters) != len(args):
                diagnostics.emit_error(wrong_number_of_template_arguments_error, id.location)
            else:
                instantiation = self.context._instantiate_module(template, [self.__bind_module_argument(node) for node in args])
                return FileModuleReference(id.location, instantiation)
        else:
            diagnostics.emit_error(instantiated_module_must_be_file_module_error, id.location)


    def __bind_simple_module_reference(self, id: DottedIdNode) -> ModuleReference:
        current_directory_id: str
        last_dot_index: int = self.source_file.name.rfind('.')
        referenced_module: Optional[SourceFile]

        # Resolve as a module parameter.
        if len(id.components) == 1:
            if self.source_file.module_parameters.count(id.dotted_id) > 0:
                return ModuleParameterReference(id.location, id.dotted_id)

        if last_dot_index >= 0:
            # Resolve relative to the directory containing the current module.
            relative_to_current_directory = f"{self.source_file.name[0:last_dot_index]}.{id.dotted_id}"
            referenced_module = self.source_file.package.find_module(relative_to_current_directory)
            if not referenced_module is None:
                return FileModuleReference(id.location, referenced_module)

        # Resolve relative to the root of the current package.
        referenced_module = self.source_file.package.find_module(id.dotted_id)
        if not referenced_module is None:
            return FileModuleReference(id.location, referenced_module)

        # Resolve relative to the root of each referenced package.
        for dependency in self.source_file.package.dependencies.values():
            referenced_module = dependency.find_module(id.dotted_id)
            if not referenced_module is None:
                return FileModuleReference(id.location, referenced_module)

        diagnostics.emit_error(module_not_found_error, id.location)

    def __instantiate_module_reference(self, ref: ModuleReference) -> FileModuleReference:
        if isinstance(ref, FileModuleReference):
            return ref
        elif isinstance(ref, ModuleParameterReference):
            return FileModuleReference(ref.location, self.module_arguments[self.source_file.module_parameters.index(ref.id)])
        elif isinstance(ref, InstantiatedModuleReference):
            raise Exception('Unimplemented')
        else:
            raise Exception('Bad')

class SourceFile:
    def __init__(self, package: 'Package', relative_path: str, file_path: str, module_parameters: List[str],
        module_arguments: List['SourceFile'], has_import_directives: bool, template: Optional['SourceFile'] = None) -> None:

        self.package: 'Package' = package
        self.name: str = path_to_dotted_name(relative_path)
        self.relative_path: str = relative_path
        self.file_path: str = file_path
        self.module_parameters: List[str] = module_parameters
        self.module_arguments: List[SourceFile] = module_arguments
        self.has_import_directives: bool = has_import_directives
        self.temp_file: str = os.path.join(package.temp_dir, relative_path)
        self.template = template
        self.instantiation_files: List[str] = []
        self.generated_code: bool = False
        
    def add_instantiation_files(self, files: List[str]) -> None:
        self.instantiation_files.extend(files)

    def sync(self) -> None:
        if self.is_template_definition():
            print(f"Syncing template definition '{self.file_path}' to '{self.temp_file}'...'")
            transform_file(self.file_path, self.temp_file, lambda output_file: StripHandler(output_file))

    def is_template_definition(self) -> bool:
        return len(self.module_parameters) > 0 and len(self.module_arguments) == 0

    def generate(self) -> None:
        if self.has_import_directives and not self.is_template_definition():
            self.generate_instantiation()
    
    def generate_instantiation(self) -> None:
        input_source_file: SourceFile
        input_file_path: str
        if self.template is None:
            input_source_file = self
            input_file_path = self.file_path
        else:
            input_source_file = self.template
            input_file_path = self.template.temp_file

        print(f"Generating instantiation of '{input_source_file.file_path}' with argument list '{self.module_arguments}'...")

        transform_file(input_file_path, self.temp_file, lambda output_file: InstantiationHandler(output_file, input_source_file, self.package, self.module_arguments))

        self.generated_code = True

    def commit(self) -> None:
        if self.generated_code:
            dest_dir: str = os.path.dirname(self.file_path)
            if not os.path.exists(dest_dir):
                os.makedirs(dest_dir)
            shutil.copy(self.temp_file, self.file_path)

class Package:
    def __init__(self, workspace: 'Workspace', path: str) -> None:
        self.workspace: 'Workspace' = workspace
        self.path: str = path

        self.qlpack_yml_path: str = os.path.join(path, 'qlpack.yml')
        with open(self.qlpack_yml_path, 'r', encoding='utf-8') as qlpack_yml:
            self.pack_definition = yaml.load(qlpack_yml)
        self.id: str = self.pack_definition['name']
        self.temp_dir: str = os.path.join(workspace.temp_root, self.id)
        self.dependencies: Dict[str, Package] = {}
        file_paths: List[str] = glob.glob(os.path.join(path, '**/*.qll'), recursive=True)
        self.source_files: Dict[str, SourceFile] = {}
        for file_path in file_paths:
            print(f"Scanning file '{file_path}'...")
            relative_path: str = file_path[len(path) + 1:].replace('\\', '/')
            handler: ScanHandler = ScanHandler()
            template_parser.parse_source_file(file_path, handler)
            if handler.template is None:
                if len(handler.module_parameters) > 0:
                    print(f"Defines template with {len(handler.module_parameters)} parameters.")
                source_file: SourceFile = SourceFile(self, relative_path, file_path,
                    handler.module_parameters, [], handler.has_import_directives)
                self.source_files[source_file.name] = source_file
            else:
                print(f"Instantiation of template '{handler.template}'.")
                self.workspace.add_instantiation_file(handler.template, file_path)

        # Map from (package, template, arg_hash) to the instantiation
        self.instantiations: Dict[Tuple[str, str, str], SourceFile] = {}
        self.instantiation_stack: List[SourceFile] = []

    def find_module(self, name: str) -> Optional[SourceFile]:
        return self.source_files.get(name)

    def sync(self) -> None:
        for source_file in self.source_files.values():
            source_file.sync()

    def resolve_dependencies(self) -> None:
        dependenciesNode = self.pack_definition.get('libraryPathDependencies', [])
        dependencies: List[str] = [dependenciesNode] if isinstance(dependenciesNode, str) else dependenciesNode
        print(dependencies)
        for dependencyId in dependencies:
            dependencyPackage: Optional[Package] = self.workspace.find_package(dependencyId)
            if dependencyPackage is None:
                diagnostics.emit_error(dependent_package_not_found_error, Location(self.qlpack_yml_path, 1, (0, 0)))
            self.dependencies[dependencyId] = dependencyPackage

    def instantiate_imports(self) -> None:
        for source_file in self.source_files.values():
            source_file.generate()
        while len(self.instantiation_stack) > 0:
            self.instantiation_stack.pop().generate_instantiation()

    def commit(self) -> None:
        for source_file in self.source_files.values():
            source_file.commit()
        for source_file in self.instantiations.values():
            source_file.commit()

    def _instantiate_module(self, template: SourceFile, args: List[SourceFile]) -> SourceFile:
        hash = hashlib.sha256()
        for arg in args:
            component: str = f"|{arg.package.id}:{arg.name}"
            hash.update(component.encode('utf-8'))
        hash_digest: str = hash.hexdigest()[0:8]
        key = (template.package.id, template.name, hash_digest)
        instantiation = self.instantiations.get(key)
        if instantiation is None:
            relative_path: str = f"internal/inst/{template.package.id.replace('-', '_')}/{os.path.dirname(template.relative_path)}/H{hash_digest}/{os.path.basename(template.relative_path)}"
            instantiation = SourceFile(self, relative_path, os.path.join(self.path, relative_path), template.module_parameters,
                args, template.has_import_directives, template)
            self.instantiations[key] = instantiation
            self.instantiation_stack.append(instantiation)
        return instantiation

class ScanHandler(TemplateHandler):
    def __init__(self) -> None:
        self.module_parameters: List[str] = []
        self.has_import_directives: bool = False
        self.has_instantiation_comment: bool = False
        self.template: Optional[Tuple[str, str]] = None

    def on_source_line(self, line: str, location: Location) -> None:
        pass

    def on_module_declaration_directive(self, line: str, location: Location, decl: ModuleDeclarationNode) -> None:
        if len(self.module_parameters) == 0:
            if self.has_import_directives:
                diagnostics.emit_error(module_declaration_after_imports_error, location)
            else:
                self.module_parameters = [param.id_token.text for param in decl.parameters]
        else:
            diagnostics.emit_error(multiple_module_declarations_error, location)

    def on_module_import_directive(self, line: str, location: Location, imp: ModuleImportNode) -> None:
        self.has_import_directives = True

    def on_instantiation_comment(self, line: str, location: Location, template: Tuple[str, str]) -> None:
        if self.template is None:
            self.template = template
        else:
            diagnostics.emit_error(multiple_instantiation_comments_error, location)

    def on_begin_generated_code(self, line: str, location: Location) -> None:
        pass

    def on_end_generated_code(self, line: str, location: Location) -> None:
        pass

    def on_generated_code(self, line: str, location: Location) -> None:
        pass

    def resolve_module_id(self, id: DottedIdNode) -> ModuleReference:
        pass

class Workspace:
    def __init__(self, root: str, temp_root: str, package_directories: List[str]) -> None:
        self.root: str = root
        self.temp_root: str = temp_root
        self.packages: Dict[str, Package] = {}
        self.instantiation_files: Dict[Tuple[str, str], List[str]] = {}
        for package_directory in package_directories:
            package: Package = Package(self, package_directory)
            self.packages[package.id] = package

        self.__resolve_package_dependencies()

    def add_instantiation_file(self, template: Tuple[str, str], file_path: str) -> None:
        files: List[str] = self.instantiation_files.get(template, [])
        files.append(file_path)

    def connect_instantiation_files(self) -> None:
        for (template, files) in self.instantiation_files.items():
            package: Optional[Package] = self.packages.get(template[0])
            if not package is None:
                module: Optional[SourceFile] = package.find_module(template[1])
                if not module is None:
                    module.add_instantiation_files(files)

    def find_package(self, id: str) -> Optional[Package]:
        return self.packages.get(id)

    def __resolve_package_dependencies(self) -> None:
        for package in self.packages.values():
            package.resolve_dependencies()

    def sync(self) -> None:
        for package in self.packages.values():
            package.sync()

    def instantiate(self) -> None:
        for package in self.packages.values():
            package.instantiate_imports()

    def delete_instantiations(self) -> None:
        for file_paths in self.instantiation_files.values():
            for file_path in file_paths:
                print(f"Deleting instantiation file '{file_path}'...")
                os.remove(file_path)

    def commit(self) -> None:
        for package in self.packages.values():
            package.commit()
