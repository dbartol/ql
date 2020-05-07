#!/usr/bin/env python3

from enum import Enum, IntEnum
import glob
import json
import os
import argparse
import os
from os import path
import re
from ruamel.yaml import YAML
import shutil
import sys
import tempfile
from typing import Callable, Dict, IO, List, Tuple

from location import Location
from packages import Package, Workspace
import template_parser
from template_parser import ModuleDeclarationNode, ModuleImportNode, TemplateHandler, parse_source_file

yaml = YAML(typ='safe')

def chdir_repo_root():
    root_path = path.join(path.dirname(path.abspath(__file__)), '..')
    os.chdir(root_path)

chdir_repo_root()

print(os.getcwd())

with open('config/template-config.yml', 'r', encoding='utf-8') as template_config_file:
    template_config = yaml.load(template_config_file)

temp_root = tempfile.mkdtemp('pyram')
temp_root = 'C:/Src/Repro/pyram'

if os.path.exists(temp_root):
    print(f"Cleaning temporary directory '{temp_root}'...'")
    shutil.rmtree(temp_root)

workspace: Workspace = Workspace(os.getcwd(), temp_root, template_config['packages'])

workspace.connect_instantiation_files()
workspace.sync()
workspace.instantiate()

workspace.delete_instantiations()

workspace.commit()
