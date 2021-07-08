#!/usr/bin/env python3
import sys
import os
import yaml
import re
import shutil

def read_yaml(p):
    with open(p, 'r') as f:
        return yaml.safe_load(f.read())

def write_yaml(obj, p):
    with open(p, 'w') as f:
        f.write(yaml.dump(obj))

def parse_simple_dep(s):
    m = re.match(r'(^[^\s]+)', s)
    dep, = m.groups()
    return dep

def tests():
    assert 'base' == parse_simple_dep('base >=4.6 && <5.0')
    assert 'base64-bytestring' == parse_simple_dep('base64-bytestring >=1.0')

def merge_deps(deps_left, deps_right):
    s = set()
    for dep in (deps_left + deps_right):
        s.add(parse_simple_dep(dep))
    return list(sorted(s))

def r(s):
    return re.compile(s)

patterns_file_exclude = [r(r'package.yaml'), r(r'LICENSE'), r(r'\.cabal'), r(r'Setup\.hs')]

def file_is_excluded(filename):
    for p in patterns_file_exclude:
        if re.search(p, filename):
            return True
    return False

def list_source_files(dir_):
    result = []
    for root, sub_folders, filenames in os.walk(dir_):
        if '/.' in root:
            continue
        for filename in filenames:
            if file_is_excluded(filename):
                continue
            result.append(os.path.relpath(os.path.join(root, filename), dir_))
    return result

def add_suffix(path, suffix):
    return path + '-' + suffix # TODO

def copy_with_dir(relpath, src_dir, dst_dir, suffix=None):
    src_path = os.path.join(src_dir, relpath)
    dst_path = os.path.join(dst_dir, relpath)
    if suffix is not None:
        dst_path = add_suffix(dst_path, suffix)
    os.makedirs(os.path.dirname(dst_path), exist_ok=True)
    shutil.copyfile(src_path, dst_path)
    # print(f'{src_path} -> {dst_path}')

def get_path(obj, path):
    o = obj
    while(len(path) > 0):
        o = o.get(path[0])
        if o is None:
            return None
        path = path[1:]
    return o

def merge_projects(dir_source, dir_target):

    package_source = read_yaml(os.path.join(dir_source, 'package.yaml'))
    package_target = read_yaml(os.path.join(dir_target, 'package.yaml'))

    package_target['dependencies'] = merge_deps(package_target['dependencies'], get_path(package_source, ['dependencies']) or [])
    package_target['library']['dependencies'] = merge_deps(package_target['library']['dependencies'], get_path(package_source, ['library', 'dependencies']) or [])

    # TODO: executables
    # TODO: tests

    package_source_name = package_source['name']

    for fname in (list_source_files(dir_source)):
        suffix=None
        if fname in ['README.md', 'Makefile']:
            suffix = package_source_name
        copy_with_dir(fname, dir_source, dir_target, suffix=suffix)

    write_yaml(package_target, os.path.join(dir_target, 'package.yaml'))


def get_lib_dirs():
    result = []
    for root, sub_folders, _ in os.walk('libs'):
        if root != 'libs':
            continue
        for sub_folder in sub_folders:
            lib_dir = os.path.join(root, sub_folder)
            result.append(lib_dir)
    return result

def main():
    os.makedirs('wire-server', exist_ok=True)
    shutil.copyfile('package_start.yaml', 'wire-server/package.yaml')
    for lib_dir in get_lib_dirs():
        if lib_dir in ['libs/hscim',
                       'libs/libzauth',
                       'libs/brig-types',
                       ]:
            continue
        print(f'Merging {lib_dir}')
        merge_projects(lib_dir, 'wire-server')

if __name__ == '__main__':
    main()
