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
    '''
    Returns all files relative to `dir_` except for those that are excluded (see file_is_excluded)
    '''
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
    '''
    add_suffix(README.md, 'wire-api') -> README-wire-api.md
    '''
    return path + '-' + suffix # TODO

# def copy_with_dir(relpath, src_dir, dst_dir, suffix=None):
#     src_path = os.path.join(src_dir, relpath)
#     dst_path = os.path.join(dst_dir, relpath)
#     if suffix is not None:
#         dst_path = add_suffix(dst_path, suffix)
#     os.makedirs(os.path.dirname(dst_path), exist_ok=True)
#     if os.path.exists(dst_path):
#         raise ValueError(f'{dst_path} already exists')
#     shutil.copyfile(src_path, dst_path)

def get_path(obj, path):
    '''
    get value of a nested structure (e.g. parsed YAML)
    '''
    o = obj
    while(len(path) > 0):
        o = o.get(path[0])
        if o is None:
            return None
        path = path[1:]
    return o

def get_list(obj, path):
    s = get_path(obj, path)
    if s is not None:
        if type(s) is list:
            return s
        elif isinstance(s, str):
            return [s]
        else:
            raise ValueError(f'dont know how to hande {s}')
    else:
        return None

def get_dependencies(obj, path, remove=[]):
    deps = set(get_path(obj, path + ['dependencies']) or [])
    return list(deps - set(remove))

def merge_projects(dir_source, dir_target):

    package_source = read_yaml(os.path.join(dir_source, 'package.yaml'))
    package_target = read_yaml(os.path.join(dir_target, 'package.yaml'))

    prefix = package_source['name']

    # TODO: remove  from all deps

    package_target['dependencies'] = \
        merge_deps(package_target['dependencies'],
                   get_dependencies(package_source, [], remove=[package_source['name']]))

    package_target['library']['dependencies'] = \
        merge_deps(package_target['library']['dependencies'], get_dependencies(package_source, ['library'], remove=[package_source['name']]))

    executables = list(package_source.get('executables', {}).items()) + \
        list(package_source.get('tests', {}).items())

    for name, exe in executables:
        if 'source-dirs' in exe:
            exe['source-dirs'] = [os.path.join(prefix, d)
                for d in get_list(exe, ['source-dirs'])]
        else:
            exe['main'] = os.path.join(prefix, exe['main'])
        if 'executables' not in package_target:
            package_target['executables'] = {}
        package_target['executables'][name] = exe

    for name, flag in package_source.get('flags', {}).items():
        if 'flags' not in package_target:
            package_target['flags'] = {}
        if name in package_target['flags']:
            raise ValueError(f"duplicated flag {name}")
        package_target['flags'][name] = flag

    prefixed_source_dirs = [os.path.join(prefix, d) for d in get_list(package_source, ['library', 'source-dirs'])]
    package_target['library']['source-dirs'].extend(prefixed_source_dirs)

    # TODO: executables
    # TODO: tests

    package_source_name = package_source['name']

    # for fname in (list_source_files(dir_source)):
    #     suffix=None
    #     if fname in ['README.md', 'Makefile']:
    #         suffix = package_source_name

    shutil.copytree(dir_source, os.path.join(dir_target, prefix),
        ignore=shutil.ignore_patterns('.stack-*', 'dist', 'deb', 'deb-*'))

    write_yaml(package_target, os.path.join(dir_target, 'package.yaml'))


def main():
    os.makedirs('wire-server', exist_ok=True)
    shutil.copyfile('package_start.yaml', 'wire-server/package.yaml')

    packages_topo_order = ['services/brig', 'services/galley']

    for package in packages_topo_order:
        merge_projects(package, 'wire-server')

if __name__ == '__main__':
    main()
