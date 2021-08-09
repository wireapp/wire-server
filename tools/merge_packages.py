import os
import re
from toposort import toposort_flatten
import sys
import yaml

def read_yaml(p):
    with open(p, 'r') as f:
        return yaml.safe_load(f)

def write_yaml(obj, p):
    with open(p, 'w') as f:
        f.write(yaml.dump(obj))

def parse_simple_dep(s):
    m = re.match(r'(^[^\s]+)', s)
    dep, = m.groups()
    return dep

class Component:
    def __init__(self, ctype, name, component):
        self.ctype = ctype
        self.name = name
        self.dependencies = set(parse_simple_dep(d) 
            for d in component.get('dependencies', []))
        self.meta = dict((key, value)
            for key, value in component.items()
            if key != 'dependencies')

    @property
    def source_dirs(self):
        dirs = self.meta.get('source-dirs', [])
        if isinstance(dirs, str):
            dirs = [dirs]
        return dirs

    @source_dirs.setter
    def source_dirs(self, value):
        self.meta['source-dirs'] = value

    def to_yaml(self):
        return dict(dependencies=list(self.dependencies),
            **self.meta)

class Package:
    def __init__(self, directory, package):
        self.directory = directory
        self.name = package['name']
        self.dependencies = set(parse_simple_dep(d) 
            for d in package.get('dependencies', []))
        libs = [package['library']] if 'library' in package else [{}]
        self.libraries = [Component('lib', None, lib) for lib in libs]
        self.executables = [Component('exe', name, exe) \
            for name, exe in package.get('executables', {}).items()]
        self.tests = [Component('test', name, test) \
            for name, test in package.get('tests', {}).items()]
        self.flags = package.get('flags', {})

        main_libs = [lib for lib in self.libraries if lib.name is None]
        if len(main_libs) == 1:
            self.main = main_libs[0]
        else:
            self.main = None

    @classmethod
    def load(cls, directory):
        with open(os.path.join(directory, 'package.yaml')) as f:
            return cls(directory, yaml.safe_load(f))

    @property
    def components(self):
        return self.libraries + self.executables + self.tests

    @property
    def all_dependencies(self):
        return self.dependencies.union(
            dep for component in self.components
                for dep in component.dependencies)

    def merge(self, other, internal):
        if self.main is None:
            raise ValueError("can only merge into package with a main library")

        # add flags
        for name, flag in other.flags.items():
            if name not in self.flags:
                self.flags[name] = flag

        # add library source directory
        for lib in other.libraries:
            self.main.source_dirs += [os.path.join(other.directory, d)
                for d in lib.source_dirs]

        # add global and library dependencies
        self.dependencies |= other.dependencies - internal
        for lib in other.libraries:
            self.dependencies |= lib.dependencies - internal

        def process_exe(exe):
            exe.dependencies -= internal
            exe.dependencies.add(self.name)
            if 'main' in exe.meta and len(exe.source_dirs) == 0:
                # if there are no source dirs, the path for the main module is
                # relative to the package root
                exe.meta['main'] = os.path.join(other.directory,
                    exe.meta['main'])
            exe.source_dirs = [os.path.join(other.directory, d)
                for d in exe.source_dirs]

        # add executables
        exe_names = set(exe.name for exe in self.executables)
        for exe in other.executables:
            if exe.name in exe_names:
                raise ValueError(f"conflicting executable: {exe.name}")
            process_exe(exe)
            self.executables.append(exe)

        # add tests
        test_names = set(test.name for test in self.tests)
        for test in other.tests:
            if test.name in test_names:
                name = other.name + "-" + test.name
                if name in test_names:
                    raise ValueError("conflict test: {test.name} and {name} both exist")
                warning(f"renaming conflicting test {test.name} to {name}")
                test.name = name
            process_exe(test)
            self.tests.append(test)

    def to_yaml(self):
        package = {
            'dependencies': list(self.dependencies),
            'executables' : dict((exe.name, exe.to_yaml()) for
                exe in self.executables),
            'tests' : dict((test.name, test.to_yaml()) for
                test in self.tests),
            'flags' : self.flags
        }

        for lib in self.libraries:
            if lib.name is not None:
                raise ValueError(f"named library in package {self.name}")
            package['library'] = lib.to_yaml()

        return package

GREEN = '\033[32m'
RED = '\033[31m'
YELLOW = '\033[33m'

def coloured(col, s):
    return f'{col}{s}\033[0m'

def info(s):
    print(coloured(GREEN, s), file=sys.stderr)

def warning(s):
    print(coloured(YELLOW, s), file=sys.stderr)

def error(s):
    print(coloured(RED, s), file=sys.stderr)

def read_dep_dag(exceptions):
    '''
    Returns all internal packages in topological order w.r.t to dependency
    '''
    dag = {}
    packages = {}
    for package_dir in read_yaml('stack_split.yaml')['packages']:
        try:
            package = Package.load(package_dir)
        except FileNotFoundError:
            warning(f'{package_dir} is not a haskell package')
            continue
        if package.name in exceptions:
            warning(f'skipping {package.name}')
            continue
        dag[package.name] = package.all_dependencies
        packages[package.name] = package

    ordered = list(toposort_flatten(dag))
    return [packages[name] for name in ordered if name in packages]

if __name__ == '__main__':
    try:
        packages = read_dep_dag(exceptions=['sodium-crypto-sign',
                                            'wire-message-proto-lens',
                                            'types-common-journal-proto'])

        internal_packages = set(package.name for package in packages)
        root = Package('.', {'name': 'wire-server'})

        for package in packages:
            info(f'merging {package.name}')
            root.merge(package, internal_packages)

        # output root package
        with open('package_start.yaml') as f:
            print(f.read())
        yaml.dump(root.to_yaml(), sys.stdout)

    except Exception as e:
        error(str(e))
        raise(e)
