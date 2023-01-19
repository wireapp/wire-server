{ pkgs ? import <nixpkgs> { } }:
(pkgs.buildFHSUserEnv {
  name = "pipzone";
  targetPkgs = pkgs: (with pkgs; [
    python3
    python3Packages.pip
    python3Packages.virtualenv
  ]);
  runScript = "bash";
}).env

# then
# virtualenv venv
# pip install rst-to-myst
# Fix this bug locally: https://github.com/executablebooks/rst-to-myst/issues/49
# pip install sphinx-reredirects
# pip install sphinx-multiversion
