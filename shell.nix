let
  pkgs = (import ./nix).pkgs;
  devPackages = (import ./nix).devPackages;
in
pkgs.mkShell {
  name = "wire-server-direnv";
  buildInputs = devPackages;
}
