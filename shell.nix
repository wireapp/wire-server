let
  pkgs = import ./nix;
  packages = import ./dev-packages.nix { pkgs = pkgs; };
in
pkgs.mkShell {
  name = "wire-server-direnv";
  buildInputs = packages; 
}
