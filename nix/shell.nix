with (import ./.);
mkShell {
  name = "niv";
  buildInputs = [ niv ];
}
