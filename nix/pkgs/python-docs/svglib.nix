{
  buildPythonPackage,
  fetchPypi,

  reportlab,
  lxml,
  cssselect2,
  pillow,

  # pytest-runner
}:
buildPythonPackage rec {
  pname = "svglib";
  version = "1.0.1";
  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-/wFZPowH6kYtN0Lh9BQb+iYcvUQAzrJd+4/sNQitDlA=";
  };

  buildInputs = [reportlab lxml cssselect2 pillow];
  # checkInputs = [pytest-runner];
  doCheck = false;
}
