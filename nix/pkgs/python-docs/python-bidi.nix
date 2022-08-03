{
  buildPythonPackage,
  fetchPypi,

  six
}:
buildPythonPackage rec {
  pname = "python-bidi";
  version = "0.4.2";
  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-U0f3HoKz6Zdtxlfwne0r/jm6jWd3yoGlssVsMBIcSW4=";
  };
  buildInputs = [six];
}
