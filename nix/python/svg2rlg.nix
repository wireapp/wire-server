{
  buildPythonPackage,
  fetchPypi,

  reportlab
}:
buildPythonPackage rec {
  pname = "svg2rlg";
  version = "0.3";
  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-BdtEgLkOkS4Icn1MskOF/jPoQ23vB5uPFJtho1Bji+4=";
  };

  buildInputs = [reportlab];
  doCheck = false;
}
