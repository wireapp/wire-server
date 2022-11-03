{
  fetchPypi,
  buildPythonPackage,

  sphinx,
} :
buildPythonPackage rec {

  pname = "sphinx_reredirects";
  version = "0.1.1";
  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256:RRmkXTFskhxGMnty/kEOHnoy/lFpR0EpYCCwygCPvO4=";
  };

  propagatedBuildInputs = [
    sphinx
  ];

}
