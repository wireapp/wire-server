{ fetchPypi
, buildPythonPackage
, sphinx
}:
buildPythonPackage rec {
  doCheck = false;
  pname = "sphinx_reredirects";
  version = "0.1.2";
  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-oOchMwR1mwHtwi8DLxcVocYRdvyPFnFk56Urn+7JrGQ=";
  };

  propagatedBuildInputs = [
    sphinx
  ];

}
