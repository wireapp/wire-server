{ fetchPypi
, buildPythonPackage
, sphinx
, requests
, setuptools
, pyyaml
}:
buildPythonPackage rec {
  doCheck = false;
  pname = "sphinxcontrib-kroki";
  version = "1.3.0";
  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256:0slvqj30hznvl4qsi86m3zf02qbl67qdvy2d5mvl6942yphlbklh";
  };

  propagatedBuildInputs = [
    sphinx
    requests
    pyyaml
    setuptools
  ];
}
