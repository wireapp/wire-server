{
  buildPythonApplication,
  buildPythonPackage,
  fetchPypi,

  sphinx,
}:
buildPythonPackage rec {
  pname = "sphinx-multiversion";
  version = "0.2.4";
  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256:1jqbk7a1sm5yfvrvczlfm57sy4ya732fkrbcip5n7vayrfgcmlaw";
  };

  doCheck = false;

  propagatedBuildInputs = [
    sphinx
  ];
}
