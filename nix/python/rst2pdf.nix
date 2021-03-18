{
  buildPythonApplication,
  buildPythonPackage,
  fetchPypi,

  docutils,
  importlib-metadata,
  jinja2,
  packaging,
  pygments,
  reportlab,
  smartypants,
  pillow,
}:
buildPythonPackage rec {
  pname = "rst2pdf";
  version = "0.98";
  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-ECQEgrzfk+KOIwDkdC/FzlTERd6wAdVK48YnQZYH5Rw=";
  };

  doCheck = false;

  propagatedBuildInputs = [
    docutils
    importlib-metadata
    jinja2
    packaging
    pygments
    reportlab
    smartypants
    pillow
  ];
}
