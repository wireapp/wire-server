{ buildPythonApplication
, buildPythonPackage
, fetchPypi
, docutils
, importlib-metadata
, jinja2
, packaging
, pygments
, pyyaml
, reportlab
, smartypants
, pillow
,
}:
buildPythonPackage rec {
  pname = "rst2pdf";
  version = "0.99";
  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-j6I/qTvd0fUtBYzq6rZYLBRVRtgPL4qVl083A71sgVI=";
  };

  doCheck = false;

  propagatedBuildInputs = [
    docutils
    importlib-metadata
    jinja2
    packaging
    pygments
    pyyaml
    reportlab
    smartypants
    pillow
  ];
}
