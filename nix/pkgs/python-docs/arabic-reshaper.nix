{
  buildPythonPackage,
  fetchPypi,

  future
}:
buildPythonPackage rec {
  pname = "arabic_reshaper";
  version = "2.1.1";
  src = fetchPypi {
    inherit pname version;
    sha256 = "sha256-zzGPpdUdLSJPpJv2vbu0aE9r0sBot1z84OYH+JrBmdw=";
  };
  buildInputs = [future];
  doCheck = false;
}
