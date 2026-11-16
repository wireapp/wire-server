{ lib, stdenv, python313Packages, fetchPypi, autoPatchelfHook }:
let
  # Upstream wheels bundle the legacy OpenSSL builds needed to probe obsolete TLS.
  wheels = {
    x86_64-linux = {
      platform = "manylinux2014_x86_64.manylinux_2_17_x86_64";
      sha256 = "439c1e76716fd10b3bb90b66509e1e045f56648482dbf0e5f93ce40336590a57";
    };
    aarch64-linux = {
      platform = "manylinux2014_aarch64.manylinux_2_17_aarch64";
      sha256 = "360d818604d48af37592eb1f7dcc335e863b752625be721b772fc5ae9d06c8f9";
    };
    aarch64-darwin = {
      platform = "macosx_11_0_arm64";
      sha256 = "bc21baf2ee85e4804d6fed38bf71564f5074660bec21746b35244ecabfb5d750";
    };
  };
  nassl = python313Packages.buildPythonPackage {
    pname = "nassl";
    version = "5.4.0";
    format = "wheel";
    src = fetchPypi ({
      pname = "nassl";
      version = "5.4.0";
      format = "wheel";
      python = "cp313";
      dist = "cp313";
      abi = "cp313";
    } // wheels.${stdenv.hostPlatform.system});
    nativeBuildInputs = lib.optionals stdenv.isLinux [ autoPatchelfHook ];
    buildInputs = lib.optionals stdenv.isLinux [ stdenv.cc.cc.lib ];
    pythonImportsCheck = [ "nassl.ssl_client" "nassl.legacy_ssl_client" ];
  };
in
python313Packages.buildPythonApplication {
  pname = "sslyze";
  version = "6.3.1";
  pyproject = true;
  src = fetchPypi {
    pname = "sslyze";
    version = "6.3.1";
    sha256 = "2cd4062f82a3fa605dbd34e3b71f99961e4e7cf438879da1c52f619855ffbc1d";
  };
  build-system = [ python313Packages.setuptools ];
  dependencies = with python313Packages; [ nassl cryptography tls-parser pydantic ];
  # Upstream already permits cryptography 48; the 6.3.1 metadata predates it.
  pythonRelaxDeps = [ "cryptography" ];
  pythonImportsCheck = [ "sslyze" ];
  meta = {
    homepage = "https://github.com/nabla-c0d3/sslyze";
    license = lib.licenses.agpl3Only;
    mainProgram = "sslyze";
    platforms = builtins.attrNames wheels;
  };
}
