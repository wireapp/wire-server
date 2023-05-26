{ stdenv, python3, fetchgit }:
stdenv.mkDerivation rec {
  name = "rabbitmqadmin";
  version = "3.11.13";

  src = fetchgit {
    url = "https://github.com/rabbitmq/rabbitmq-server";
    rev = "v${version}";
    sha256 = "sha256-lbOuxJz66xlGlgodbz8Xlb3hcaewVFMqf9R/5XlqaAY=";
  };

  propagatedBuildInputs = [ python3 ];

  dontBuild = true;

  installPhase = ''
    install -Dm755 "$src/deps/rabbitmq_management/bin/rabbitmqadmin" "$out/bin/rabbitmqadmin"
  '';
}
