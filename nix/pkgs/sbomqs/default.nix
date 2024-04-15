{ buildGoModule, fetchFromGitHub, lib, ... }:
buildGoModule rec {
  pname = "sbomqs";
  version = "0.0.30";

  src = fetchFromGitHub {
    owner = "interlynk-io";
    repo = "sbomqs";
    rev = "v${version}";
    hash = "sha256-+y7+xi+E8kjGUjhIRKNk6ogcQMP+Dp39LrL66B1XdrQ=";
  };

  vendorHash = "sha256-V6k7nF2ovyl4ELE8Cqe/xjpmPAKI0t5BNlssf41kd0Y=";

  meta = with lib; {
    description = "SBOM quality score - Quality metrics for your sboms";
    homepage = "https://github.com/interlynk-io/sbomqs";
    license = licenses.asl20;
    mainProgram = "sbomqs";
  };
}
