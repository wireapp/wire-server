# Copied from nixpkgs and modified because it seems too complicated to override
# buildGoModule packages.
{ lib, stdenv, buildGoModule, fetchFromGitHub, installShellFiles }:

buildGoModule rec {
  pname = "kubernetes-helm";
  version = "3.11.0-patched";

  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "helm";
    rev = "949de3195be5b3d21ed707da18ee3bcb2a9a2af8";
    sha256 = "sha256-alyR6+gm7WEvFfJxHl9a0jpC3+457Kg6aRHcidA0RZg=";
  };
  vendorSha256 = "sha256-LRMDrBSl5EGQqQt5FUU4JJHqdwfYt5qsVpe76jUQBVI=";

  subPackages = [ "cmd/helm" ];
  ldflags = [
    "-w"
    "-s"
    "-X helm.sh/helm/v3/internal/version.version=v${version}"
    "-X helm.sh/helm/v3/internal/version.gitCommit=${src.rev}"
  ];

  preCheck = ''
    # skipping version tests because they require dot git directory
    substituteInPlace cmd/helm/version_test.go \
      --replace "TestVersion" "SkipVersion"
  '' + lib.optionalString stdenv.isLinux ''
    # skipping plugin tests on linux
    substituteInPlace cmd/helm/plugin_test.go \
      --replace "TestPluginDynamicCompletion" "SkipPluginDynamicCompletion" \
      --replace "TestLoadPlugins" "SkipLoadPlugins"
    substituteInPlace cmd/helm/helm_test.go \
      --replace "TestPluginExitCode" "SkipPluginExitCode"
  '';

  nativeBuildInputs = [ installShellFiles ];
  postInstall = ''
    $out/bin/helm completion bash > helm.bash
    $out/bin/helm completion zsh > helm.zsh
    installShellCompletion helm.{bash,zsh}
  '';

  meta = with lib; {
    homepage = "https://github.com/kubernetes/helm";
    description = "A package manager for kubernetes";
    mainProgram = "helm";
    license = licenses.asl20;
    maintainers = with maintainers; [ rlupton20 edude03 saschagrunert Frostman Chili-Man techknowlogick ];
  };
}
