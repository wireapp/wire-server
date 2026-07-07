{
  description = "A very basic flake";

  inputs = {
    self.submodules = true;
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-26.05";
    nixpkgs_24_11.url = "github:nixos/nixpkgs?ref=nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
    tom-bombadil = {
      url = "github:wireapp/tom-bombadil";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    sbomnix = {
      url = "github:tiiuae/sbomnix/v1.7.4";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    bloodhound = {
      url = "github:wireapp/bloodhound?ref=wire-fork";
      flake = false;
    };

    http-client = {
      url = "github:wireapp/http-client?ref=master";
      flake = false;
    };

    hspec-wai = {
      url = "github:wireapp/hspec-wai?ref=body-contains";
      flake = false;
    };

    cql = {
      url = "github:wireapp/cql?ref=develop";
      flake = false;
    };

    cql-io = {
      url = "github:wireapp/cql-io?ref=control-conn";
      flake = false;
    };

    wai-predicates = {
      url = "github:wireapp/wai-predicates?ref=develop";
      flake = false;
    };

    tasty = {
      url = "github:wireapp/tasty?ref=mangoiv/full-stacktrace-rebased";
      flake = false;
    };

    servant-openapi3 = {
      url = "github:wireapp/servant-openapi3?ref=required-request-bodies";
      flake = false;
    };

    postie = {
      url = "github:alexbiehl/postie?ref=master";
      flake = false;
    };

    tinylog = {
      url = "github:wireapp/tinylog?ref=wire-fork";
      flake = false;
    };

    tasty-ant-xml = {
      url = "github:wireapp/tasty-ant-xml?ref=drop-console-formatting_rebased";
      flake = false;
    };

    text-icu-translit = {
      url = "github:wireapp/text-icu-translit?ref=master";
      flake = false;
    };

    amazonka = {
      url = "github:brendanhay/amazonka";
      flake = false;
    };

    hasql-migration = {
      url = "github:wireapp/hasql-migration?ref=upgrade-hasql";
      flake = false;
    };

    postgresql-connection-string = {
      url = "github:wireapp/postgresql-connection-string?ref=expose-from-key-value-params";
      flake = false;
    };

    cryptostore = {
      # Use master because the released version doesn't work with the latest version of cyrpton.
      url = "git+https://codeberg.org/ocheron/cryptostore.git";
      flake = false;
    };

    hsaml2 = {
      url = "github:wireapp/hsaml2/use-crypton-asn1";
      flake = false;
    };
  };

  outputs = inputs@{ nixpkgs, nixpkgs_24_11, flake-utils, tom-bombadil, sbomnix, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            (import ./nix/overlay.nix)
            (import ./nix/overlay-docs.nix)
          ];
        };
        pkgs_24_11 = import nixpkgs_24_11 {
          inherit system;
        };
        bomDependenciesDrv = tom-bombadil.lib.${system}.bomDependenciesDrv;
        wireServerPkgs = import ./nix { inherit pkgs pkgs_24_11 inputs bomDependenciesDrv; };
      in
      {
        # profileEnv wireServer docs docsEnv mls-test-cli nginz;
        packages = {
          inherit (wireServerPkgs) pkgs profileEnv wireServer docs docsEnv mls-test-cli nginz;
        };
        devShells = {
          default = pkgs.mkShell {
            packages = wireServerPkgs.wireServer.devEnvPkgs;
          };
        };
        # Shell environment for generating Software Bill of Materials (SBOMs)
        # Used on CI.
        devShells.sbom = pkgs.mkShell {
          packages = [
            # Shell and core utilities
            pkgs.bash
            pkgs.coreutils
            pkgs.findutils
            pkgs.git

            # JSON/YAML processing
            pkgs.jq
            pkgs.yq

            # Network tools
            pkgs.curl

            # Container and SBOM tools
            pkgs.cyclonedx-cli
            pkgs.syft
            pkgs.kubernetes-helm
            pkgs.helmfile
            sbomnix.packages.${system}.default
          ] ++ pkgs.lib.optionals pkgs.stdenv.isLinux [
            # Linux-only container tools
            pkgs.skopeo
            pkgs.docker
            pkgs.docker-compose
          ];
          meta = {
            description = "Development shell with tools for SBOM generation";
          };
        };
      }
    );
}
