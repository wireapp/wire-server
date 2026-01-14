{
  description = "A very basic flake";

  inputs = {
    self.submodules = true;
    nixpkgs.url = "github:nixos/nixpkgs?rev=09b8fda8959d761445f12b55f380d90375a1d6bb";
    nixpkgs_24_11.url = "github:nixos/nixpkgs?ref=nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
    tom-bombadil = {
      url = "path:/home/axeman/workspace/tom-bombadil";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    cryptobox-haskell = {
      url = "github:wireapp/cryptobox-haskell?ref=master";
      flake = false;
    };
    bloodhound = {
      url = "github:wireapp/bloodhound?ref=wire-fork";
      flake = false;
    };
    hsaml2 = {
      url = "github:dylex/hsaml2?ref=main";
      flake = false;
    };
    hedis = {
      url = "github:wireapp/hedis?ref=wire-changes";
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
      url = "github:brendanhay/amazonka?rev=a7d699be1076e2aad05a1930ca3937ffea954ad8";
      flake = false;
    };
  };

  outputs = inputs@{ nixpkgs, nixpkgs_24_11, flake-utils, tom-bombadil, ... }:
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
          default = wireServerPkgs.wireServer.devEnv;
        };
      }
    );
}
