{
  description = "A very basic flake";

  inputs = {
    self.submodules = true;
    nixpkgs.url = "github:nixos/nixpkgs?rev=09b8fda8959d761445f12b55f380d90375a1d6bb";
    nixpkgs_24_11.url = "github:nixos/nixpkgs?ref=nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";

    cryptobox-haskell = {
      url = "github:wireapp/cryptobox-haskell?rev=7546a1a25635ef65183e3d44c1052285e8401608";
      flake = false;
    };
    bloodhound = {
      url = "github:wireapp/bloodhound?rev=dac0f1384b335ce35dc026bf8154e574b1a15d62";
      flake = false;
    };
    hsaml2 = {
      url = "github:dylex/hsaml2?rev=874627ad22e69afe4d9a797e39633ffb30697c78";
      flake = false;
    };
    hedis = {
      url = "github:wireapp/hedis?rev=00d7fbf5f19b812b9e64e12be8860c4741be8558";
      flake = false;
    };

    http-client = {
      url = "github:wireapp/http-client?rev=37494bb9a89dd52f97a8dc582746c6ff52943934";
      flake = false;
    };

    hspec-wai = {
      url = "github:wireapp/hspec-wai?rev=08176f07fa893922e2e78dcaf996c33d79d23ce2";
      flake = false;
    };

    cql = {
      url = "github:wireapp/cql?rev=abbd2739969d17a909800f282d10d42a254c4e3b";
      flake = false;
    };

    cql-io = {
      url = "github:wireapp/cql-io?rev=c2b6aa995b5817ed7c78c53f72d5aa586ef87c36";
      flake = false;
    };

    wai-predicates = {
      url = "github:wireapp/wai-predicates?rev=ff95282a982ab45cced70656475eaf2cefaa26ea";
      flake = false;
    };

    tasty = {
      url = "github:wireapp/tasty?rev=97df5c1db305b626ffa0b80055361b7b28e69cec";
      flake = false;
    };

    servant-openapi3 = {
      url = "github:wireapp/servant-openapi3?rev=0db0095040df2c469a48f5b8724595f82afbad0c";
      flake = false;
    };

    postie = {
      url = "github:alexbiehl/postie?rev=13404b8cb7164cd9010c9be6cda5423194dd0c06";
      flake = false;
    };

    tinylog = {
      url = "github:wireapp/tinylog?rev=9609104263e8cd2a631417c1c3ef23e090de0d09";
      flake = false;
    };

    tasty-ant-xml = {
      url = "github:wireapp/tasty-ant-xml?rev=11c53e976e2e941f25a33e8768669eb576d19ea8";
      flake = false;
    };

    text-icu-translit = {
      url = "github:wireapp/text-icu-translit?rev=317bbd27ea5ae4e7f93836ee9ca664f9bde7c583";
      flake = false;
    };

    warp = {
      url = "github:yesodweb/wai?rev=ef34334b160c74b62435ccc21f5b458f73506b2f";
      flake = false;
    };

    http2 = {
      url = "github:wireapp/http2?rev=ca606d86ed304fa780f7a60d11244019c62a10e0";
      flake = false;
    };

    amazonka = {
      url = "github:brendanhay/amazonka?rev=a7d699be1076e2aad05a1930ca3937ffea954ad8";
      flake = false;
    };
  };

  outputs = inputs@{ nixpkgs, nixpkgs_24_11, flake-utils, ... }:
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
        wireServerPkgs = import ./nix { inherit pkgs pkgs_24_11 inputs; };
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
