{
  extras = hackage:
    {
      packages = {
        "servant-swagger" = (((hackage.servant-swagger)."1.1.7.1").revisions).default;
        "swagger2" = (((hackage.swagger2)."2.5").revisions).default;
        "ormolu" = (((hackage.ormolu)."0.0.3.1").revisions).default;
        "bloodhound" = (((hackage.bloodhound)."0.16.0.0").revisions).default;
        "template" = (((hackage.template)."0.2.0.10").revisions).default;
        "HaskellNet" = (((hackage.HaskellNet)."0.5.1").revisions).default;
        "HaskellNet-SSL" = (((hackage.HaskellNet-SSL)."0.3.4.1").revisions).default;
        "snappy" = (((hackage.snappy)."0.2.0.2").revisions).default;
        "smtp-mail" = (((hackage.smtp-mail)."0.2.0.0").revisions).default;
        "stm-containers" = (((hackage.stm-containers)."1.1.0.4").revisions).default;
        "redis-io" = (((hackage.redis-io)."1.0.0").revisions).default;
        "redis-resp" = (((hackage.redis-resp)."1.0.0").revisions).default;
        "hedgehog-quickcheck" = (((hackage.hedgehog-quickcheck)."0.1.1").revisions).default;
        "stm-hamt" = (((hackage.stm-hamt)."1.2.0.4").revisions).default;
        "optics-th" = (((hackage.optics-th)."0.2").revisions).default;
        "primitive-unlifted" = (((hackage.primitive-unlifted)."0.1.2.0").revisions).default;
        "currency-codes" = (((hackage.currency-codes)."3.0.0.1").revisions).default;
        "mime" = (((hackage.mime)."0.4.0.2").revisions).default;
        "data-timeout" = (((hackage.data-timeout)."0.3.1").revisions).default;
        "geoip2" = (((hackage.geoip2)."0.4.0.1").revisions).default;
        "stomp-queue" = (((hackage.stomp-queue)."0.3.1").revisions).default;
        "text-icu-translit" = (((hackage.text-icu-translit)."0.1.0.7").revisions).default;
        "wai-middleware-gunzip" = (((hackage.wai-middleware-gunzip)."0.0.2").revisions).default;
        "cql-io-tinylog" = (((hackage.cql-io-tinylog)."0.1.0").revisions).default;
        "invertible-hxt" = (((hackage.invertible-hxt)."0.1").revisions).default;
        "network-uri-static" = (((hackage.network-uri-static)."0.1.2.1").revisions).default;
        "base58-bytestring" = (((hackage.base58-bytestring)."0.1.0").revisions).default;
        "stompl" = (((hackage.stompl)."0.5.0").revisions).default;
        "pattern-trie" = (((hackage.pattern-trie)."0.1.0").revisions).default;
        "wai-route" = (((hackage.wai-route)."0.4.0").revisions).default;
        "ghc-lib-parser" = (((hackage.ghc-lib-parser)."8.8.2.20200205").revisions)."343f889f7b29f5ec07cf0d18d2a53f250fa5c002b6468a6a05b385d0191b8d34";
        api-bot = ./api-bot.nix;
        api-client = ./api-client.nix;
        bilge = ./bilge.nix;
        brig-types = ./brig-types.nix;
        cargohold-types = ./cargohold-types.nix;
        cassandra-util = ./cassandra-util.nix;
        extended = ./extended.nix;
        federation-util = ./federation-util.nix;
        galley-types = ./galley-types.nix;
        gundeck-types = ./gundeck-types.nix;
        imports = ./imports.nix;
        metrics-core = ./metrics-core.nix;
        metrics-wai = ./metrics-wai.nix;
        ropes = ./ropes.nix;
        sodium-crypto-sign = ./sodium-crypto-sign.nix;
        ssl-util = ./ssl-util.nix;
        tasty-cannon = ./tasty-cannon.nix;
        types-common = ./types-common.nix;
        types-common-aws = ./types-common-aws.nix;
        types-common-journal = ./types-common-journal.nix;
        wai-utilities = ./wai-utilities.nix;
        zauth = ./zauth.nix;
        brig = ./brig.nix;
        cannon = ./cannon.nix;
        cargohold = ./cargohold.nix;
        galley = ./galley.nix;
        gundeck = ./gundeck.nix;
        proxy = ./proxy.nix;
        spar = ./spar.nix;
        api-simulations = ./api-simulations.nix;
        bonanza = ./bonanza.nix;
        auto-whitelist = ./auto-whitelist.nix;
        migrate-sso-feature-flag = ./migrate-sso-feature-flag.nix;
        service-backfill = ./service-backfill.nix;
        makedeb = ./makedeb.nix;
        stern = ./stern.nix;
        wai-middleware-prometheus = ./wai-middleware-prometheus.nix;
        saml2-web-sso = ./saml2-web-sso.nix;
        hscim = ./hscim.nix;
        collectd = ./collectd.nix;
        snappy-framing = ./snappy-framing.nix;
        wai-routing = ./wai-routing.nix;
        multihash = ./multihash.nix;
        aws = ./aws.nix;
        hspec-wai = ./hspec-wai.nix;
        cryptobox-haskell = ./cryptobox-haskell.nix;
        hsaml2 = ./hsaml2.nix;
        http-client = ./http-client.nix;
        http-client-openssl = ./http-client-openssl.nix;
        http-client-tls = ./http-client-tls.nix;
        http-conduit = ./http-conduit.nix;
        amazonka = ./amazonka.nix;
        amazonka-elb = ./amazonka-elb.nix;
        amazonka-redshift = ./amazonka-redshift.nix;
        amazonka-route53 = ./amazonka-route53.nix;
        amazonka-core = ./amazonka-core.nix;
        };
      };
  resolver = "lts-14.12";
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "types-common" = {
            flags = {
              "arbitrary" = lib.mkOverride 900 true;
              "protobuf" = lib.mkOverride 900 true;
              "cql" = lib.mkOverride 900 true;
              };
            };
          "galley-types" = { flags = { "cql" = lib.mkOverride 900 true; }; };
          "brig-types" = { flags = { "cql" = lib.mkOverride 900 true; }; };
          };
        })
    { packages = {}; }
    ];
  }