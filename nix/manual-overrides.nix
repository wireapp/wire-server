{ libsodium, protobuf, hlib, mls-test-cli, fetchurl, curl, pkg-config, postgresql, openssl, icu, cryptobox, stdenv, lib, ... }:
# FUTUREWORK: Figure out a way to detect if some of these packages are not
# actually marked broken, so we can cleanup this file on every nixpkgs bump.
hself: hsuper: {
  # ----------------
  # tests don't pass
  # (these are in general not fine they need to be investigated)
  # FUTUREWORK: investigate whether all of these tests need to fail
  # ----------------

  # tests don't work, but only in a flake
  saml2-web-sso = hlib.dontCheck hsuper.saml2-web-sso;

  # test suite doesn't compile and needs network access
  bloodhound = hlib.dontCheck hsuper.bloodhound;

  # tests need network access, cabal2nix disables haddocks
  cql-io = hlib.doHaddock (hlib.dontCheck hsuper.cql-io);

  quickcheck-state-machine = hlib.markUnbroken (hlib.dontCheck hsuper.quickcheck-state-machine);

  # Tests require a running redis
  hedis = hlib.dontCheck hsuper.hedis;

  HaskellNet = hlib.dontCheck hsuper.HaskellNet;

  # Tests require a running postgresql
  hasql = hlib.dontCheck hsuper.hasql;
  hasql-pool = hlib.dontCheck hsuper.hasql-pool;
  hasql-migration = hlib.markUnbroken (hlib.dontCheck hsuper.hasql-migration);
  hasql-transaction = hlib.dontCheck hsuper.hasql-transaction; # users 1.2.1 from nixpkgs
  postgresql-binary = hlib.dontCheck (hsuper.postgresql-binary);

  # ---------------------
  # need to be jailbroken
  # (these need to be fixed upstream eventually)
  # FUTUREWORK: fix the dependency bounds upstream
  # ---------------------
  binary-parsers = hlib.markUnbroken (hlib.doJailbreak hsuper.binary-parsers);
  bytestring-arbitrary = hlib.markUnbroken (hlib.doJailbreak hsuper.bytestring-arbitrary);
  lens-datetime = hlib.markUnbroken (hlib.doJailbreak hsuper.lens-datetime);
  postie = hlib.doJailbreak hsuper.postie;
  lrucaching = hlib.doJailbreak (hlib.markUnbroken hsuper.lrucaching);
  # added servant-openapi3 because the version bounds of some dependent packages
  # of our pin exclude the versions in our current nixpkgs
  servant-openapi3 = hlib.doJailbreak (hlib.dontCheck hsuper.servant-openapi3);

  # the libsodium haskell library is incompatible with the new version of the libsodium c library
  # that nixpkgs has - this downgrades libsodium from 1.0.19 to 1.0.18
  libsodium = hlib.markUnbroken (hlib.addPkgconfigDepend hsuper.libsodium (
    libsodium.overrideAttrs (old:
      rec {
        # we don't care for the patches for mingw and for 1.0.19
        patches = [ ];
        version = "1.0.18";
        src = fetchurl {
          url = "https://download.libsodium.org/libsodium/releases/${old.pname}-${version}.tar.gz";
          hash = "sha256-b1BEkLNCpPikxKAvybhmy++GItXfTlRStGvhIeRmNsE=";
        };
      }
    )));

  # hs-opentelemetry pin removal bumps API -> 0.3.0.0 and SDK -> 0.1.0.1 from the pinned commit; instrumentation stays at 0.1.1.0/0.1.0.1.
  hs-opentelemetry-instrumentation-wai = hlib.markUnbroken (hlib.doJailbreak hsuper.hs-opentelemetry-instrumentation-wai);
  hs-opentelemetry-instrumentation-conduit = hlib.markUnbroken (hlib.doJailbreak hsuper.hs-opentelemetry-instrumentation-conduit);
  hs-opentelemetry-instrumentation-http-client = hlib.doJailbreak hsuper.hs-opentelemetry-instrumentation-http-client;
  hs-opentelemetry-utils-exceptions = hlib.markUnbroken (hlib.doJailbreak hsuper.hs-opentelemetry-utils-exceptions);

  # ------------------------------------
  # okay but marked broken (nixpkgs bug)
  # (we can unfortunately not do anything here but update nixpkgs)
  # ------------------------------------
  template = hlib.markUnbroken hsuper.template;
  system-linux-proc = hlib.markUnbroken hsuper.system-linux-proc;

  # -----------------
  # version overrides
  # (these are fine but will probably need to be adjusted in a future nixpkgs update)
  # -----------------
  # warp requires curl in its testsuite
  warp = hlib.addTestToolDepends hsuper.warp [ curl ];

  # -----------------
  # flags and patches
  # (these are fine)
  # -----------------
  cryptostore = hlib.addBuildDepends (hlib.dontCheck (hlib.appendConfigureFlags hsuper.cryptostore [ "-fuse_crypton" ]))
    [ hself.crypton hself.crypton-x509 hself.crypton-x509-validation ];
  # doJailbreak because upstreams requires a specific crypton-connection version we don't have
  hoogle = hlib.justStaticExecutables (hlib.dontCheck (hsuper.hoogle));

  # Extra dependencies/flags for local packages
  cryptobox-haskell = hlib.addBuildDepends hsuper.cryptobox-haskell [ cryptobox ];
  http2-manager = hlib.disableCabalFlag hsuper.http2-manager "test-trailing-dot";
  sodium-crypto-sign = hlib.addPkgconfigDepend hsuper.sodium-crypto-sign libsodium.dev;
  text-icu-translit = hlib.addPkgconfigDepend hsuper.text-icu-translit icu;
  types-common-journal = hlib.addBuildTool hsuper.types-common-journal protobuf;
  wire-api = hlib.addBuildTool hsuper.wire-api mls-test-cli;
  wire-message-proto-lens = hlib.addBuildTool hsuper.wire-message-proto-lens protobuf;
  # Patch federator to work on Darwin by removing Linux-specific hinotify dependency
  federator = (if stdenv.isDarwin then hlib.dontCheck else (x: x))
    (hlib.overrideCabal hsuper.federator (drv:
    lib.optionalAttrs stdenv.isDarwin {
      postUnpack = ''
        ${drv.postUnpack or ""}
        # Remove hinotify and filepath from build-depends (no longer needed without file monitoring)
        sed -i.bak -e '/^    , hinotify$/d' -e '/^    , filepath$/d' */federator.cabal

        # Patch Monitor.Internal.hs to conditionally compile on Linux only
        cat > */src/Federator/Monitor/Internal.hs << 'MONITOR_EOF'
-- Stub implementation for non-Linux platforms
{-# LANGUAGE LambdaCase #-}
module Federator.Monitor.Internal
  ( FederationSetupError (..),
    mkSSLContext,
    mkSSLContextWithoutCert,
    showFederationSetupError,
  )
where

import Control.Exception (Exception, SomeException, displayException)
import Control.Monad (forM_, unless, when)
import Data.Text (Text)
import Data.Text qualified as Text
import Federator.Options (RunSettings (..))
import OpenSSL.Session (SSLContext)
import OpenSSL.Session qualified as SSL
import Polysemy (Embed, Member, Members, Sem, embed)
import Polysemy.Error qualified as Polysemy
import Prelude

-- Error types
data FederationSetupError
  = InvalidCAStore FilePath String
  | InvalidClientCertificate String
  | InvalidClientPrivateKey String
  | CertificateAndPrivateKeyDoNotMatch FilePath FilePath
  | SSLException SSL.SomeSSLException
  deriving (Show)

instance Exception FederationSetupError

showFederationSetupError :: FederationSetupError -> Text
showFederationSetupError = \case
  InvalidCAStore path err -> "Invalid CA store at " <> Text.pack path <> ": " <> Text.pack err
  InvalidClientCertificate err -> "Invalid client certificate: " <> Text.pack err
  InvalidClientPrivateKey err -> "Invalid client private key: " <> Text.pack err
  CertificateAndPrivateKeyDoNotMatch cert key ->
    "Certificate " <> Text.pack cert <> " and private key " <> Text.pack key <> " do not match"
  SSLException e -> "SSL exception: " <> Text.pack (show e)

displayExceptionNoBacktrace :: SomeException -> String
displayExceptionNoBacktrace = displayException

blessedTLS12Ciphers :: String
blessedTLS12Ciphers = "ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:DHE-RSA-AES128-GCM-SHA256:DHE-RSA-AES256-GCM-SHA384"

-- SSL Context creation without file monitoring
mkSSLContext ::
  ( Member (Embed IO) r,
    Member (Polysemy.Error FederationSetupError) r
  ) =>
  RunSettings ->
  Sem r SSLContext
mkSSLContext settings = do
  ctx <- mkSSLContextWithoutCert settings

  Polysemy.fromExceptionVia @SomeException (InvalidClientCertificate . displayExceptionNoBacktrace) $
    SSL.contextSetCertificateChainFile ctx (clientCertificate settings)

  Polysemy.fromExceptionVia @SomeException (InvalidClientPrivateKey . displayExceptionNoBacktrace) $
    SSL.contextSetPrivateKeyFile ctx (clientPrivateKey settings)

  privateKeyCheck <- Polysemy.fromExceptionVia @SSL.SomeSSLException SSLException $ SSL.contextCheckPrivateKey ctx
  unless privateKeyCheck $ do
    Polysemy.throw $ CertificateAndPrivateKeyDoNotMatch (clientCertificate settings) (clientPrivateKey settings)

  pure ctx

mkSSLContextWithoutCert :: (Members '[Embed IO, Polysemy.Error FederationSetupError] r) => RunSettings -> Sem r SSLContext
mkSSLContextWithoutCert settings = do
  ctx <- embed $ SSL.context
  embed $ do
    SSL.contextAddOption ctx SSL.SSL_OP_ALL
    SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv2
    SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv3
    SSL.contextAddOption ctx SSL.SSL_OP_NO_TLSv1

    SSL.contextSetCiphers ctx blessedTLS12Ciphers
    SSL.contextSetALPNProtos ctx ["h2"]

    SSL.contextSetVerificationMode ctx $
      SSL.VerifyPeer
        { SSL.vpFailIfNoPeerCert = False,
          SSL.vpClientOnce = False,
          SSL.vpCallback = Nothing
        }
  forM_ (remoteCAStore settings) $ \caStorePath ->
    Polysemy.fromExceptionVia @SomeException (InvalidCAStore caStorePath . displayExceptionNoBacktrace) $
      SSL.contextSetCAFile ctx caStorePath

  when (useSystemCAStore settings) $
    embed (SSL.contextSetDefaultVerifyPaths ctx)

  pure ctx
MONITOR_EOF

        # Patch Monitor.hs to provide stub implementation on Darwin
        cat > */src/Federator/Monitor.hs << 'EXPORT_EOF'
{-# OPTIONS_GHC -Wno-unused-imports #-}
-- Stub implementation for non-Linux platforms (no file monitoring)
module Federator.Monitor
  ( withMonitor,
    mkTLSSettingsOrThrow,
    FederationSetupError (..),
  )
where

import Control.Exception (throw)
import Federator.Monitor.Internal
import Federator.Options (RunSettings)
import OpenSSL.Session (SSLContext)
import Polysemy qualified
import Polysemy.Error qualified as Polysemy
import Prelude
import System.Logger (Logger)

-- Create TLS settings or throw an error
mkTLSSettingsOrThrow :: RunSettings -> IO SSLContext
mkTLSSettingsOrThrow = Polysemy.runM . runEither . Polysemy.runError @FederationSetupError . mkSSLContext
  where
    runEither = (either (Polysemy.embed @IO . throw) pure =<<)

-- Stub withMonitor that just runs the action without file monitoring
withMonitor :: Logger -> (SSLContext -> IO ()) -> RunSettings -> IO a -> IO a
withMonitor _logger _onNewContext _rs action = action
EXPORT_EOF
      '';
    }));
  postgresql-libpq-pkgconfig = hlib.addBuildDepends
    (hlib.markUnbroken hsuper.postgresql-libpq-pkgconfig)
    [ pkg-config postgresql.dev openssl.dev ];
  postgresql-libpq = hlib.overrideCabal
    (hlib.enableCabalFlag hsuper.postgresql-libpq "use-pkg-config")
    (drv: {
      libraryHaskellDepends = with hself; [
        base
        bytestring
        postgresql-libpq-pkgconfig
        unix
      ];
    });
}
