{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.Spar.APISpec where

import Bilge
import Control.Monad
import Control.Monad.IO.Class
import Data.List (isInfixOf)
import Data.String.Conversions
import Lens.Micro
import SAML2.WebSSO as SAML
import Spar.API ()
import Spar.Options as Opts
import Test.Hspec
import Util.Options

import qualified Data.UUID as UUID


mkspec :: Opts -> IO Spec
mkspec opts = do
  mgr :: Manager <- newManager defaultManagerSettings
  let brigreq :: (Request -> Request)
      brigreq = Bilge.host (opts ^. to Opts.brig . epHost . to cs)
              . Bilge.port (opts ^. to Opts.brig . epPort)
      sparreq :: (Request -> Request)
      sparreq = Bilge.host (opts ^. to Opts.saml . SAML.cfgSPHost . to cs)
              . Bilge.port (opts ^. to Opts.saml . SAML.cfgSPPort . to fromIntegral)

      shouldRespondWith :: forall a. (HasCallStack, Show a, Eq a) => Http a -> (a -> Bool) -> Expectation
      shouldRespondWith action proper = liftIO (runHttpT mgr action) >>= \resp -> resp `shouldSatisfy` proper

      ping :: (Request -> Request) -> Http ()
      ping req = void . get $ req . path "/i/status" . expect2xx

  pure $ do
    describe "status, metainfo" $ do
      it "brig /i/status" $ do
        ping brigreq `shouldRespondWith` (== ())

      it "spar /i/status" $ do
        ping sparreq `shouldRespondWith` (== ())

      it "metainfo" $ do
        get (sparreq . path "/sso/metainfo" . expect2xx)
          `shouldRespondWith` (\(responseBody -> Just (cs -> bdy)) -> all (`isInfixOf` bdy)
                                [ "md:SPSSODescriptor"
                                , "validUntil"
                                , "WantAssertionsSigned=\"true\""
                                ])

    describe "/sso/initiate-login/:idp" $ do
      context "unknown IdP" $ do
        it "responds with 'not found'" $ do
          let uuid = cs $ UUID.toText UUID.nil
          get (sparreq . path ("/sso/initiate-login/" <> uuid))
            `shouldRespondWith` ((>= 400) . statusCode)

      context "known IdP" $ do
        it "responds with request" $ do
          IdPId (cs . UUID.toText -> uuid) <- createTestIdP
          get (sparreq . path ("/sso/initiate-login/" <> uuid) . expect2xx)
            `shouldRespondWith` (\(responseBody -> Just (cs -> bdy)) -> all (`isInfixOf` bdy)
                                  [ "<html xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">"
                                  , "<body onload=\"document.forms[0].submit()\">"
                                  , "<input name=\"SAMLRequest\" type=\"hidden\" "
                                  ])

    describe "/sso/finalize-login" $ do
      context "access denied" $ do
        it "responds with 'forbidden'" $ do
          pending

      context "access granted" $ do
        it "responds with redirect to app" $ do
          pending

      context "response does not match any request" $ do
        it "rejects" $ do
          pending

      context "response contains assertions that have been offered before" $ do
        it "rejects" $ do
          pending


createTestIdP :: IO IdPId
createTestIdP = undefined
