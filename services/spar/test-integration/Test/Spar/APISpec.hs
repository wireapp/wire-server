{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Test.Spar.APISpec where

import Bilge
import Control.Monad.Reader
import Data.Aeson as Aeson hiding (json)
import Data.Aeson.QQ as Aeson
import Data.Either (isRight)
import Data.Id
import Data.List (isInfixOf)
import Data.String.Conversions
import Data.UUID as UUID hiding (null, fromByteString)
import Lens.Micro
import SAML2.WebSSO as SAML
import Spar.Types
import URI.ByteString.QQ
import Util

import qualified Galley.Types.Teams as Galley


-- TODO: what else needs to be tested, beyond the pending tests listed here?


spec :: SpecWith TestEnv
spec = do
    describe "status, metainfo" $ do
      it "brig /i/status" $ do
        env <- ask
        ping (env ^. teBrig) `shouldRespondWith` (== ())

      it "spar /i/status" $ do
        env <- ask
        ping (env ^. teSpar) `shouldRespondWith` (== ())

      it "metainfo" $ do
        env <- ask
        get ((env ^. teSpar) . path "/sso/metainfo" . expect2xx)
          `shouldRespondWith` (\(responseBody -> Just (cs -> bdy)) -> all (`isInfixOf` bdy)
                                [ "md:SPSSODescriptor"
                                , "validUntil"
                                , "WantAssertionsSigned=\"true\""
                                ])

    describe "/sso/initiate-login/:idp" $ do
      context "unknown IdP" $ do
        it "responds with 'not found'" $ do
          env <- ask
          let uuid = cs $ UUID.toText UUID.nil
          get ((env ^. teSpar) . path ("/sso/initiate-login/" <> uuid))
            `shouldRespondWith` ((== 404) . statusCode)

      context "known IdP" $ do
        it "responds with request" $ do
          env <- ask
          (_, _, cs . UUID.toText . fromIdPId -> idp) <- createTestIdP
          get ((env ^. teSpar) . path ("/sso/initiate-login/" <> idp) . expect2xx)
            `shouldRespondWith` (\(responseBody -> Just (cs -> bdy)) -> all (`isInfixOf` bdy)
                                  [ "<html xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">"
                                  , "<body onload=\"document.forms[0].submit()\">"
                                  , "<input name=\"SAMLRequest\" type=\"hidden\" "
                                  ])

    describe "/sso/finalize-login" $ do  -- TODO: either use workingIdP or mock one locally.  the
                                         -- latter is faster to run, but we need the former anyway,
                                         -- so we might as well rely on that.
      context "access denied" $ do
        it "responds with 'forbidden'" $ do
          pending

      context "access granted" $ do
        it "responds with redirect to app" $ do
          pending

        context "unknown user" $ do
          it "creates the user" $ do
            pending

      context "unknown IdP" $ do
        it "rejects" $ do
          pending

      context "bad AuthnRequest" $ do
        it "rejects" $ do
          pending

      context "response does not match any request" $ do
        it "rejects" $ do
          pending

      context "response contains assertions that have been offered before" $ do
        it "rejects" $ do
          pending


    let testGetOrDelete :: (Eq a, Show a)
                        => (SparReq -> Maybe UserId -> IdPId -> Http (Bilge.Response a))
                        -> SpecWith TestEnv
        testGetOrDelete whichone = do
          context "unknown IdP" $ do
            it "responds with 'not found'" $ do
              env <- ask
              whichone (env ^. teSpar) Nothing (IdPId UUID.nil)
                `shouldRespondWith` ((== 404) . statusCode)

          context "no zuser" $ do
            it "responds with 'not found'" $ do
              pending
              env <- ask
              (_, _, idp) <- createTestIdP
              whichone (env ^. teSpar) Nothing idp
                `shouldRespondWith` ((== 404) . statusCode)

          context "zuser has no team" $ do
            it "responds with 'not found'" $ do
              pending
              env <- ask
              (_, _, idp) <- createTestIdP
              (uid, _) <- call $ createRandomPhoneUser (env ^. teBrig)
              whichone (env ^. teSpar) (Just uid) idp
                `shouldRespondWith` ((== 404) . statusCode)

          context "zuser has wrong team" $ do
            it "responds with 'not found'" $ do
              pending
              env <- ask
              (_, _, idp) <- createTestIdP
              (uid, _) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
              whichone (env ^. teSpar) (Just uid) idp
                `shouldRespondWith` ((== 404) . statusCode)

          context "zuser is a team member, but not a team admin" $ do
            it "responds with 'forbidden' and a helpful message" $ do
              env <- ask
              (_owner, tid, idp) <- createTestIdP
              nobody <- let Just perms = Galley.newPermissions mempty mempty
                        in call $ createTeamMember (env ^. teBrig) (env ^. teGalley) tid perms
              whichone (env ^. teSpar) (Just nobody) idp
                `shouldRespondWith` ((== 403)  . statusCode)

    describe "GET /identity-providers/:idp" $ do
      testGetOrDelete callIdpGet'

      context "known IdP" $ do
        it "responds with 2xx and IdP" $ do
          env <- ask
          (uid, _, idp) <- createTestIdP
          callIdpGet' (env ^. teSpar) (Just uid) idp
            `shouldRespondWith` (\resp -> statusCode resp == 200 && isRight (responseJSON @IdP resp))

    describe "DELETE /identity-providers/:idp" $ do
      testGetOrDelete callIdpDelete'

      context "known IdP" $ do
        it "responds with 2xx and removes IdP" $ do
          env <- ask
          (uid, _, idp) <- createTestIdP
          callIdpDelete' (env ^. teSpar) (Just uid) idp
            `shouldRespondWith` \resp -> statusCode resp < 300
          callIdpGet' (env ^. teSpar) (Just uid) idp
            `shouldRespondWith` ((== 404) . statusCode)

    describe "POST /identity-providers/:idp" $ do
      let check :: (Int -> Bool) -> Value -> ResponseLBS -> Bool
          check statusIs msg resp = statusIs (statusCode resp) && responseJSON resp == Right msg

      context "no zuser" $ do
        it "responds with 'forbidden' and a helpful message" $ do
          env <- ask
          callIdpCreate' (env ^. teSpar) Nothing (env ^. teNewIdp)
            `shouldRespondWith` check (== 403) [aesonQQ|{"error":"no auth token"}|]

      context "zuser has no team" $ do
        it "responds with 'forbidden' and a helpful message" $ do
          env <- ask
          (uid, _) <- call $ createRandomPhoneUser (env ^. teBrig)
          callIdpCreate' (env ^. teSpar) (Just uid) (env ^. teNewIdp)
            `shouldRespondWith` check (== 403) [aesonQQ|{"error":"you need to be team admin to create an IdP"}|]

      context "zuser is a team member, but not a team admin" $ do
        it "responds with 'forbidden' and a helpful message" $ do
          env <- ask
          (_owner, tid) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
          nobody <- let Just perms = Galley.newPermissions mempty mempty
                    in call $ createTeamMember (env ^. teBrig) (env ^. teGalley) tid perms
          callIdpCreate' (env ^. teSpar) (Just nobody) (env ^. teNewIdp)
            `shouldRespondWith` check (== 403) [aesonQQ|{"error":"you need to be team admin to create an IdP"}|]

      context "invalid metainfo url or bad answer" $ do
        it "rejects" $ do
          pending
          env <- ask
          let newidp = (env ^. teNewIdp) & nidpMetadata .~ [uri|https://www.example.com/|]
          (uid, _) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
          callIdpCreate' (env ^. teSpar) (Just uid) newidp
            `shouldRespondWith` check (== 400) [aesonQQ|{"error":"not a SAML metainfo URL or bad response"}|]

      context "invalid metainfo signature (on an XML document otherwise arbitrarily off)" $ do
        it "rejects" $ do
          pending
          env <- ask
          newIdpMetaUrl <- endpointToURL (env ^. teMockIdp)
          let newIdp = (env ^. teNewIdp) & nidpMetadata .~ newIdpMetaUrl
          (uid, _) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
          withMockIdP (unconditionallyServeFile "resources/meta-bad-sig.xml") $ do
            callIdpCreate' (env ^. teSpar) (Just uid) newIdp
              `shouldRespondWith` check (== 400) [aesonQQ|{"error":"invalid signature in response from SAML metainfo URL"}|]

      context "invalid or unresponsive login request url" $ do
        it "rejects" $ do
          pending
          env <- ask
          let newidp = (env ^. teNewIdp) & nidpRequestUri .~ [uri|https://www.example.com/|]
          (uid, _) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
          callIdpCreate' (env ^. teSpar) (Just uid) newidp
            `shouldRespondWith` check (== 400) [aesonQQ|{"error":"not a SAML SSO request URL"}|]

      context "pubkey in IdPConfig does not match the one provided in metainfo url" $ do
        it "rejects" $ do
          pending
          env <- ask
          let newidp = (env ^. teNewIdp) & nidpPublicKey .~ samplePublicKey2
          (uid, _) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
          callIdpCreate' (env ^. teSpar) (Just uid) newidp
            `shouldRespondWith` check (== 400) [aesonQQ|{"error":"public keys in request body and metainfo do not match"}|]

      context "some URLs are not https" $ do
        it "rejects (metainfo, request url)" $ do
          pending
          env <- ask
          (uid, _) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
          let newidpBadMeta   = (env ^. teNewIdp) & nidpMetadata   .~ [uri|http://www.example.com/|]
              newidpBadReqUrl = (env ^. teNewIdp) & nidpRequestUri .~ [uri|http://www.example.com/|]
              msg = [aesonQQ|{"error":"http URLs are not allowed for metainfo or request url end-point"}|]
          callIdpCreate' (env ^. teSpar) (Just uid) newidpBadMeta
            `shouldRespondWith` check (== 400) msg
          callIdpCreate' (env ^. teSpar) (Just uid) newidpBadReqUrl
            `shouldRespondWith` check (== 400) msg

      context "everything in order" $ do
        it "responds with 2xx" $ do
          pending

        it "makes IdP available for GET /identity-providers/" $ do
          pending
