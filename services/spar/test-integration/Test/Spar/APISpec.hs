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
import Brig.Types.User
import Control.Monad.Reader
import Data.ByteString.Conversion
import Data.Either (isRight)
import Data.Id
import Data.List (isInfixOf)
import Data.Maybe
import Data.String.Conversions
import Data.UUID as UUID hiding (null, fromByteString)
import Galley.Types.Teams as Galley
import Lens.Micro
import SAML2.WebSSO as SAML
import Spar.Types
import Util

import qualified Network.HTTP.Types.Status as HTTP
import qualified Spar.Intra.Brig as Intra


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


    let checkErr :: (Int -> Bool) -> TestErrorLabel -> ResponseLBS -> Bool
        checkErr statusIs label resp = statusIs (statusCode resp) && responseJSON resp == Right label

        testGetPutDelete :: (SparReq -> Maybe UserId -> IdPId -> Http ResponseLBS) -> SpecWith TestEnv
        testGetPutDelete whichone = do
          context "unknown IdP" $ do
            it "responds with 'not found'" $ do
              env <- ask
              whichone (env ^. teSpar) Nothing (IdPId UUID.nil)
                `shouldRespondWith` checkErr (== 404) "not-found"

          context "no zuser" $ do
            it "responds with 'not found'" $ do
              env <- ask
              (_, _, idp) <- createTestIdP
              whichone (env ^. teSpar) Nothing idp
                `shouldRespondWith` checkErr (== 404) "not-found"

          context "zuser has no team" $ do
            it "responds with 'not found'" $ do
              env <- ask
              (_, _, idp) <- createTestIdP
              (uid, _) <- call $ createRandomPhoneUser (env ^. teBrig)
              whichone (env ^. teSpar) (Just uid) idp
                `shouldRespondWith` checkErr (== 404) "not-found"

          context "zuser has wrong team" $ do
            it "responds with 'not found'" $ do
              env <- ask
              (_, _, idp) <- createTestIdP
              (uid, _) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
              whichone (env ^. teSpar) (Just uid) idp
                `shouldRespondWith` checkErr (== 404) "not-found"

          context "zuser is a team member, but not a team owner" $ do
            it "responds with 'forbidden' and a helpful message" $ do
              env <- ask
              (_owner, tid, idp) <- createTestIdP
              newmember <- let Just perms = newPermissions mempty mempty
                        in call $ createTeamMember (env ^. teBrig) (env ^. teGalley) tid perms
              whichone (env ^. teSpar) (Just newmember) idp
                `shouldRespondWith` checkErr (== 403) "forbidden"

    describe "GET /identity-providers/:idp" $ do
      testGetPutDelete callIdpGet'

      context "known IdP, client is team owner" $ do
        it "responds with 2xx and IdP" $ do
          env <- ask
          (uid, _, idp) <- createTestIdP
          callIdpGet' (env ^. teSpar) (Just uid) idp
            `shouldRespondWith` (\resp -> statusCode resp == 200 && isRight (responseJSON @IdP resp))

    describe "GET /identity-providers" $ do
      context "client is not team owner" $ do
        it "rejects" $ do
          pending

      context "client is team owner" $ do
        context "no idps registered" $ do
          it "returns an empty list" $ do
            pending

        context "some idps are registered" $ do
          it "returns a non-empty empty list" $ do
            pending

    describe "DELETE /identity-providers/:idp" $ do
      testGetPutDelete callIdpDelete'

      context "known IdP, client is team owner" $ do
        it "responds with 2xx and removes IdP" $ do
          env <- ask
          (uid, _, idp) <- createTestIdP
          callIdpDelete' (env ^. teSpar) (Just uid) idp
            `shouldRespondWith` \resp -> statusCode resp < 300
          callIdpGet' (env ^. teSpar) (Just uid) idp
            `shouldRespondWith` checkErr (== 404) "not-found"

    describe "PUT /identity-providers/:idp" $ do
      xdescribe "need to implement `callIdpGet'` for these tests" $ do
        let callIdpPut' :: SparReq -> Maybe UserId -> IdPId -> Http ResponseLBS
            callIdpPut' = undefined  -- (we need to change the type of 'testGetPutDelete', too, to accomodate the PUT body.)
        testGetPutDelete callIdpPut'

      context "known IdP, client is team owner" $ do
        it "responds with 2xx and updates IdP" $ do
          pending

        context "invalid body" $ do
          it "rejects" $ do
            pending  -- (only test for signature here, but make sure that the same validity tests
                     -- are performed as for POST in Spar.API.)

    describe "POST /identity-providers/:idp" $ do
      context "no zuser" $ do
        it "responds with 'not found'" $ do
          env <- ask
          callIdpCreate' (env ^. teSpar) Nothing (env ^. teNewIdp)
            `shouldRespondWith` checkErr (== 404) "not-found"

      context "zuser has no team" $ do
        it "responds with 'not found'" $ do
          env <- ask
          (uid, _) <- call $ createRandomPhoneUser (env ^. teBrig)
          callIdpCreate' (env ^. teSpar) (Just uid) (env ^. teNewIdp)
            `shouldRespondWith` checkErr (== 404) "not-found"

      context "zuser is a team member, but not a team owner" $ do
        it "responds with 'forbidden' and a helpful message" $ do
          env <- ask
          (_owner, tid) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
          newmember <- let Just perms = newPermissions mempty mempty
                       in call $ createTeamMember (env ^. teBrig) (env ^. teGalley) tid perms
          callIdpCreate' (env ^. teSpar) (Just newmember) (env ^. teNewIdp)
            `shouldRespondWith` checkErr (== 403) "forbidden"

      let createIdpMockErr :: (NewIdP -> NewIdP) -> FilePath -> HTTP.Status -> ReaderT TestEnv IO ()
          createIdpMockErr modnewidp metafile respstatus = do
            pending
            env <- ask
            metaurl <- endpointToURL (env ^. teMockIdp) "meta"
            respurl <- endpointToURL (env ^. teMockIdp) "resp"
            let newidp = (env ^. teNewIdp)
                  & nidpMetadata   .~ metaurl
                  & nidpRequestUri .~ respurl
                  & modnewidp
            (uid, _) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
            withMockIdP (serveMetaAndResp metafile respstatus) $ do
              callIdpCreate' (env ^. teSpar) (Just uid) newidp
                `shouldRespondWith` checkErr (== 400) "client-error"

      context "bad metainfo answer" $ do
        it "rejects" $ createIdpMockErr
          id
          "meta-bad.xml"
          HTTP.status200

      context "invalid metainfo signature (on an XML document otherwise arbitrarily off)" $ do
        it "rejects" $ createIdpMockErr
          id
          "meta-bad-sig.xml"
          HTTP.status200

      context "invalid or unresponsive login request url" $ do
        it "rejects" $ createIdpMockErr
          id
          "meta-good-sig.xml"
          HTTP.status400

      context "pubkey in IdPConfig does not match the one provided in metainfo url" $ do
        it "rejects" $ createIdpMockErr
          (nidpPublicKey .~ samplePublicKey2)
          "meta-good-sig.xml"
          HTTP.status200

      context "idp is in use by other team" $ do
        it "rejects" $ do
          pending

      context "everything in order" $ do
        it "responds with 2xx" $ do
          pending

        it "makes IdP available for GET /identity-providers/" $ do
          pending


    describe "test helper functions" $ do
      describe "createTeamMember" $ do
        let check :: HasCallStack => Bool -> Int -> SpecWith TestEnv
            check tryowner permsix =
              it ("works: tryowner == " <> show (tryowner, permsix)) $ do
                env <- ask
                (owner, tid, _idp) <- createTestIdP
                newmember <- if tryowner
                  then pure undefined
                  else call $ createTeamMember (env ^. teBrig) (env ^. teGalley) tid (permses !! permsix)
                rawResp <- call $ get ((env ^. teBrig)
                              . path "/self"
                              . header "Z-User" (toByteString' $ if tryowner then owner else newmember)
                              . expect2xx)
                parsedResp <- either (error . show) pure $ selfUser <$> Intra.parseResponse @SelfProfile rawResp
                liftIO $ userTeam parsedResp `shouldSatisfy` isJust

            permses :: [Permissions]
            permses = fromJust <$>
              [ Just fullPermissions
              , newPermissions mempty mempty
              ]

        sequence_ [ check tryowner perms | tryowner <- [minBound..], perms <- [0.. (length permses - 1)] ]
