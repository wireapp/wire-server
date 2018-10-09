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
import Control.Retry
import Data.ByteString.Conversion
import Data.Id
import Data.List (isInfixOf)
import Data.Maybe
import Data.String.Conversions
import Data.UUID as UUID hiding (null, fromByteString)
import Galley.Types.Teams as Galley
import GHC.Stack
import Lens.Micro
import Prelude hiding (head)
import SAML2.WebSSO as SAML
import SAML2.WebSSO.Test.MockResponse
import Spar.Types
import URI.ByteString.QQ (uri)
import Util

import qualified Data.Aeson as Aeson
import qualified Spar.Intra.Brig as Intra


spec :: SpecWith TestEnv
spec = do
  specMisc
  specMetadata
  specInitiateLogin
  specFinalizeLogin
  specBindingUsers
  specCRUDIdentityProvider
  specAux


specMisc :: SpecWith TestEnv
specMisc = do
    describe "CORS" $ do
      it "is disabled" $ do
        -- I put this there because I was playing with a CORS middleware to make swagger browsing more
        -- convenient, but went for a simpler work flow that didn't require that in the end.  I left
        -- it in so if we ever start adding CORS headers, whether by accident or intentionally, we
        -- will fall over this test and will have to extend it to document the new behavior.
        env <- ask
        get ((env ^. teSpar) . path "/i/status" . expect2xx)
          `shouldRespondWith` (\(responseHeaders -> hdrs) -> isNothing $ lookup "Access-Control-Allow-Origin" hdrs)


    describe "status" $ do
      it "brig /i/status" $ do
        env <- ask
        ping (env ^. teBrig) `shouldRespondWith` (== ())

      it "spar /i/status" $ do
        env <- ask
        ping (env ^. teSpar) `shouldRespondWith` (== ())


specMetadata :: SpecWith TestEnv
specMetadata = do
    describe "metadata" $ do
      it "metadata" $ do
        env <- ask
        get ((env ^. teSpar) . path "/sso/metadata/0d784c66-c1c6-11e8-9576-2bdb3c574a4d" . expect2xx)
          `shouldRespondWith` (\(responseBody -> Just (cs -> bdy)) -> all (`isInfixOf` bdy)
                                [ "md:SPSSODescriptor"
                                , "validUntil"
                                , "WantAssertionsSigned=\"true\""
                                ])


specInitiateLogin :: SpecWith TestEnv
specInitiateLogin = do
    describe "HEAD /sso/initiate-login/:idp" $ do
      context "unknown IdP" $ do
        it "responds with 404" $ do
          env <- ask
          let uuid = cs $ UUID.toText UUID.nil
          void . call $ head ((env ^. teSpar) . path (cs $ "/sso/initiate-login/" -/ uuid) . expect4xx)

      context "known IdP" $ do
        it "responds with 200" $ do
          env <- ask
          let idp = idPIdToST $ env ^. teIdP . idpId
          void . call $ head ((env ^. teSpar) . path (cs $ "/sso/initiate-login/" -/ idp) . expect2xx)


    describe "GET /sso/initiate-login/:idp" $ do
      context "unknown IdP" $ do
        it "responds with 'not found'" $ do
          env <- ask
          let uuid = cs $ UUID.toText UUID.nil
          get ((env ^. teSpar) . path (cs $ "/sso/initiate-login/" -/ uuid))
            `shouldRespondWith` ((== 404) . statusCode)

      let checkRespBody :: HasCallStack => ResponseLBS -> Bool
          checkRespBody (responseBody -> Just (cs -> bdy)) = all (`isInfixOf` bdy)
            [ "<html xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">"
            , "<body onload=\"document.forms[0].submit()\">"
            , "<input name=\"SAMLRequest\" type=\"hidden\" "
            ]
          checkRespBody bad = error $ show bad

      context "known IdP, no z-user" $ do  -- see 'specBindingUsers' below for the "with z-user" case.
        it "responds with authentication request and NO bind cookie" $ do
          env <- ask
          let idp = idPIdToST $ env ^. teIdP . idpId
          get ((env ^. teSpar) . path (cs $ "/sso/initiate-login/" -/ idp) . expect2xx)
            `shouldRespondWith` \resp -> checkRespBody resp && hasDeleteBindCookieHeader resp


specFinalizeLogin :: SpecWith TestEnv
specFinalizeLogin = do
    describe "POST /sso/finalize-login/:idp" $ do
      context "access denied" $ do
        it "responds with a very peculiar 'forbidden' HTTP response" $ do
          (idp, privcreds, authnreq) <- negotiateAuthnRequest
          spmeta <- getTestSPMetadata (idp ^. idpId)
          authnresp <- liftIO $ mkAuthnResponse privcreds idp spmeta authnreq False
          sparresp <- submitAuthnResponse (idp ^. idpId) authnresp
          liftIO $ do
            -- import Text.XML
            -- putStrLn $ unlines
            --   [ cs . renderLBS def { rsPretty = True } . fromSignedAuthnResponse $ authnresp
            --   , show sparresp
            --   , maybe "Nothing" cs (responseBody sparresp)
            --   ]
            statusCode sparresp `shouldBe` 200
            let bdy = maybe "" (cs @LBS @String) (responseBody sparresp)
            bdy `shouldContain` "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            bdy `shouldContain` "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
            bdy `shouldContain` "<title>wire:sso:error:forbidden</title>"
            bdy `shouldContain` "window.opener.postMessage({"
            bdy `shouldContain` "\"type\":\"AUTH_ERROR\""
            bdy `shouldContain` "\"payload\":{"
            bdy `shouldContain` "\"label\":\"forbidden\""
            bdy `shouldContain` "}, receiverOrigin)"

      context "access granted" $ do
        it "responds with a very peculiar 'allowed' HTTP response" $ do
          (idp, privcreds, authnreq) <- negotiateAuthnRequest
          spmeta <- getTestSPMetadata (idp ^. idpId)
          authnresp <- liftIO $ mkAuthnResponse privcreds idp spmeta authnreq True
          sparresp <- submitAuthnResponse (idp ^. idpId) authnresp
          liftIO $ do
            statusCode sparresp `shouldBe` 200
            let bdy = maybe "" (cs @LBS @String) (responseBody sparresp)
            bdy `shouldContain` "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            bdy `shouldContain` "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
            bdy `shouldContain` "<title>wire:sso:success</title>"
            bdy `shouldContain` "window.opener.postMessage({type: 'AUTH_SUCCESS'}, receiverOrigin)"

        context "unknown user" $ do
          it "creates the user" $ do
            pending

        context "known user A, but client device (probably a browser?) is already authenticated as another (probably non-sso) user B" $ do
          it "logs out user B, logs in user A" $ do
            pending

        context "more than one dsig cert" $ do
          it "accepts the first of two certs for signatures" $ do
            pending

          it "accepts the second of two certs for signatures" $ do
            pending

      context "unknown IdP Issuer" $ do
        it "rejects" $ do
          (idp, privcreds, authnreq) <- negotiateAuthnRequest
          spmeta <- getTestSPMetadata (idp ^. idpId)
          authnresp <- liftIO $ mkAuthnResponse
            privcreds
            (idp & idpMetadata . edIssuer .~ Issuer [uri|http://unknown-issuer/|])
            spmeta
            authnreq
            True
          sparresp <- submitAuthnResponse (idp ^. idpId) authnresp
          liftIO $ do
            statusCode sparresp `shouldBe` 404
            responseJSON sparresp `shouldBe` Right (TestErrorLabel "not-found")

      context "AuthnResponse does not match any request" $ do
        it "rejects" $ do
          pending

      context "AuthnResponse contains assertions that have been offered before" $ do
        it "rejects" $ do
          pending


specBindingUsers :: SpecWith TestEnv
specBindingUsers = describe "binding existing users to sso identities" $ do
    describe "HEAD /sso/initiate-login/:idp" $ do
      context "known IdP, running session with non-sso user" $ do
        it "responds with 200" $ do
          env <- ask
          let idp = idPIdToST $ env ^. teIdP . idpId
          void . call $ head ( (env ^. teSpar)
                             . path (cs $ "/sso/initiate-login/" -/ idp)
                             . header "Z-User" (toByteString' $ env ^. teUserId)
                             . expect2xx
                             )

      context "known IdP, running session with sso user" $ do
        it "responds with 2xx" $ do
          uid <- loginSsoUserFirstTime
          env <- ask
          let idp = idPIdToST $ env ^. teIdP . idpId
          void . call $ head ( (env ^. teSpar)
                             . header "Z-User" (toByteString' uid)
                             . path (cs $ "/sso/initiate-login/" -/ idp)
                             . expect2xx
                             )

    describe "GET /sso/initiate-login/:idp" $ do
      let checkRespBody :: HasCallStack => ResponseLBS -> Bool
          checkRespBody (responseBody -> Just (cs -> bdy)) = all (`isInfixOf` bdy)
            [ "<html xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">"
            , "<body onload=\"document.forms[0].submit()\">"
            , "<input name=\"SAMLRequest\" type=\"hidden\" "
            ]
          checkRespBody bad = error $ show bad

      context "known IdP, running session with non-sso user" $ do
        it "responds with 200 and a bind cookie" $ do
          env <- ask
          let idp = idPIdToST $ env ^. teIdP . idpId
          (uid, _) <- call $ createRandomPhoneUser (env ^. teBrig)
          get ( (env ^. teSpar)
              . header "Z-User" (toByteString' uid)
              . path (cs $ "/sso/initiate-login/" -/ idp)
              . expect2xx
              )
            `shouldRespondWith` (\resp -> checkRespBody resp && hasSetBindCookieHeader resp)

      context "known IdP, running session with sso user" $ do  -- TODO: this is *almost* the same test as above.
        it "responds with 2xx and bind cookie" $ do
          env <- ask
          let idp = idPIdToST $ env ^. teIdP . idpId
          uid <- loginSsoUserFirstTime
          get ( (env ^. teSpar)
              . header "Z-User" (toByteString' uid)
              . path (cs $ "/sso/initiate-login/" -/ idp)
              . expect2xx
              )
            `shouldRespondWith` hasSetBindCookieHeader

    let checkFinalizeLogin :: forall m. m ~ ReaderT TestEnv IO => m UserId -> m ()
        checkFinalizeLogin createUser = do
          env <- ask
          uid <- createUser
          (idp, privCreds, authnReq, Just bindCookie) <- negotiateAuthnRequest' (header "Z-User" $ toByteString' uid)
          spmeta <- getTestSPMetadata (idp ^. idpId)
          authnResp <- liftIO $ mkAuthnResponse privCreds idp spmeta authnReq True
          sparAuthnResp :: ResponseLBS
            <- submitAuthnResponse' (header "Cookie" bindCookie) (idp ^. idpId) authnResp
          liftIO $ do
            (cs @_ @String . fromJust . responseBody $ sparAuthnResp)
              `shouldContain` "<title>wire:sso:success</title>"
            responseHeaders sparAuthnResp
              `shouldSatisfy` (isJust . lookup "set-cookie")

          ssoidViaAuthResp :: UserSSOId <- do
            authnRespParsed :: AuthnResponse
              <- either error pure . parseFromDocument $ fromSignedAuthnResponse authnResp
            either error (pure . Intra.toUserSSOId) $ getUserRef authnRespParsed

          ssoidViaSelf :: UserSSOId <- do
            let probe :: m (Maybe UserSSOId)
                probe = fmap getUserSSOId . call . get $
                  ( (env ^. teBrig)
                  . header "Z-User" (toByteString' uid)
                  . path "/self"
                  . expect2xx
                  )

                getUserSSOId :: ResponseLBS -> Maybe UserSSOId
                getUserSSOId (fmap Aeson.eitherDecode . responseBody -> Just (Right selfprof))
                  = case userIdentity $ selfUser selfprof of
                      Just (SSOIdentity ssoid _ _) -> Just ssoid
                      Just (FullIdentity _ _)  -> Nothing
                      Just (EmailIdentity _)   -> Nothing
                      Just (PhoneIdentity _)   -> Nothing
                      Nothing                  -> Nothing
                getUserSSOId _ = Nothing

            Just ssoid <- liftIO $ retrying
              (exponentialBackoff 50 <> limitRetries 5)
              (\_ -> pure . isNothing)
              (\_ -> probe `runReaderT` env)
            pure ssoid

          liftIO $ ('s', ssoidViaSelf) `shouldBe` ('s', ssoidViaAuthResp)
          Just uidViaSpar <- ssoToUidSpar ssoidViaAuthResp
          liftIO $ ('u', uidViaSpar) `shouldBe` ('u', uid)

    describe "POST /sso/finalize-login/:idp" $ do
      context "AuthnResponse is fine, request contains bind cookie" $ do
        it "Sends user back to the app and adds UserSSOId to brig user" $ do
          checkFinalizeLogin (fmap fst . call . createRandomPhoneUser =<< asks (^. teBrig))

      context "known IdP, running session with sso user" $ do
        it "overwrites UserSSOId on both brig and spar" $ do
          checkFinalizeLogin loginSsoUserFirstTime


-- | FUTUREWORK: this function deletes the test IdP from the test env.  (it should probably not do
-- that, so other tests after this can still use it.)
specCRUDIdentityProvider :: SpecWith TestEnv
specCRUDIdentityProvider = do
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
            it "responds with 'client error'" $ do
              env <- ask
              let idpid = env ^. teIdP . idpId
              whichone (env ^. teSpar) Nothing idpid
                `shouldRespondWith` checkErr (== 400) "client-error"

          context "zuser has no team" $ do
            it "responds with 'no team member'" $ do
              env <- ask
              let idpid = env ^. teIdP . idpId
              (uid, _) <- call $ createRandomPhoneUser (env ^. teBrig)
              whichone (env ^. teSpar) (Just uid) idpid
                `shouldRespondWith` checkErr (== 403) "no-team-member"

          context "zuser has wrong team" $ do
            it "responds with 'no team member'" $ do
              env <- ask
              let idpid = env ^. teIdP . idpId
              (uid, _) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
              whichone (env ^. teSpar) (Just uid) idpid
                `shouldRespondWith` checkErr (== 403) "no-team-member"

          context "zuser is a team member, but not a team owner" $ do
            it "responds with 'insufficient-permissions' and a helpful message" $ do
              env <- ask
              let idpid = env ^. teIdP . idpId
                  teamid = env ^. teTeamId
              newmember <- let Just perms = newPermissions mempty mempty
                        in call $ createTeamMember (env ^. teBrig) (env ^. teGalley) teamid perms
              whichone (env ^. teSpar) (Just newmember) idpid
                `shouldRespondWith` checkErr (== 403) "insufficient-permissions"


    describe "GET /identity-providers/:idp" $ do
      testGetPutDelete callIdpGet'

      context "known IdP, client is team owner" $ do
        it "responds with 2xx and IdP" $ do
          env <- ask
          let idpid = env ^. teIdP . idpId
              userid = env ^. teUserId
          _ <- call $ callIdpGet (env ^. teSpar) (Just userid) idpid
          passes


    describe "GET /identity-providers" $ do
      context "client is not team owner" $ do
        it "rejects" $ do
          env <- ask
          (_owner :: UserId, teamid :: TeamId)
            <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
          member :: UserId
            <- let Just perms = newPermissions mempty mempty
               in call $ createTeamMember (env ^. teBrig) (env ^. teGalley) teamid perms
          callIdpGetAll' (env ^. teSpar) (Just member)
            `shouldRespondWith` ((== 403) . statusCode)

      context "client is team owner" $ do
        context "no idps registered" $ do
          it "returns an empty list" $ do
            env <- ask
            (owner :: UserId, _teamid :: TeamId)
              <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
            callIdpGetAll (env ^. teSpar) (Just owner)
              `shouldRespondWith` (null . _idplProviders)

        context "some idps are registered" $ do
          it "returns a non-empty empty list" $ do
            env <- ask
            metadata <- makeTestIdPMetadata
            (owner, _, _) <- createTestIdPFrom metadata (env ^. teMgr) (env ^. teBrig) (env ^. teGalley) (env ^. teSpar)
            callIdpGetAll (env ^. teSpar) (Just owner)
              `shouldRespondWith` (not . null . _idplProviders)


    describe "DELETE /identity-providers/:idp" $ do
      testGetPutDelete callIdpDelete'

      context "known IdP, client is team owner" $ do
        it "responds with 2xx and removes IdP" $ do
          env <- ask
          let idpid = env ^. teIdP . idpId
              userid = env ^. teUserId
          callIdpDelete' (env ^. teSpar) (Just userid) idpid
            `shouldRespondWith` \resp -> statusCode resp < 300
          callIdpGet' (env ^. teSpar) (Just userid) idpid
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


    describe "POST /identity-providers" $ do
      context "no zuser" $ do
        it "responds with 'client error'" $ do
          env <- ask
          callIdpCreate' (env ^. teSpar) Nothing (env ^. teIdP . idpMetadata)
            `shouldRespondWith` checkErr (== 400) "client-error"

      context "zuser has no team" $ do
        it "responds with 'no team member'" $ do
          env <- ask
          (uid, _) <- call $ createRandomPhoneUser (env ^. teBrig)
          callIdpCreate' (env ^. teSpar) (Just uid) (env ^. teIdP . idpMetadata)
            `shouldRespondWith` checkErr (== 403) "no-team-member"

      context "zuser is a team member, but not a team owner" $ do
        it "responds with 'insufficient-permissions' and a helpful message" $ do
          env <- ask
          (_owner, tid) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
          newmember <- let Just perms = newPermissions mempty mempty
                       in call $ createTeamMember (env ^. teBrig) (env ^. teGalley) tid perms
          callIdpCreate' (env ^. teSpar) (Just newmember) (env ^. teIdP . idpMetadata)
            `shouldRespondWith` checkErr (== 403) "insufficient-permissions"

      context "idp (identified by issuer) is in use by other team" $ do
        it "rejects" $ do
          env <- ask
          newMetadata <- makeTestIdPMetadata
          (uid1, _) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
          (uid2, _) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)

          resp1 <- call $ callIdpCreate' (env ^. teSpar) (Just uid1) newMetadata
          resp2 <- call $ callIdpCreate' (env ^. teSpar) (Just uid1) newMetadata
          resp3 <- call $ callIdpCreate' (env ^. teSpar) (Just uid2) newMetadata

          liftIO $ do
            statusCode resp1 `shouldBe` 201

            statusCode resp2 `shouldBe` 400
            responseJSON resp2 `shouldBe` Right (TestErrorLabel "idp-already-in-use")

            statusCode resp3 `shouldBe` 400
            responseJSON resp3 `shouldBe` Right (TestErrorLabel "idp-already-in-use")

      context "everything in order" $ do
        it "responds with 2xx; makes IdP available for GET /identity-providers/" $ do
          env <- ask
          metadata <- makeTestIdPMetadata
          idp <- call $ callIdpCreate (env ^. teSpar) (Just (env ^. teUserId)) metadata
          idp' <- call $ callIdpGet (env ^. teSpar) (Just (env ^. teUserId)) (idp ^. idpId)
          liftIO $ idp `shouldBe` idp'


specAux :: SpecWith TestEnv
specAux = do
    describe "test helper functions" $ do
      describe "createTeamMember" $ do
        let check :: HasCallStack => Bool -> Int -> SpecWith TestEnv
            check tryowner permsix =
              it ("works: tryowner == " <> show (tryowner, permsix)) $ do
                env <- ask
                (owner, tid) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
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




-- TODO: go through DataSpec, APISpec and check that all the tests still make sense with the new implicit mock idp.
-- TODO: what else needs to be tested, beyond the pending tests listed here?
-- TODO: what tests can go to saml2-web-sso package?
