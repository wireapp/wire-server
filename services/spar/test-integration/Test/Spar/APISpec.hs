module Test.Spar.APISpec (spec) where

import Imports hiding (head)

import Bilge
import Brig.Types.User
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Id
import Data.Proxy
import Data.String.Conversions
import Data.UUID as UUID hiding (null, fromByteString)
import Network.HTTP.Types (status200)
import SAML2.WebSSO as SAML
import SAML2.WebSSO.Test.Lenses
import SAML2.WebSSO.Test.MockResponse
import Spar.API.Types
import Spar.Types
import URI.ByteString.QQ (uri)
import Util.Core
import Util.Types

import qualified Data.ByteString.Builder as LB
import qualified Data.Text as ST
import qualified Data.ZAuth.Token as ZAuth
import qualified Galley.Types.Teams as Galley
import qualified Spar.Data as Data
import qualified Spar.Intra.Brig as Intra
import qualified Util.Scim as ScimT
import qualified Web.Cookie as Cky


spec :: SpecWith TestEnv
spec = do
  specMisc
  specMetadata
  specInitiateLogin
  specFinalizeLogin
  specBindingUsers
  specCRUDIdentityProvider
  specScimAndSAML
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


    describe "rule do disallow http idp urls." $ do
      let check :: Bool -> TestSpar ()
          check isHttps = do
            somemeta <- do
              issuer <- makeIssuer
              let nonfresh = either (error . show) id $ SAML.decode ("<?xml version=\"1.0\" encoding=\"UTF-8\"?><md:EntityDescriptor xmlns:md=\"urn:oasis:names:tc:SAML:2.0:metadata\" entityID=\"" <> (if isHttps then "https" else "http") <> "://www.okta.com/exkgxxperpx1txfkY0h7\"><md:IDPSSODescriptor WantAuthnRequestsSigned=\"false\" protocolSupportEnumeration=\"urn:oasis:names:tc:SAML:2.0:protocol\"><md:KeyDescriptor use=\"signing\"><ds:KeyInfo xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\"><ds:X509Data><ds:X509Certificate>MIIDpDCCAoygAwIBAgIGAWSx7x1HMA0GCSqGSIb3DQEBCwUAMIGSMQswCQYDVQQGEwJVUzETMBEG\nA1UECAwKQ2FsaWZvcm5pYTEWMBQGA1UEBwwNU2FuIEZyYW5jaXNjbzENMAsGA1UECgwET2t0YTEU\nMBIGA1UECwwLU1NPUHJvdmlkZXIxEzARBgNVBAMMCmRldi01MDA1MDgxHDAaBgkqhkiG9w0BCQEW\nDWluZm9Ab2t0YS5jb20wHhcNMTgwNzE5MDk0NTM1WhcNMjgwNzE5MDk0NjM0WjCBkjELMAkGA1UE\nBhMCVVMxEzARBgNVBAgMCkNhbGlmb3JuaWExFjAUBgNVBAcMDVNhbiBGcmFuY2lzY28xDTALBgNV\nBAoMBE9rdGExFDASBgNVBAsMC1NTT1Byb3ZpZGVyMRMwEQYDVQQDDApkZXYtNTAwNTA4MRwwGgYJ\nKoZIhvcNAQkBFg1pbmZvQG9rdGEuY29tMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA\nhUaQm/3dgPws1A5IjFK9ZQpj170vIqENuDG0tapAzkvk6+9vyhduGckHTeZF3k5MMlW9iix2Eg0q\na1oS/Wrq/aBf7+BH6y1MJlQnaKQ3hPL+OFvYzbnrN8k2uC2LivP7Y90dXwtN3P63rA4QSyDPYEMv\ndKSubUKX/HNsUg4I2PwHmpfWBNgoMkqe0bxQILBv+84L62IYSd6k77XXnCFb/usHpG/gY6sJsTQ2\naFl9FuJ51uf67AOj8RzPXstgtUaXbdJI0kAqKIb3j9Zv3mpPCy/GHnyB3PMalvtc1uaz1ZnwO2el\niqhwB6/8W6CPutFo1Bhq1glQIX+1OD7906iORwIDAQABMA0GCSqGSIb3DQEBCwUAA4IBAQB0h6vK\nAywJwH3g0RnocOpBvT42QW57TZ3Wzm9gbg6dQL0rB+NHDx2V0VIh51E3YHL1os9W09MreM7I74D/\nfX27r1Q3+qAsL1v3CN8WIVh9eYitBCtF7DwZmL2UXTia+GWPrabO14qAztFmTXfqNuCZej7gJd/K\n2r0KBiZtZ6o58WBREW2F70a6nN6Nk1yjzBkDTJMMf8OMXHphTaalMBXojN9W6HEDpGBE0qY7c70P\nqvfUEzd8wHWcDxo6+3jajajelk0V4rg7Cqxccr+WwjYtENEuQypNG2mbI52iPZked0QWKy0WzhSM\nw5wjJ+QDG31vJInAB2769C2KmhPDyNhU</ds:X509Certificate></ds:X509Data></ds:KeyInfo></md:KeyDescriptor><md:NameIDFormat>urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified</md:NameIDFormat><md:NameIDFormat>urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress</md:NameIDFormat><md:SingleSignOnService Binding=\"urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST\" Location=\"" <> (if isHttps then "https" else "http") <> "://dev-500508.oktapreview.com/app/wireswissgmbhdev500508_z1tejapsbeyonce_1/exkgxxperpx1txfkY0h7/sso/saml\"/><md:SingleSignOnService Binding=\"urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Redirect\" Location=\"" <> (if isHttps then "https" else "http") <> "://dev-500508.oktapreview.com/app/wireswissgmbhdev500508_z1tejapsbeyonce_1/exkgxxperpx1txfkY0h7/sso/saml\"/></md:IDPSSODescriptor></md:EntityDescriptor>")
              pure $ nonfresh & edIssuer .~ issuer
            env <- ask
            uid <- fst <$> call (createUserWithTeam (env ^. teBrig) (env ^. teGalley))
            resp <- call $ callIdpCreate' (env ^. teSpar) (Just uid) somemeta
            liftIO $ statusCode resp `shouldBe` if isHttps then 201 else 400

      it "does not trigger on https urls" $ check True
      it "does trigger on http urls" $ check False


specMetadata :: SpecWith TestEnv
specMetadata = do
    describe "metadata" $ do
      it "metadata" $ do
        env <- ask
        get ((env ^. teSpar) . path "/sso/metadata" . expect2xx)
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
          (_, _, idPIdToST . (^. idpId) -> idp) <- registerTestIdP
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
          (_, _, idPIdToST . (^. idpId) -> idp) <- registerTestIdP
          resp <- call $ get ((env ^. teSpar) . path (cs $ "/sso/initiate-login/" -/ idp) . expect2xx)
          liftIO $ do
            resp `shouldSatisfy` checkRespBody
            hasDeleteBindCookieHeader resp `shouldBe` Right ()


specFinalizeLogin :: SpecWith TestEnv
specFinalizeLogin = do
    describe "POST /sso/finalize-login" $ do
      context "access denied" $ do
        it "responds with a very peculiar 'forbidden' HTTP response" $ do
          (_, _, idp) <- registerTestIdP
          (privcreds, authnreq) <- negotiateAuthnRequest idp
          spmeta <- getTestSPMetadata
          authnresp <- runSimpleSP $ mkAuthnResponse privcreds idp spmeta authnreq False
          sparresp <- submitAuthnResponse authnresp
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
            hasPersistentCookieHeader sparresp `shouldBe` Left "no set-cookie header"

        context "user has been deleted" $ do
          it "responds with 'forbidden'" $ do
            pendingWith "or do we want to un-delete the user?  or create a new one?"

      context "access granted" $ do
        let loginSuccess :: HasCallStack => ResponseLBS -> TestSpar ()
            loginSuccess sparresp = liftIO $ do
              statusCode sparresp `shouldBe` 200
              let bdy = maybe "" (cs @LBS @String) (responseBody sparresp)
              bdy `shouldContain` "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
              bdy `shouldContain` "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
              bdy `shouldContain` "<title>wire:sso:success</title>"
              bdy `shouldContain` "window.opener.postMessage({type: 'AUTH_SUCCESS'}, receiverOrigin)"
              hasPersistentCookieHeader sparresp `shouldBe` Right ()

        context "happy flow" $ do
          it "responds with a very peculiar 'allowed' HTTP response" $ do
            (_, _, idp) <- registerTestIdP
            spmeta <- getTestSPMetadata
            (privcreds, authnreq) <- negotiateAuthnRequest idp
            authnresp <- runSimpleSP $ mkAuthnResponse privcreds idp spmeta authnreq True
            loginSuccess =<< submitAuthnResponse authnresp

        context "user is created once, then deleted in team settings, then can login again." $ do
          it "responds with 'allowed'" $ do
            (ownerid, teamid, idp) <- registerTestIdP
            spmeta <- getTestSPMetadata

            -- first login
            newUserAuthnResp :: SignedAuthnResponse <- do
              (privcreds, authnreq) <- negotiateAuthnRequest idp
              authnresp <- runSimpleSP $ mkAuthnResponse privcreds idp spmeta authnreq True
              loginSuccess =<< submitAuthnResponse authnresp
              pure $ authnresp

            let newUserRef@(UserRef _ subj) = either (error . show) (^. userRefL) $
                  parseFromDocument (fromSignedAuthnResponse newUserAuthnResp)

            -- remove user from team settings
            do
              env <- ask
              newUserId <- getUserIdViaRef newUserRef
              _ <- call . get $
                ( (env ^. teGalley)
                . header "Z-User" (toByteString' ownerid)
                . header "Z-Connection" "fake"
                . paths ["teams", toByteString' teamid, "members"]
                . expect2xx
                )
              void . call . delete $
                ( (env ^. teGalley)
                . header "Z-User" (toByteString' ownerid)
                . header "Z-Connection" "fake"
                . paths ["teams", toByteString' teamid, "members", toByteString' newUserId]
                . Bilge.json (Galley.newTeamMemberDeleteData (Just defPassword))
                . expect2xx
                )
              liftIO $ threadDelay 100000  -- make sure deletion is done.  if we don't want to take
                                           -- the time, we should find another way to robustly
                                           -- confirm that deletion has compelted in the background.

            -- second login
            do
              (privcreds, authnreq) <- negotiateAuthnRequest idp
              authnresp <- runSimpleSP $ mkAuthnResponseWithSubj subj privcreds idp spmeta authnreq True
              loginSuccess =<< submitAuthnResponse authnresp

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
          (_, _, idp) <- registerTestIdP
          (privcreds, authnreq) <- negotiateAuthnRequest idp
          spmeta <- getTestSPMetadata
          authnresp <- runSimpleSP $ mkAuthnResponse
            privcreds
            (idp & idpMetadata . edIssuer .~ Issuer [uri|http://unknown-issuer/|])
            spmeta
            authnreq
            True
          sparresp <- submitAuthnResponse authnresp
          liftIO $ do
            statusCode sparresp `shouldBe` 404
            responseJsonEither sparresp `shouldBe` Right (TestErrorLabel "not-found")

      context "AuthnResponse does not match any request" $ do
        it "rejects" $ do
          pending

      context "AuthnResponse contains assertions that have been offered before" $ do
        it "rejects" $ do
          pending


specBindingUsers :: SpecWith TestEnv
specBindingUsers = describe "binding existing users to sso identities" $ do
    describe "HEAD /sso/initiate-bind/:idp" $ do
      context "known IdP, running session with non-sso user" $ do
        it "responds with 200" $ do
          env <- ask
          (owner, _, idPIdToST . (^. idpId) -> idp) <- registerTestIdP
          void . call $ head ( (env ^. teSpar)
                             . path (cs $ "/sso-initiate-bind/" -/ idp)
                             . header "Z-User" (toByteString' owner)
                             . expect2xx
                             )

      context "known IdP, running session with sso user" $ do
        it "responds with 2xx" $ do
          (_, _, idp) <- registerTestIdP
          uid <- loginSsoUserFirstTime idp
          env <- ask
          void . call $ head ( (env ^. teSpar)
                             . header "Z-User" (toByteString' uid)
                             . path (cs $ "/sso-initiate-bind/" -/ (idPIdToST $ idp ^. idpId))
                             . expect2xx
                             )

    let checkInitiateLogin :: HasCallStack => Bool -> TestSpar UserId -> SpecWith TestEnv
        checkInitiateLogin hasZUser createUser = do
          let testmsg = if hasZUser
                        then "responds with 200 and a bind cookie"
                        else "responds with 403 and 'bind-without-auth'"

              checkRespBody :: HasCallStack => ResponseLBS -> Bool
              checkRespBody (responseBody -> Just (cs -> bdy)) = all (`isInfixOf` bdy)
                [ "<html xml:lang=\"en\" xmlns=\"http://www.w3.org/1999/xhtml\">"
                , "<body onload=\"document.forms[0].submit()\">"
                , "<input name=\"SAMLRequest\" type=\"hidden\" "
                ]
              checkRespBody bad = error $ show bad

          it testmsg $ do
            env <- ask
            (_, _, idPIdToST . (^. idpId) -> idp) <- registerTestIdP
            uid <- createUser
            resp <- call $ get ( (env ^. teSpar)
                               . (if hasZUser then header "Z-User" (toByteString' uid) else id)
                               . path (cs $ "/sso-initiate-bind/" -/ idp)
                               )
            liftIO $ if hasZUser
              then do
                statusCode resp `shouldBe` 200
                resp `shouldSatisfy` checkRespBody
                hasSetBindCookieHeader resp `shouldBe` Right ()
              else do
                statusCode resp `shouldBe` 403
                resp `shouldSatisfy` (not . checkRespBody)
                hasSetBindCookieHeader resp `shouldBe` Left "no set-cookie header"
                responseJsonEither resp `shouldBe` Right (TestErrorLabel "bind-without-auth")

    describe "GET /sso-initiate-bind/:idp" $ do
      context "known IdP, running session without authentication" $ do
        checkInitiateLogin False (fmap fst . call . createRandomPhoneUser =<< asks (^. teBrig))

      context "known IdP, running session with non-sso user" $ do
        checkInitiateLogin True (fmap fst . call . createRandomPhoneUser =<< asks (^. teBrig))

      context "known IdP, running session with sso user" $ do
        checkInitiateLogin True (registerTestIdP >>= \(_, _, idp) -> loginSsoUserFirstTime idp)

    describe "POST /sso/finalize-login" $ do
      let checkGrantingAuthnResp :: HasCallStack => UserId -> SignedAuthnResponse -> ResponseLBS -> TestSpar ()
          checkGrantingAuthnResp uid sparrq sparresp = do
            checkGrantingAuthnResp' sparresp

            ssoidViaAuthResp <- getSsoidViaAuthResp sparrq
            ssoidViaSelf <- getSsoidViaSelf uid

            liftIO $ ('s', ssoidViaSelf) `shouldBe` ('s', ssoidViaAuthResp)
            Just uidViaSpar <- ssoToUidSpar ssoidViaAuthResp
            liftIO $ ('u', uidViaSpar) `shouldBe` ('u', uid)

          checkGrantingAuthnResp' :: HasCallStack => ResponseLBS -> TestSpar ()
          checkGrantingAuthnResp' sparresp = do
            liftIO $ do
              (cs @_ @String . fromJust . responseBody $ sparresp)
                `shouldContain` "<title>wire:sso:success</title>"
              hasPersistentCookieHeader sparresp `shouldBe` Right ()

          checkDenyingAuthnResp :: HasCallStack => ResponseLBS -> ST -> TestSpar ()
          checkDenyingAuthnResp sparresp errorlabel = do
            liftIO $ do
              (cs @_ @String . fromJust . responseBody $ sparresp)
                `shouldContain` ("<title>wire:sso:error:" <> cs errorlabel <> "</title>")
              hasPersistentCookieHeader sparresp `shouldBe` Left "no set-cookie header"

          getSsoidViaAuthResp :: HasCallStack => SignedAuthnResponse -> TestSpar UserSSOId
          getSsoidViaAuthResp aresp = do
            parsed :: AuthnResponse
              <- either error pure . parseFromDocument $ fromSignedAuthnResponse aresp
            either error (pure . Intra.toUserSSOId) $ getUserRef parsed

          initialBind :: HasCallStack => UserId -> IdP -> TestSpar (NameID, SignedAuthnResponse, ResponseLBS)
          initialBind = initialBind' Just

          initialBind'
            :: HasCallStack => (Cky.Cookies -> Maybe Cky.Cookies)
            -> UserId -> IdP -> TestSpar (NameID, SignedAuthnResponse, ResponseLBS)
          initialBind' tweakcookies uid idp = do
            subj <- nextSubject
            (authnResp, sparAuthnResp) <- reBindSame' tweakcookies uid idp subj
            pure (subj, authnResp, sparAuthnResp)

          reBindSame :: HasCallStack => UserId -> IdP -> NameID -> TestSpar (SignedAuthnResponse, ResponseLBS)
          reBindSame = reBindSame' Just

          reBindSame'
            :: HasCallStack => (Cky.Cookies -> Maybe Cky.Cookies)
            -> UserId -> IdP -> NameID -> TestSpar (SignedAuthnResponse, ResponseLBS)
          reBindSame' tweakcookies uid idp subj = do
            (privCreds, authnReq, Just (SimpleSetCookie bindCky)) <- do
              negotiateAuthnRequest' DoInitiateBind idp (header "Z-User" $ toByteString' uid)
            spmeta <- getTestSPMetadata
            authnResp <- runSimpleSP $ mkAuthnResponseWithSubj subj privCreds idp spmeta authnReq True
            let cookiehdr = case tweakcookies [(Cky.setCookieName bindCky, Cky.setCookieValue bindCky)] of
                  Just val -> header "Cookie" . cs . LB.toLazyByteString . Cky.renderCookies $ val
                  Nothing  -> id
            sparAuthnResp :: ResponseLBS
              <- submitAuthnResponse' cookiehdr authnResp
            pure (authnResp, sparAuthnResp)

          reBindDifferent :: HasCallStack => UserId -> TestSpar (SignedAuthnResponse, ResponseLBS)
          reBindDifferent uid = do
            env <- ask
            idp <- call . callIdpCreate (env ^. teSpar) (Just uid) =<< makeTestIdPMetadata
            (_, authnResp, sparAuthnResp) <- initialBind uid idp
            pure (authnResp, sparAuthnResp)


      context "initial bind" $ do
        it "allowed" $ do
          (uid, _, idp)                 <- registerTestIdP
          (_, authnResp, sparAuthnResp) <- initialBind uid idp
          checkGrantingAuthnResp uid authnResp sparAuthnResp

      context "re-bind to same UserRef" $ do
        it "allowed" $ do
          (uid, _, idp)      <- registerTestIdP
          (subj, _, _)       <- initialBind uid idp
          (sparrq, sparresp) <- reBindSame uid idp subj
          checkGrantingAuthnResp uid sparrq sparresp

      context "re-bind to new UserRef from different IdP" $ do
        it "allowed" $ do
          (uid, _, idp)      <- registerTestIdP
          _                  <- initialBind uid idp
          (sparrq, sparresp) <- reBindDifferent uid
          checkGrantingAuthnResp uid sparrq sparresp

      context "bind to UserRef in use by other wire user" $ do
        it "forbidden" $ do
          env <- ask
          (uid, teamid, idp) <- registerTestIdP
          (subj, _, _)       <- initialBind uid idp
          uid'               <- let Just perms = Galley.newPermissions mempty mempty
                                in call $ createTeamMember (env ^. teBrig) (env ^. teGalley) teamid perms
          (_, sparresp)      <- reBindSame uid' idp subj
          checkDenyingAuthnResp sparresp "subject-id-taken"

      context "bind to UserRef from different team" $ do
        it "forbidden" $ do
          (uid, _, _) <- registerTestIdP
          (_, _, idp) <- registerTestIdP
          (_, _, sparresp) <- initialBind uid idp
          checkDenyingAuthnResp sparresp "bad-team"

      describe "cookie corner cases" $ do
        -- attempt to bind with different 'Cookie' headers in the request to finalize-login.  if the
        -- zbind cookie cannot be found, the user is created from scratch, and the old, existing one
        -- is "detached".  if the zbind cookie is found, the binding is successful.
        let check :: HasCallStack => (Cky.Cookies -> Maybe Cky.Cookies) -> Bool -> SpecWith TestEnv
            check tweakcookies bindsucceeds = do
              it (if bindsucceeds then "binds existing user" else "creates new user") $ do
                (uid, _, idp) <- registerTestIdP
                (subj :: NameID, sparrq, sparresp) <- initialBind' tweakcookies uid idp
                checkGrantingAuthnResp' sparresp

                uid' <- getUserIdViaRef $ UserRef (idp ^. idpMetadata . edIssuer) subj
                checkGrantingAuthnResp uid' sparrq sparresp
                liftIO $ (if bindsucceeds then shouldBe else shouldNotBe) uid' uid

            addAtBeginning :: Cky.SetCookie -> Cky.Cookies -> Cky.Cookies
            addAtBeginning cky = ((Cky.setCookieName cky, Cky.setCookieValue cky):)

            addAtEnd :: Cky.SetCookie -> Cky.Cookies -> Cky.Cookies
            addAtEnd cky = (<> [(Cky.setCookieName cky, Cky.setCookieValue cky)])

            cky1, cky2, cky3 :: Cky.SetCookie
            cky1 = Cky.def { Cky.setCookieName = "cky1", Cky.setCookieValue = "val1" }
            cky2 = Cky.def { Cky.setCookieName = "cky2", Cky.setCookieValue = "val2" }
            cky3 = Cky.def { Cky.setCookieName = "cky3", Cky.setCookieValue = "val3" }

        context "with no cookies header in the request" $ do
          check (const Nothing) False

        context "with empty cookies header in the request" $ do
          check (const $ Just mempty) False

        context "with no bind cookie and one other cookie in the request" $ do
          check (\_ -> Just $ addAtBeginning cky1 mempty) False

        context "with bind cookie and one other cookie in the request" $ do
          check (\bindcky -> Just $ addAtBeginning cky1 bindcky) True

        context "with bind cookie and two other cookies in the request" $ do
          check (\bindcky -> Just . addAtEnd cky1 . addAtEnd cky2 . addAtBeginning cky3 $ bindcky) True


specCRUDIdentityProvider :: SpecWith TestEnv
specCRUDIdentityProvider = do
    let checkErr :: HasCallStack => (Int -> Bool) -> TestErrorLabel -> ResponseLBS -> Bool
        checkErr statusIs label resp = statusIs (statusCode resp) && responseJsonEither resp == Right label

        testGetPutDelete :: HasCallStack => (SparReq -> Maybe UserId -> IdPId -> Http ResponseLBS) -> SpecWith TestEnv
        testGetPutDelete whichone = do
          context "unknown IdP" $ do
            it "responds with 'not found'" $ do
              env <- ask
              whichone (env ^. teSpar) Nothing (IdPId UUID.nil)
                `shouldRespondWith` checkErr (== 404) "not-found"

          context "no zuser" $ do
            it "responds with 'client error'" $ do
              env <- ask
              (_, _, (^. idpId) -> idpid) <- registerTestIdP
              whichone (env ^. teSpar) Nothing idpid
                `shouldRespondWith` checkErr (== 400) "client-error"

          context "zuser has no team" $ do
            it "responds with 'no team member'" $ do
              env <- ask
              (_, _, (^. idpId) -> idpid) <- registerTestIdP
              (uid, _) <- call $ createRandomPhoneUser (env ^. teBrig)
              whichone (env ^. teSpar) (Just uid) idpid
                `shouldRespondWith` checkErr (== 403) "no-team-member"

          context "zuser has wrong team" $ do
            it "responds with 'no team member'" $ do
              env <- ask
              (_, _, (^. idpId) -> idpid) <- registerTestIdP
              (uid, _) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
              whichone (env ^. teSpar) (Just uid) idpid
                `shouldRespondWith` checkErr (== 403) "no-team-member"

          context "zuser is a team member, but not a team owner" $ do
            it "responds with 'insufficient-permissions' and a helpful message" $ do
              env <- ask
              (_, teamid, (^. idpId) -> idpid) <- registerTestIdP
              newmember <- let Just perms = Galley.newPermissions mempty mempty
                        in call $ createTeamMember (env ^. teBrig) (env ^. teGalley) teamid perms
              whichone (env ^. teSpar) (Just newmember) idpid
                `shouldRespondWith` checkErr (== 403) "insufficient-permissions"

        -- Authenticate via sso, and assign owner status to the thus created user.  (This
        -- doesn't work via the cookie, since we don't talk to nginz here, so we assume there
        -- is only one user in the team, which is the original owner.)
        mkSsoOwner :: UserId -> TeamId -> IdP -> TestSpar UserId
        mkSsoOwner firstOwner tid idp = do
          spmeta <- getTestSPMetadata
          (privcreds, authnreq) <- negotiateAuthnRequest idp
          authnresp <- runSimpleSP $ mkAuthnResponse privcreds idp spmeta authnreq True
          loginresp <- submitAuthnResponse authnresp
          liftIO $ responseStatus loginresp `shouldBe` status200
          [ssoOwner] <- filter (/= firstOwner) <$> getTeamMembers firstOwner tid
          promoteTeamMember firstOwner tid ssoOwner
          pure ssoOwner


    describe "GET /identity-providers/:idp" $ do
      testGetPutDelete callIdpGet'

      context "known IdP, client is team owner" $ do
        it "responds with 2xx and IdP" $ do
          env <- ask
          (owner, _, (^. idpId) -> idpid) <- registerTestIdP
          _ <- call $ callIdpGet (env ^. teSpar) (Just owner) idpid
          passes

      context "known IdP, client is team owner (authenticated via sso, user without email)" $ do
        it "responds with 2xx and IdP" $ do
          env <- ask
          (firstOwner, tid, idp) <- registerTestIdP
          ssoOwner <- mkSsoOwner firstOwner tid idp
          _ <- call $ callIdpGet (env ^. teSpar) (Just ssoOwner) (idp ^. idpId)
          passes


    describe "GET /identity-providers" $ do
      context "client is not team owner" $ do
        it "rejects" $ do
          env <- ask
          (_owner :: UserId, teamid :: TeamId)
            <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
          member :: UserId
            <- let Just perms = Galley.newPermissions mempty mempty
               in call $ createTeamMember (env ^. teBrig) (env ^. teGalley) teamid perms
          callIdpGetAll' (env ^. teSpar) (Just member)
            `shouldRespondWith` checkErr (== 403) "insufficient-permissions"

      context "no idps registered" $ do
        context "client is team owner" $ do
          it "returns an empty list" $ do
            env <- ask
            (owner :: UserId, _teamid :: TeamId)
              <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
            callIdpGetAll (env ^. teSpar) (Just owner)
              `shouldRespondWith` (null . _idplProviders)

      context "some idps are registered" $ do
        context "client is team owner with email" $ do
          it "returns a non-empty empty list" $ do
            env <- ask
            metadata <- makeTestIdPMetadata
            (owner, _, _) <- registerTestIdPFrom metadata (env ^. teMgr) (env ^. teBrig) (env ^. teGalley) (env ^. teSpar)
            callIdpGetAll (env ^. teSpar) (Just owner)
              `shouldRespondWith` (not . null . _idplProviders)

        context "client is team owner without email" $ do
          it "returns a non-empty empty list" $ do
            env <- ask
            metadata <- makeTestIdPMetadata
            (firstOwner, tid, idp) <- registerTestIdPFrom metadata (env ^. teMgr) (env ^. teBrig) (env ^. teGalley) (env ^. teSpar)
            ssoOwner <- mkSsoOwner firstOwner tid idp
            callIdpGetAll (env ^. teSpar) (Just ssoOwner)
              `shouldRespondWith` (not . null . _idplProviders)


    describe "DELETE /identity-providers/:idp" $ do
      testGetPutDelete callIdpDelete'

      context "known IdP, client is team owner" $ do
        context "without email" $ it "responds with 2xx and removes IdP" $ do
          env <- ask
          (userid, _, (^. idpId) -> idpid) <- registerTestIdP
          callIdpDelete' (env ^. teSpar) (Just userid) idpid
            `shouldRespondWith` \resp -> statusCode resp < 300
          callIdpGet' (env ^. teSpar) (Just userid) idpid
            `shouldRespondWith` checkErr (== 404) "not-found"

        context "with email" $ it "responds with 2xx and removes IdP" $ do
          env <- ask
          (firstOwner, tid, idp) <- registerTestIdP
          ssoOwner <- mkSsoOwner firstOwner tid idp
          callIdpDelete' (env ^. teSpar) (Just ssoOwner) (idp ^. idpId)
            `shouldRespondWith` \resp -> statusCode resp < 300
          callIdpGet' (env ^. teSpar) (Just ssoOwner) (idp ^. idpId)
            `shouldRespondWith` checkErr (== 404) "not-found"


    -- there are no routes for PUT yet.
    xdescribe "PUT /identity-providers/:idp" $ do
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

      it "also works with sso-authenticated users (see above)" $ pending


    describe "POST /identity-providers" $ do
      context "sso disabled for team" $ do
        it "responds with 403 forbidden" $ do
          env <- ask
          (uid, _tid) <- call $ createUserWithTeamDisableSSO (env ^. teBrig) (env ^. teGalley)
          (callIdpCreate' (env ^. teSpar) (Just uid) =<< makeTestIdPMetadata)
            `shouldRespondWith` checkErr (== 403) "sso-disabled"

      context "bad xml" $ do
        it "responds with a 'client error'" $ do
          env <- ask
          callIdpCreateRaw' (env ^. teSpar) Nothing "application/xml" "@@ bad xml ###"
            `shouldRespondWith` checkErr (== 400) "invalid-metadata"

      context "no zuser" $ do
        it "responds with 'client error'" $ do
          env <- ask
          idpmeta <- makeTestIdPMetadata
          callIdpCreate' (env ^. teSpar) Nothing idpmeta
            `shouldRespondWith` checkErr (== 400) "client-error"

      context "zuser has no team" $ do
        it "responds with 'no team member'" $ do
          env <- ask
          (uid, _) <- call $ createRandomPhoneUser (env ^. teBrig)
          idpmeta <- makeTestIdPMetadata
          callIdpCreate' (env ^. teSpar) (Just uid) idpmeta
            `shouldRespondWith` checkErr (== 403) "no-team-member"

      context "zuser is a team member, but not a team owner" $ do
        it "responds with 'insufficient-permissions' and a helpful message" $ do
          env <- ask
          (_owner, tid, idp) <- registerTestIdP
          newmember <- let Just perms = Galley.newPermissions mempty mempty
                       in call $ createTeamMember (env ^. teBrig) (env ^. teGalley) tid perms
          callIdpCreate' (env ^. teSpar) (Just newmember) (idp ^. idpMetadata)
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
            responseJsonEither resp2 `shouldBe` Right (TestErrorLabel "idp-already-in-use")

            statusCode resp3 `shouldBe` 400
            responseJsonEither resp3 `shouldBe` Right (TestErrorLabel "idp-already-in-use")

      context "client is owner with email" $ do
        it "responds with 2xx; makes IdP available for GET /identity-providers/" $ do
          env <- ask
          (owner, _) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
          metadata <- makeTestIdPMetadata
          idp <- call $ callIdpCreate (env ^. teSpar) (Just owner) metadata
          idp' <- call $ callIdpGet (env ^. teSpar) (Just owner) (idp ^. idpId)
          rawmeta <- runSparCass $ Data.getIdPRawMetadata (idp ^. idpId)
          liftIO $ do
            idp `shouldBe` idp'
            let prefix = "<EntityDescriptor xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names"
            (ST.take (ST.length prefix) <$> rawmeta) `shouldBe` (Just prefix)

      context "client is owner without email" $ do
        it "responds with 2xx; makes IdP available for GET /identity-providers/" $ do
          pending

      describe "with json body" $ do
        context "bad json" $ do
          it "responds with a 'client error'" $ do
            env <- ask
            callIdpCreateRaw' (env ^. teSpar) Nothing "application/json" "@@ bad json ###"
              `shouldRespondWith` checkErr (== 400) "invalid-metadata"

        context "good json" $ do
          it "responds with 2xx; makes IdP available for GET /identity-providers/" $ do
            env <- ask
            (owner, _) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
            metadata <- Data.Aeson.encode . (IdPMetadataValue mempty) <$> makeTestIdPMetadata
            idp <- call $ callIdpCreateRaw (env ^. teSpar) (Just owner) "application/json" metadata
            idp' <- call $ callIdpGet (env ^. teSpar) (Just owner) (idp ^. idpId)
            rawmeta <- runSparCass $ Data.getIdPRawMetadata (idp ^. idpId)
            liftIO $ do
              idp `shouldBe` idp'
              let prefix = "<EntityDescriptor xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names"
              (ST.take (ST.length prefix) <$> rawmeta) `shouldBe` (Just prefix)


specScimAndSAML :: SpecWith TestEnv
specScimAndSAML = do
    it "SCIM and SAML work together and SCIM-created users can login" $ do
      env <- ask

      -- create a user via scim
      (tok, (_, _, idp))                <- ScimT.registerIdPAndScimToken
      (usr, subj)                       <- ScimT.randomScimUserWithSubject
      scimStoredUser                    <- ScimT.createUser tok usr
      let userid     :: UserId           = ScimT.scimUserId scimStoredUser
          userref    :: UserRef          = UserRef tenant subject
          tenant     :: Issuer           = idp ^. idpMetadata . edIssuer
          subject    :: NameID           = either (error . show) id $
                                           mkNameID subj Nothing Nothing Nothing

      -- UserRef maps onto correct UserId in spar (and back).
      userid' <- getUserIdViaRef' userref
      liftIO $ ('i', userid') `shouldBe` ('i', Just userid)

      userssoid <- getSsoidViaSelf' userid
      liftIO $ ('r', Intra.fromUserSSOId <$> userssoid) `shouldBe` ('r', Just (Right userref))

      -- login a user for the first time with the scim-supplied credentials
      (privcreds, authnreq) <- negotiateAuthnRequest idp
      spmeta <- getTestSPMetadata
      authnresp  :: SignedAuthnResponse <- runSimpleSP $ mkAuthnResponseWithSubj subject privcreds idp spmeta authnreq True
      sparresp   :: ResponseLBS         <- submitAuthnResponse authnresp

      -- user should receive a cookie
      liftIO $ statusCode sparresp `shouldBe` 200
      setcky :: SAML.SimpleSetCookie "zuid"
        <- either error pure $ Util.Core.getCookie (Proxy @"zuid") sparresp

      -- /access with that cookie should give us a token
      let ckyraw = cookieRaw (Cky.setCookieName setcky') (Cky.setCookieValue setcky')
          setcky' = fromSimpleSetCookie setcky

      accessresp <- call . post $ (env ^. teBrig) . path "/access" . ckyraw
      token :: Text <- liftIO $ do
        statusCode accessresp `shouldBe` 200
        bdy :: LBS   <- maybe (error "no body") pure $ responseBody accessresp
        val :: Value <- either error pure            $ eitherDecode bdy
        maybe (error "no access token") pure         $ val ^? key "access_token" . _String

      -- token should contain the expected userid
      userid'' <- do
        parsed :: ZAuth.Token ZAuth.Access
          <- maybe (error "bad access token") pure . fromByteString . cs $ token
        pure $ Id (parsed ^. ZAuth.body . ZAuth.userId)
      liftIO $ userid'' `shouldBe` userid

      -- /self should contain the expected UserSSOId
      self :: ResponseLBS
        <- call . get $ (env ^. teBrig)
           . path "/self"
           . header "Z-User" (toByteString' userid'')
      selfbdy :: SelfProfile
        <- do
             bdy :: LBS <- maybe (error "no self body") pure $ responseBody self
             either error pure $ eitherDecode bdy
      liftIO $ userId (selfUser selfbdy) `shouldBe` userid


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

            permses :: [Galley.Permissions]
            permses = fromJust <$>
              [ Just Galley.fullPermissions
              , Galley.newPermissions mempty mempty
              ]

        sequence_ [ check tryowner perms | tryowner <- [minBound..], perms <- [0.. (length permses - 1)] ]




-- TODO: go through DataSpec, APISpec and check that all the tests still make sense with the new implicit mock idp.
-- TODO: what else needs to be tested, beyond the pending tests listed here?
-- TODO: what tests can go to saml2-web-sso package?
