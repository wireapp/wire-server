{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.Spar.AppSpec
  ( spec,
  )
where

import Bilge
import Control.Lens
import qualified Data.ByteString.Builder as Builder
import Data.Id
import qualified Data.List as List
import Data.String.Conversions
import Imports
import SAML2.WebSSO as SAML
import qualified SAML2.WebSSO.Test.MockResponse as SAML
import qualified Servant
import qualified Spar.App as Spar
import Spar.Orphans ()
import qualified Spar.Sem.SAMLUserStore as SAMLUserStore
import qualified Text.XML as XML
import qualified Text.XML.DSig as DSig
import URI.ByteString as URI
import URI.ByteString.QQ (uri)
import Util
import Web.Cookie
import Wire.API.User.IdentityProvider (IdP)
import qualified Wire.API.User.IdentityProvider as User

spec :: SpecWith TestEnv
spec = describe "accessVerdict" $ do
  context "web" $ do
    context "invalid idp" $ do
      it "responds with status 200 and a valid html page with constant expected title." $ do
        pending
    context "denied" $ do
      it "responds with status 200 and a valid html page with constant expected title." $ do
        env <- ask
        (owner, _teamId) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
        idp <- registerTestIdP owner
        (Nothing, ResponseVerdict outcome, _, _) <- requestAccessVerdict idp False mkAuthnReqWeb
        liftIO $ do
          Servant.errHTTPCode outcome `shouldBe` 200
          Servant.errReasonPhrase outcome `shouldBe` "forbidden"
          ('1', cs @LByteString @String (Servant.errBody outcome))
            `shouldSatisfy` (("<title>wire:sso:error:forbidden</title>" `List.isInfixOf`) . snd)
          ('2', XML.parseLBS XML.def $ Servant.errBody outcome)
            `shouldSatisfy` (isRight . snd)
    context "granted" $ do
      it "responds with status 200 and a valid html page with constant expected title." $ do
        env <- ask
        (owner, _teamId) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
        idp <- registerTestIdP owner
        (Just _, ResponseVerdict outcome, _, _) <- requestAccessVerdict idp True mkAuthnReqWeb
        liftIO $ do
          Servant.errHTTPCode outcome `shouldBe` 200
          Servant.errReasonPhrase outcome `shouldBe` "success"
          ('1', cs @LByteString @String (Servant.errBody outcome))
            `shouldSatisfy` (("<title>wire:sso:success</title>" `List.isInfixOf`) . snd)
          ('2', XML.parseLBS XML.def (Servant.errBody outcome))
            `shouldSatisfy` (isRight . snd)
          ('3', List.lookup "Set-Cookie" . Servant.errHeaders $ outcome)
            `shouldSatisfy` (isJust . snd)
  context "mobile" $ do
    context "invalid idp" $ do
      it "responds with status 303 with appropriate details." $ do
        pending
    context "denied" $ do
      it "responds with status 303 with appropriate details." $ do
        env <- ask
        (owner, _teamId) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
        idp <- registerTestIdP owner
        (Nothing, ResponseVerdict outcome, loc, qry) <- requestAccessVerdict idp False mkAuthnReqMobile
        liftIO $ do
          Servant.errHTTPCode outcome `shouldBe` 303
          Servant.errReasonPhrase outcome `shouldBe` "forbidden"
          Servant.errBody outcome `shouldBe` "[\"No Bearer SubjectConfirmation\",\"no AuthnStatement\"]"
          uriScheme loc `shouldBe` URI.Scheme "wire"
          List.lookup "userid" qry `shouldBe` Nothing
          List.lookup "cookie" qry `shouldBe` Nothing
          List.lookup "label" qry `shouldBe` Just "forbidden"
    context "granted" $ do
      it "responds with status 303 with appropriate details." $ do
        env <- ask
        (owner, _teamId) <- call $ createUserWithTeam (env ^. teBrig) (env ^. teGalley)
        idp <- registerTestIdP owner
        (Just uid, ResponseVerdict outcome, loc, qry) <- requestAccessVerdict idp True mkAuthnReqMobile
        liftIO $ do
          Servant.errHTTPCode outcome `shouldBe` 303
          Servant.errReasonPhrase outcome `shouldBe` "success"
          Servant.errBody outcome `shouldBe` mempty
          uriScheme loc `shouldBe` URI.Scheme "wire"
          List.lookup "label" qry `shouldBe` Nothing
          List.lookup "userid" qry `shouldBe` (Just . cs . show $ uid)
          List.lookup "cookie" qry `shouldNotBe` Nothing
          List.lookup "cookie" qry `shouldNotBe` Just "$cookie"
          -- cookie variable should be substituted with value.  see
          -- 'mkVerdictGrantedFormatMobile', 'mkVerdictDeniedFormatMobile'
          let Just (ckies :: ByteString) = List.lookup "cookie" qry
              cky :: SetCookie = parseSetCookie ckies
          setCookieName cky `shouldBe` "zuid"
          ('s', setCookieSecure cky) `shouldBe` ('s', False) -- we're in integration test mode, no https here!
          ('h', setCookieHttpOnly cky) `shouldBe` ('h', True)

mkAuthnReqWeb :: SAML.IdPId -> TestSpar ResponseLBS
mkAuthnReqWeb idpid = do
  env <- ask
  call $ get ((env ^. teSpar) . path (cs $ "/sso/initiate-login/" -/ SAML.idPIdToST idpid) . expect2xx)

mkAuthnReqMobile :: SAML.IdPId -> TestSpar ResponseLBS
mkAuthnReqMobile idpid = do
  env <- ask
  let succurl = [uri|wire://login-granted/?cookie=$cookie&userid=$userid|]
      errurl = [uri|wire://login-denied/?label=$label|]
      mk = Builder.toLazyByteString . urlEncode [] . serializeURIRef'
      arQueries = cs $ "success_redirect=" <> mk succurl <> "&error_redirect=" <> mk errurl
      arPath = cs $ "/sso/initiate-login/" -/ SAML.idPIdToST idpid <> "?" <> arQueries
  call $ get ((env ^. teSpar) . path arPath . expect2xx)

-- | Take an idp, "granted" flag, and 'AuthnRequest' constructor.  Construct a fresh random
-- 'UserRef' on that idp; construct the 'AuthnRequest'; construct a mock 'AuthnResponse' from the
-- idp; call 'Spar.verdictHandler' on that response and return the outcome.  Since the 'UserRef' is
-- fresh, iff the verdict is "granted" the user will be created during the call to
-- 'Spar.verdictHandler'.
requestAccessVerdict ::
  (HasCallStack) =>
  IdP ->
  -- | is the verdict granted?
  Bool ->
  -- | raw authnreq
  (SAML.IdPId -> TestSpar ResponseLBS) ->
  TestSpar
    ( Maybe UserId,
      SAML.ResponseVerdict,
      URI, -- location header
      [(ByteString, ByteString)] -- query params
    )
requestAccessVerdict idp isGranted mkAuthnReq = do
  subject <- nextSubject
  let uref = SAML.UserRef tenant subject
      tenant = idp ^. SAML.idpMetadata . SAML.edIssuer
  authnreq :: SAML.FormRedirect SAML.AuthnRequest <- do
    raw <- mkAuthnReq (idp ^. SAML.idpId)
    bdy <- maybe (error "authreq") pure $ responseBody raw
    either (error . show) pure $ Servant.mimeUnrender (Servant.Proxy @SAML.HTML) bdy
  spmeta <- getTestSPMetadata (idp ^. idpExtraInfo . User.team)
  (privKey, _, _) <- DSig.mkSignCredsWithCert Nothing 96
  authnresp :: SAML.AuthnResponse <- do
    case authnreq of
      (SAML.FormRedirect _ req) -> do
        SAML.SignedAuthnResponse (XML.Document _ el _) <-
          runSimpleSP $
            SAML.mkAuthnResponseWithSubj subject privKey idp spmeta req True
        either (error . show) pure $ SAML.parse [XML.NodeElement el]
  let verdict =
        if isGranted
          then SAML.AccessGranted uref
          else SAML.AccessDenied [DeniedNoBearerConfSubj, DeniedNoAuthnStatement]
  outcome <- do
    runSpar $ Spar.verdictHandler (authnresp ^. rspPayload) verdict idp
  let loc :: URI.URI
      loc =
        maybe (error "no location") (either error id . SAML.parseURI' . cs)
          . List.lookup "Location"
          . Servant.errHeaders
          $ unResponseVerdict outcome
      qry :: [(ByteString, ByteString)]
      qry = queryPairs $ uriQuery loc
  muid <- runSpar $ SAMLUserStore.get uref
  pure (muid, outcome, loc, qry)
