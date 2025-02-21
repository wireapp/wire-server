{-# OPTIONS_GHC -Wno-orphans #-}

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

module Test.Spar.Intra.BrigSpec where

import Arbitrary ()
import Data.String.Conversions
import Data.These
import Data.These.Combinators
import Imports
import Polysemy (EffectRow, Sem, interpret, run)
import SAML2.WebSSO as SAML
import Spar.Intra.BrigApp
import Spar.Sem.BrigAccess (BrigAccess (GetDomainRegistration))
import Test.Hspec
import Test.QuickCheck
import URI.ByteString (URI, laxURIParserOptions, parseURI)
import Wire.API.EnterpriseLogin
import Wire.API.Routes.Public.Brig.DomainVerification
import Wire.API.User.Identity (EmailAddress, UserSSOId (UserSSOId))
import Wire.API.User.Scim

mkuri :: Text -> URI
mkuri = either (error . show) id . parseURI laxURIParserOptions . cs

spec :: Spec
spec = do
  describe "veidToUserSSOId, veidFromUserSSOId" $ do
    -- example unit tests are mostly for documentation.  if they fail, it may be because of some
    -- harmless change in the string representation of the xml data, and you can probably just
    -- remove them.

    it "example" $ do
      let veid =
            ValidScimId "V" . That $
              UserRef
                (Issuer $ mkuri "http://wire.com/")
                ( either (error . show) id $
                    mkNameID (mkUNameIDTransient "V") (Just "kati") (Just "rolli") (Just "jaan")
                )
          ssoId = UserSSOId (SAML.UserRef iss nam)
          iss :: SAML.Issuer = fromRight undefined $ SAML.decodeElem "<Issuer xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">http://wire.com/</Issuer>"
          nam :: SAML.NameID = fromRight undefined $ SAML.decodeElem "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:2.0:nameid-format:transient\" NameQualifier=\"kati\" SPNameQualifier=\"rolli\" SPProvidedID=\"jaan\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">V</NameID>"
      veidToUserSSOId veid `shouldBe` ssoId
      veidFromUserSSOId ssoId Nothing `shouldBe` Right veid

    it "another example" $ do
      let veid =
            ValidScimId "PWkS" . That $
              UserRef
                (Issuer $ mkuri "http://wire.com/")
                ( either (error . show) id $
                    mkNameID (mkUNameIDPersistent "PWkS") (Just "hendrik") Nothing (Just "marye")
                )
          ssoId = UserSSOId (SAML.UserRef iss nam)
          iss :: SAML.Issuer = fromRight undefined $ SAML.decodeElem "<Issuer xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">http://wire.com/</Issuer>"
          nam :: SAML.NameID = fromRight undefined $ SAML.decodeElem "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:2.0:nameid-format:persistent\" NameQualifier=\"hendrik\" SPProvidedID=\"marye\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">PWkS</NameID>"
      veidToUserSSOId veid `shouldBe` ssoId
      veidFromUserSSOId ssoId Nothing `shouldBe` Right veid

    it "roundtrips" . property $
      \(ValidScimIdNoNameIDQualifiers x) ->
        veidFromUserSSOId @(Either String) (veidToUserSSOId x) (justHere x.validScimIdAuthInfo) === Right x

  describe "emailDomainIsRegisteredForSSO" $ do
    it "should return true for domain redirect sso" . property $ \email response ->
      let actual =
            run . (mockBrig email response) $
              emailDomainIsRegisteredForSSO email
          expected = case response.redirect of
            None -> False
            Locked -> False
            SSO _ -> True
            Backend _ -> False
            NoRegistration -> False
            PreAuthorized -> False
       in actual === expected

mockBrig ::
  forall (r :: EffectRow) a.
  EmailAddress ->
  DomainRedirectResponse ->
  Sem (BrigAccess ': r) a ->
  Sem r a
mockBrig email response = interpret $ \case
  (GetDomainRegistration email') | email' == email -> pure $ response
  _ -> error "Throw error here to avoid implementation of all cases."
