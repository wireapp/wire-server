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
import SAML2.WebSSO as SAML
import Spar.Intra.BrigApp
import Test.Hspec
import Test.QuickCheck
import URI.ByteString (URI, laxURIParserOptions, parseURI)
import Wire.API.User.Identity (UserSSOId (UserSSOId))
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
                    mkNameID (mkUNameIDTransient "V") Nothing Nothing (Just "jaan")
                )
          ssoId = UserSSOId (SAML.UserRef iss nam)
          iss :: SAML.Issuer = fromRight undefined $ SAML.decodeElem "<Issuer xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">http://wire.com/</Issuer>"
          nam :: SAML.NameID = fromRight undefined $ SAML.decodeElem "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:2.0:nameid-format:transient\" SPProvidedID=\"jaan\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">V</NameID>"
      veidToUserSSOId veid `shouldBe` ssoId
      veidFromUserSSOId ssoId Nothing `shouldBe` Right veid

    it "another example" $ do
      let veid =
            ValidScimId "PWKS" . That $
              UserRef
                (Issuer $ mkuri "http://wire.com/")
                ( either (error . show) id $
                    -- TODO(fisx): was the lower case 'k' in 'PWkS' here on purpose?
                    mkNameID (mkUNameIDPersistent "PWKS") Nothing Nothing (Just "marye")
                )
          ssoId = UserSSOId (SAML.UserRef iss nam)
          iss :: SAML.Issuer = fromRight undefined $ SAML.decodeElem "<Issuer xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">http://wire.com/</Issuer>"
          nam :: SAML.NameID = fromRight undefined $ SAML.decodeElem "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:2.0:nameid-format:persistent\" SPProvidedID=\"marye\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">PWKS</NameID>"
      veidToUserSSOId veid `shouldBe` ssoId
      veidFromUserSSOId ssoId Nothing `shouldBe` Right veid

    it "roundtrips" . property $
      \(x :: ValidScimId) ->
        veidFromUserSSOId @(Either String) (veidToUserSSOId x) (justHere x.validScimIdAuthInfo) === Right x
