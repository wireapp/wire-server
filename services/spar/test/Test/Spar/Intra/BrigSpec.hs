{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
import Brig.Types.User (UserSSOId (UserSSOId))
import Control.Lens ((^.))
import Data.String.Conversions (ST, cs)
import Imports
import SAML2.WebSSO as SAML
import Spar.Intra.Brig
import Spar.Scim.Types
import Test.Hspec
import Test.QuickCheck
import URI.ByteString (URI, laxURIParserOptions, parseURI)

mkuri :: ST -> URI
mkuri = either (error . show) id . parseURI laxURIParserOptions . cs

spec :: Spec
spec = do
  describe "veidToUserSSOId, veidFromUserSSOId" $ do
    -- example unit tests are mostly for documentation.  if they fail, it may be because of some
    -- harmless change in the string representation of the xml data, and you can probably just
    -- remove them.

    it "example" $ do
      let have =
            UrefOnly $
              UserRef
                (Issuer $ mkuri "http://wire.com/")
                ( either (error . show) id $
                    mkNameID (mkUNameIDTransient "V") (Just "kati") (Just "rolli") (Just "jaan")
                )
          want =
            UserSSOId
              "<Issuer xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">http://wire.com/</Issuer>"
              "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:2.0:nameid-format:transient\" NameQualifier=\"kati\" SPNameQualifier=\"rolli\" SPProvidedID=\"jaan\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">V</NameID>"
      veidToUserSSOId have `shouldBe` want
      veidFromUserSSOId want `shouldBe` Right have
    it "another example" $ do
      let have =
            UrefOnly $
              UserRef
                (Issuer $ mkuri "http://wire.com/")
                ( either (error . show) id $
                    mkNameID (mkUNameIDPersistent "PWkS") (Just "hendrik") Nothing (Just "marye")
                )
          want =
            UserSSOId
              "<Issuer xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">http://wire.com/</Issuer>"
              "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:2.0:nameid-format:persistent\" NameQualifier=\"hendrik\" SPProvidedID=\"marye\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">PWkS</NameID>"
      veidToUserSSOId have `shouldBe` want
      veidFromUserSSOId want `shouldBe` Right have

    it "roundtrips" . property $
      \(x :: ValidExternalId) -> (veidFromUserSSOId @(Either String) . veidToUserSSOId) x === Right x

instance Arbitrary ValidExternalId where
  arbitrary = do
    muref <- arbitrary
    case muref of
      Just uref -> case emailFromSAMLNameID $ uref ^. SAML.uidSubject of
        Just email -> pure $ EmailAndUref email uref
        Nothing -> pure $ UrefOnly uref
      Nothing -> EmailOnly <$> arbitrary
