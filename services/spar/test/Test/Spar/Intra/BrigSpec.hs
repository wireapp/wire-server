{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Test.Spar.Intra.BrigSpec where

import Arbitrary ()
import Brig.Types.User (UserSSOId(UserSSOId))
import Data.String.Conversions (ST, cs)
import SAML2.WebSSO as SAML
import Spar.Intra.Brig
import Test.Hspec
import Test.QuickCheck
import URI.ByteString (URI, parseURI, laxURIParserOptions)


mkuri :: ST -> URI
mkuri = either (error . show) id . parseURI laxURIParserOptions . cs

spec :: Spec
spec = do
  describe "toUserSSOId, fromUserSSOId" $ do
    -- example unit tests are mostly for documentation.  if they fail, it may be because of some
    -- harmless change in the string representation of the xml data, and you can probably just
    -- remove them.

    it "example" $ do
      let have = UserId
            (Issuer $ mkuri "http://wire.com/")
            (NameID (NameIDFTransient "V") (Just "kati") (Just "rolli") (Just "jaan"))
          want = UserSSOId
            "<Issuer xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">http://wire.com/</Issuer>"
            "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:2.0:nameid-format:transient\" NameQualifier=\"kati\" SPNameQualifier=\"rolli\" SPProvidedID=\"jaan\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">V</NameID>"
      toUserSSOId have `shouldBe` want
      fromUserSSOId want `shouldBe` Right have

    it "another example" $ do
      let have = UserId
            (Issuer $ mkuri "http://wire.com/")
            (NameID (NameIDFPersistent "PWkS") (Just "hendrik") Nothing (Just "marye"))
          want = UserSSOId
            "<Issuer xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">http://wire.com/</Issuer>"
            "<NameID xmlns:samlp=\"urn:oasis:names:tc:SAML:2.0:protocol\" xmlns:samla=\"urn:oasis:names:tc:SAML:2.0:assertion\" xmlns:samlm=\"urn:oasis:names:tc:SAML:2.0:metadata\" xmlns:ds=\"http://www.w3.org/2000/09/xmldsig#\" Format=\"urn:oasis:names:tc:SAML:2.0:nameid-format:persistent\" NameQualifier=\"hendrik\" SPProvidedID=\"marye\" xmlns=\"urn:oasis:names:tc:SAML:2.0:assertion\">PWkS</NameID>"
      toUserSSOId have `shouldBe` want
      fromUserSSOId want `shouldBe` Right have

    it "roundtrips" . property $
      \(x :: SAML.UserId) -> (fromUserSSOId @(Either String) . toUserSSOId) x == Right x
