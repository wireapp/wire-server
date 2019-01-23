{-# LANGUAGE QuasiQuotes #-}

module Test.Schema.UserSpec (spec) where

import           Test.Util

import           Web.Scim.Schema.Common (URI(..))
import           Web.Scim.Schema.User
import           Web.Scim.Schema.User.Address as Address
import           Web.Scim.Schema.User.Certificate as Certificate
import           Web.Scim.Schema.User.Email as Email
import           Web.Scim.Schema.User.IM as IM
import           Web.Scim.Schema.User.Name as Name
import           Web.Scim.Schema.User.Phone as Phone
import           Web.Scim.Schema.User.Photo as Photo
import           Data.Aeson
import           Test.Hspec
import           Text.Email.Validate (emailAddress)
import           Network.URI.Static (uri)

spec :: Spec
spec = do
  describe "JSON serialization" $ do
    it "handles all fields" $ do
      toJSON completeUser `shouldBe` completeUserJson
      eitherDecode (encode completeUserJson) `shouldBe` Right completeUser

    it "has defaults for all optional and multi-valued fields" $ do
      toJSON minimalUser `shouldBe` minimalUserJson
      eitherDecode (encode minimalUserJson) `shouldBe` Right minimalUser

    it "treats 'null' and '[]' as absence of fields" $
      eitherDecode (encode minimalUserJsonRedundant) `shouldBe` Right minimalUser

    it "allows casing variations in field names" $
      eitherDecode (encode minimalUserJsonNonCanonical) `shouldBe` Right minimalUser

-- | A 'User' with all attributes present.
completeUser :: User
completeUser = User
  { userName = "sample userName"
  , externalId = Just "sample externalId"
  , name = Just $ Name
      { Name.formatted = Just "sample formatted name"
      , Name.familyName = Nothing
      , Name.givenName = Nothing
      , Name.middleName = Nothing
      , Name.honorificPrefix = Nothing
      , Name.honorificSuffix = Nothing
      }
  , displayName = Just "sample displayName"
  , nickName = Just "sample nickName"
  , profileUrl = Just (URI [uri|https://example.com|])
  , title = Just "sample title"
  , userType = Just "sample userType"
  , preferredLanguage = Just "da, en-gb;q=0.8, en;q=0.7"
  , locale = Just "en-US"
  , active = Just True
  , password = Just "sample password"
  , emails = [
      Email { Email.typ = Just "work"
            , Email.value = maybe (error "couldn't parse email") EmailAddress2
                            (emailAddress "user@example.com")
            , Email.primary = Nothing }
      ]
  , phoneNumbers = [
      Phone { Phone.typ = Just "work"
            , Phone.value = Just "+15417543010" }
      ]
  , ims = [
      IM { IM.typ = Just "Wire"
         , IM.value = Just "@user" }
      ]
  , photos = [
      Photo { Photo.typ = Just "userpic"
            , Photo.value = Just (URI [uri|https://example.com/userpic.png|]) }
      ]
  , addresses = [
      Address { Address.formatted = Just "sample Address"
              , Address.streetAddress = Nothing
              , Address.locality = Nothing
              , Address.region = Nothing
              , Address.postalCode = Nothing
              , Address.country = Nothing
              , Address.typ = Just "home"
              , Address.primary = Just True }
      ]
  , entitlements = ["sample entitlement"]
  , roles = ["sample role"]
  , x509Certificates = [
      Certificate { Certificate.typ = Just "sample certificate type"
                  , Certificate.value = Just "sample certificate" }
      ]
  }

-- | Reference encoding of 'completeUser'.
completeUserJson :: Value
completeUserJson = [scim|
{
  "roles": [
    "sample role"
  ],
  "x509Certificates": [
    {
      "value": "sample certificate",
      "type": "sample certificate type"
    }
  ],
  "schemas": [
    "urn:ietf:params:scim:schemas:core:2.0:User"
  ],
  "locale": "en-US",
  "addresses": [
    {
      "formatted": "sample Address",
      "primary": true,
      "type": "home"
    }
  ],
  "userName": "sample userName",
  "phoneNumbers": [
    {
      "value": "+15417543010",
      "type": "work"
    }
  ],
  "active": true,
  "photos": [
    {
      "value": "https://example.com/userpic.png",
      "type": "userpic"
    }
  ],
  "name": {
    "formatted": "sample formatted name"
  },
  "password": "sample password",
  "emails": [
    {
      "value": "user@example.com",
      "type": "work"
    }
  ],
  "ims": [
    {
      "value": "@user",
      "type": "Wire"
    }
  ],
  "preferredLanguage": "da, en-gb;q=0.8, en;q=0.7",
  "entitlements": [
    "sample entitlement"
  ],
  "displayName": "sample displayName",
  "nickName": "sample nickName",
  "profileUrl": "https://example.com",
  "title": "sample title",
  "externalId": "sample externalId",
  "userType": "sample userType"
}
|]

-- | A 'User' with all attributes empty (if possible).
minimalUser :: User
minimalUser = empty { userName = "sample userName" }

-- | Reference encoding of 'minimalUser'.
minimalUserJson :: Value
minimalUserJson = [scim|
{
  "schemas": [
    "urn:ietf:params:scim:schemas:core:2.0:User"
  ],
  "userName": "sample userName"
}
|]

-- | An encoding of 'minimalUser' with redundant @null@s and @[]@s for missing
-- fields.
minimalUserJsonRedundant :: Value
minimalUserJsonRedundant = [scim|
{
  "roles": [],
  "x509Certificates": [],
  "locale": null,
  "addresses": [],
  "userName": "sample userName",
  "phoneNumbers": [],
  "active": null,
  "photos": [],
  "name": null,
  "password": null,
  "emails": [],
  "ims": [],
  "preferredLanguage": null,
  "entitlements": [],
  "displayName": null,
  "nickName": null,
  "profileUrl": null,
  "title": null,
  "externalId": null,
  "userType": null
}
|]

-- | An encoding of 'minimalUser' with non-canonical field name casing.
minimalUserJsonNonCanonical :: Value
minimalUserJsonNonCanonical = [scim|
{
  "USERname": "sample userName"
}
|]
