{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Test.Spar.ScimSpec where

import Imports
import Data.Id
import Network.URI (parseURI)
import Spar.Scim
import Test.Hspec
import URI.ByteString

import qualified Data.UUID as UUID
import qualified SAML2.WebSSO as SAML
import qualified Web.Scim.Class.User as ScimC
import qualified Web.Scim.Schema.Common as Scim
import qualified Web.Scim.Schema.Meta as Scim
import qualified Web.Scim.Schema.ResourceType as ScimR
import qualified Web.Scim.Schema.User as Scim
import qualified Web.Scim.Schema.User.Name as ScimN


spec :: Spec
spec = describe "toScimStoredUser'" $ do
  it "works" $ do
    let usr :: Scim.User
        usr = Scim.User
          { Scim.userName = "02b35298-088f-11e9-b4a4-478635dd0d2b"
          , Scim.externalId = Just "c1704a48-0a1e-11e9-9186-9b185fe892e8"
          , Scim.name = Just (ScimN.Name { ScimN.formatted = Nothing
                                         , ScimN.familyName = Just ""
                                         , ScimN.givenName = Just ""
                                         , ScimN.middleName = Nothing
                                         , ScimN.honorificPrefix = Nothing
                                         , ScimN.honorificSuffix = Nothing
                                         })
          , Scim.displayName = Just "67d0268e-088e-11e9-a400-b71b4d4d2275"
          , Scim.nickName = Nothing
          , Scim.profileUrl = Nothing
          , Scim.title = Nothing
          , Scim.userType = Nothing
          , Scim.preferredLanguage = Nothing
          , Scim.locale = Nothing
          , Scim.active = Nothing
          , Scim.password = Nothing
          , Scim.emails = []
          , Scim.phoneNumbers = []
          , Scim.ims = []
          , Scim.photos = []
          , Scim.addresses = []
          , Scim.entitlements = []
          , Scim.roles = []
          , Scim.x509Certificates = []
          }

        meta :: Scim.Meta
        meta = Scim.Meta
          { Scim.resourceType = ScimR.UserResource
          , Scim.created = now
          , Scim.lastModified = now
          , Scim.version = Scim.Weak "e5442c575adce7a7affbb2f744ab0825c553c9e6ce5dafdee8789364d824614e"
          , Scim.location = Scim.URI . fromJust $ Network.URI.parseURI
                            "https://127.0.0.1/scim/v2/Users/90b5ee1c-088e-11e9-9a16-73f80f483813"
          }

        now'@(SAML.Time now) = SAML.unsafeReadTime "1918-04-14T09:58:58.457Z"
        baseuri :: URI = either (error . show) id $
          URI.ByteString.parseURI laxURIParserOptions "https://127.0.0.1/scim/v2/"
        uid = Id . fromJust . UUID.fromText $ "90b5ee1c-088e-11e9-9a16-73f80f483813"

        result :: ScimC.StoredUser
        result = toScimStoredUser' now' baseuri uid usr

    Scim.meta result `shouldBe` meta
    Scim.value (Scim.thing result) `shouldBe` usr
