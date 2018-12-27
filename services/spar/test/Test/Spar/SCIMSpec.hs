{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Test.Spar.SCIMSpec where

import Imports
import Data.Id
import Network.URI (parseURI)
import Spar.SCIM
import Test.Hspec
import URI.ByteString

import qualified Data.UUID as UUID
import qualified SAML2.WebSSO as SAML
import qualified Web.SCIM.Class.User as SCIMC
import qualified Web.SCIM.Schema.Common as SCIM
import qualified Web.SCIM.Schema.Schema as SCIM
import qualified Web.SCIM.Schema.Meta as SCIM
import qualified Web.SCIM.Schema.ResourceType as SCIMR
import qualified Web.SCIM.Schema.User as SCIM
import qualified Web.SCIM.Schema.User.Name as SCIMN


spec :: Spec
spec = describe "toSCIMStoredUser'" $ do
  it "works" $ do
    let usr :: SCIM.User
        usr = SCIM.User
          { SCIM.schemas = [SCIM.User20]
          , SCIM.userName = "02b35298-088f-11e9-b4a4-478635dd0d2b"
          , SCIM.externalId = Just "c1704a48-0a1e-11e9-9186-9b185fe892e8"
          , SCIM.name = Just (SCIMN.Name { SCIMN.formatted = Nothing
                                         , SCIMN.familyName = Just ""
                                         , SCIMN.givenName = Just ""
                                         , SCIMN.middleName = Nothing
                                         , SCIMN.honorificPrefix = Nothing
                                         , SCIMN.honorificSuffix = Nothing
                                         })
          , SCIM.displayName = Just "67d0268e-088e-11e9-a400-b71b4d4d2275"
          , SCIM.nickName = Nothing
          , SCIM.profileUrl = Nothing
          , SCIM.title = Nothing
          , SCIM.userType = Nothing
          , SCIM.preferredLanguage = Nothing
          , SCIM.locale = Nothing
          , SCIM.active = Nothing
          , SCIM.password = Nothing
          , SCIM.emails = Nothing
          , SCIM.phoneNumbers = Nothing
          , SCIM.ims = Nothing
          , SCIM.photos = Nothing
          , SCIM.addresses = Nothing
          , SCIM.entitlements = Nothing
          , SCIM.roles = Nothing
          , SCIM.x509Certificates = Nothing
          }

        meta :: SCIM.Meta
        meta = SCIM.Meta
          { SCIM.resourceType = SCIMR.UserResource
          , SCIM.created = now
          , SCIM.lastModified = now
          , SCIM.version = SCIM.Strong "e5442c575adce7a7affbb2f744ab0825c553c9e6ce5dafdee8789364d824614e"
          , SCIM.location = SCIM.URI . fromJust $ Network.URI.parseURI
                            "https://127.0.0.1/scim/Users/90b5ee1c-088e-11e9-9a16-73f80f483813"
          }

        now'@(SAML.Time now) = SAML.unsafeReadTime "1918-04-14T09:58:58.457Z"
        baseuri :: URI = either (error . show) id $
          URI.ByteString.parseURI laxURIParserOptions "https://127.0.0.1/scim/"
        uid = Id . fromJust . UUID.fromText $ "90b5ee1c-088e-11e9-9a16-73f80f483813"

        result :: SCIMC.StoredUser
        result = toSCIMStoredUser' now' baseuri uid usr

    SCIM.meta result `shouldBe` meta
    SCIM.value (SCIM.thing result) `shouldBe` usr
