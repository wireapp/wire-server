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
import Brig.Types.User
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
spec = describe "toSCIMUser'" $ do
  it "works (1)" $ do
    let usr :: User
        usr = User
          { userId = Id . fromJust . UUID.fromText $ "90b5ee1c-088e-11e9-9a16-73f80f483813"
          , userIdentity = Nothing :: Maybe UserIdentity
          , userName = Name "67d0268e-088e-11e9-a400-b71b4d4d2275"
          , userPict = Pict []
          , userAssets = []
          , userAccentId = ColourId 3
          , userDeleted = False
          , userLocale = fromJust $ parseLocale "en_US.UTF-8"
          , userService = Nothing
          , userHandle = Just $ Handle "02b35298-088f-11e9-b4a4-478635dd0d2b"
          , userExpire = Nothing
          , userTeam = Just $ Id . fromJust . UUID.fromText $ "95e29976-088e-11e9-8018-f31b54078a43"
          }

        scmusr :: SCIMC.StoredUser
        scmusr = SCIM.WithMeta
          { SCIM.meta = SCIM.Meta
            { SCIM.resourceType = SCIMR.UserResource
            , SCIM.created = now
            , SCIM.lastModified = now
            , SCIM.version = SCIM.Strong "2a05a40c01b07d4996d424c5fd60667feb3d17fe7f9bd531129e308a71e57117"
            , SCIM.location = SCIM.URI . fromJust $ Network.URI.parseURI
                              "https://127.0.0.1/scim/Users/90b5ee1c-088e-11e9-9a16-73f80f483813"
            }
          , SCIM.thing = SCIM.WithId
            { SCIM.id = "90b5ee1c-088e-11e9-9a16-73f80f483813"
            , SCIM.value = SCIM.User
              { SCIM.schemas = [SCIM.User20]
              , SCIM.userName = "02b35298-088f-11e9-b4a4-478635dd0d2b"
              , SCIM.externalId = Nothing
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
            }
          }

        SAML.Time now = SAML.unsafeReadTime "1918-04-14T09:58:58.457Z"

        baseuri :: URI
        baseuri = either (error . show) id $
          URI.ByteString.parseURI laxURIParserOptions "https://127.0.0.1/scim/"

    toSCIMUser' now baseuri usr `shouldBe` scmusr

  it "works (2)" $ do
    let usr :: User
        usr = User
          { userId = Id . fromJust . UUID.fromText $ "f11c261e-0892-11e9-8381-ab12930a22e8"
          , userIdentity = Nothing :: Maybe UserIdentity
          , userName = Name "f3def660-0892-11e9-9070-7779e1a45ed2"
          , userPict = Pict []
          , userAssets = []
          , userAccentId = ColourId 3
          , userDeleted = False
          , userLocale = fromJust $ parseLocale "en_US.UTF-8"
          , userService = Nothing
          , userHandle = Nothing
          , userExpire = Nothing
          , userTeam = Nothing
          }

        scmusr :: SCIMC.StoredUser
        scmusr = SCIM.WithMeta
          { SCIM.meta = SCIM.Meta
            { SCIM.resourceType = SCIMR.UserResource
            , SCIM.created = now
            , SCIM.lastModified = now
            , SCIM.version = SCIM.Strong "faf2c5b379beddf4b64fc234c11925ff63aa3344af4dc2f10501aca39f13e766"
            , SCIM.location = SCIM.URI . fromJust $ Network.URI.parseURI
                              "https://127.0.0.1/scim/Users/f11c261e-0892-11e9-8381-ab12930a22e8"
            }
          , SCIM.thing = SCIM.WithId
            { SCIM.id = "f11c261e-0892-11e9-8381-ab12930a22e8"
            , SCIM.value = SCIM.User
              { SCIM.schemas = [SCIM.User20]
              , SCIM.userName = "f11c261e-0892-11e9-8381-ab12930a22e8"
              , SCIM.externalId = Nothing
              , SCIM.name = Just (SCIMN.Name { SCIMN.formatted = Nothing
                                       , SCIMN.familyName = Just ""
                                       , SCIMN.givenName = Just ""
                                       , SCIMN.middleName = Nothing
                                       , SCIMN.honorificPrefix = Nothing
                                       , SCIMN.honorificSuffix = Nothing
                                       })
              , SCIM.displayName = Just "f3def660-0892-11e9-9070-7779e1a45ed2"
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
            }
          }

        SAML.Time now = SAML.unsafeReadTime "1918-04-14T09:58:58.457Z"

        baseuri :: URI
        baseuri = either (error . show) id $
          URI.ByteString.parseURI laxURIParserOptions "https://127.0.0.1/scim/"

    toSCIMUser' now baseuri usr `shouldBe` scmusr
