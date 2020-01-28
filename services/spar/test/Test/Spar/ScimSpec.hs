{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

module Test.Spar.ScimSpec where

import Imports
import Brig.Types.Test.Arbitrary
import Brig.Types.User (RichInfo(..))
import Control.Lens ((%~), (.~))
import Data.Aeson (encode, eitherDecode')
import Data.Id
import Network.URI (parseURI)
import Spar.Scim
import Test.Hspec
import Test.QuickCheck
import URI.ByteString

import qualified Data.HashMap.Strict              as HM
import qualified Data.Map as Map
import qualified Data.Text                        as T
import qualified Data.UUID as UUID
import qualified SAML2.WebSSO as SAML
import qualified Web.Scim.Class.User as ScimC
import qualified Web.Scim.Schema.Common as Scim
import qualified Web.Scim.Schema.Meta as Scim
import qualified Web.Scim.Schema.Schema as Scim
import qualified Web.Scim.Schema.ResourceType as ScimR
import qualified Web.Scim.Schema.User as Scim
import qualified Web.Scim.Schema.User.Name as ScimN


spec :: Spec
spec = describe "toScimStoredUser'" $ do
  it "works" $ do
    let usr :: Scim.User SparTag
        usr = Scim.User
          { Scim.schemas = [Scim.User20,
                            Scim.CustomSchema "urn:wire:scim:schemas:profile:1.0"]
          , Scim.userName = "02b35298-088f-11e9-b4a4-478635dd0d2b"
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
          , Scim.extra = ScimUserExtra (RichInfo mempty mempty)
          }

        meta :: Scim.Meta
        meta = Scim.Meta
          { Scim.resourceType = ScimR.UserResource
          , Scim.created = now
          , Scim.lastModified = now
          , Scim.version = Scim.Weak "cd79ccdd2cff3eeb01bce976f586b086547325907e0a3a7303ecaa61a04635da"
          , Scim.location = Scim.URI . fromJust $ Network.URI.parseURI
                            "https://127.0.0.1/scim/v2/Users/90b5ee1c-088e-11e9-9a16-73f80f483813"
          }

        now'@(SAML.Time now) = SAML.unsafeReadTime "1918-04-14T09:58:58.457Z"
        baseuri :: URI = either (error . show) id $
          URI.ByteString.parseURI laxURIParserOptions "https://127.0.0.1/scim/v2/"
        uid = Id . fromJust . UUID.fromText $ "90b5ee1c-088e-11e9-9a16-73f80f483813"

        result :: ScimC.StoredUser SparTag
        result = toScimStoredUser' now' baseuri uid usr

    Scim.meta result `shouldBe` meta
    Scim.value (Scim.thing result) `shouldBe` usr

  it "roundtrips" . property $ do
      \(sue :: ScimUserExtra) ->
        eitherDecode' (encode sue) `shouldBe` Right (sue & sueRichInfo %~ id)


z :: IO ()
z = hspec $
  it "roundtrips" . property $ do
      \(sue :: ScimUserExtra) ->
        eitherDecode' (encode sue) `shouldBe` Right sue


x :: ScimUserExtra
x = ScimUserExtra $ RichInfo { richInfoMap = Map.fromList [ ("T", ";m")
                                                          , ("T\180192", "\ESCu\843858")
                                                          , ("", "5\a")
                                                          ]
                             , richInfoAssocList = []
                             }

x' :: Either String ScimUserExtra
x' = eitherDecode' (encode x)




instance Arbitrary ScimUserExtra where
  arbitrary = ScimUserExtra <$> arbitrary

instance Arbitrary UserCustomSchema where
  arbitrary = elements [minBound..]



-- TODO: what happens if an key occurs twice in the richinfo list?
