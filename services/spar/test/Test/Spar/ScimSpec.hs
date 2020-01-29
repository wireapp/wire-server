{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-unused-imports #-}

module Test.Spar.ScimSpec where

import Imports
import Brig.Types.Test.Arbitrary
import Brig.Types.User (RichInfo(..), RichField(..))
import Data.Aeson (encode, eitherDecode', parseJSON)
import Data.Id
import Network.URI (parseURI)
import Spar.Scim
import Test.Hspec
import Test.QuickCheck
import URI.ByteString
import Web.Scim.Schema.PatchOp (Operation(..), Path(NormalPath), Op(Remove), PatchOp(..), applyOperation)
import Web.Scim.Schema.Schema (Schema(CustomSchema))
import Web.Scim.AttrName (AttrName(..))
import Web.Scim.Filter (AttrPath(..))
import Data.Aeson.QQ (aesonQQ)

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
import qualified Web.Scim.Schema.PatchOp as PatchOp
import qualified Data.Aeson.Types as Aeson

spec :: Spec
spec = describe "toScimStoredUser'" $ do
  it "works" $ do
    let usr :: Scim.User SparTag
        usr = Scim.User
          { Scim.schemas = [ Scim.User20
                           , Scim.CustomSchema "urn:wire:scim:schemas:profile:1.0"
                           , Scim.CustomSchema "urn:ietf:params:scim:schemas:extension:wire:1.0:User"
                           ]
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
          , Scim.version = Scim.Weak "46246ab15ccab8a70b59f97f7182d6fb557dd454c0f06cdcb83d99d027cff08e"
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
        eitherDecode' (encode sue) `shouldBe` Right sue

  describe "ScimUserExtra" $ do
    describe "Patchable" $ do
      it "can add to rich info map" $ do
        let operationJSON = [aesonQQ|{
                                       "schemas" : [ "urn:ietf:params:scim:api:messages:2.0:PatchOp" ],
                                       "operations" : [{
                                         "op" : "add",
                                         "path" : "urn:ietf:params:scim:schemas:extension:wire:1.0:User:newAttr",
                                         "value" : "newValue"
                                       }]
                                     }|]
        let (Aeson.Success (PatchOp [operation])) = Aeson.parse (parseJSON @(PatchOp SparTag)) operationJSON
        applyOperation (ScimUserExtra (RichInfo mempty mempty)) operation
          `shouldBe` (Right (ScimUserExtra (RichInfo (Map.singleton "newAttr" "newValue") mempty)))

      it "can replace in rich info map" $ do
        let operationJSON = [aesonQQ|{
                                       "schemas" : [ "urn:ietf:params:scim:api:messages:2.0:PatchOp" ],
                                       "operations" : [{
                                         "op" : "replace",
                                         "path" : "urn:ietf:params:scim:schemas:extension:wire:1.0:User:oldAttr",
                                         "value" : "newValue"
                                       }]
                                     }|]
        let (Aeson.Success (PatchOp [operation])) = Aeson.parse (parseJSON @(PatchOp SparTag)) operationJSON
        applyOperation (ScimUserExtra (RichInfo (Map.singleton "oldAttr" "oldValue") mempty)) operation
          `shouldBe` (Right (ScimUserExtra (RichInfo (Map.singleton "oldAttr" "newValue") mempty)))

      it "can remove from rich info map" $ do
        let operationJSON = [aesonQQ|{
                                       "schemas" : [ "urn:ietf:params:scim:api:messages:2.0:PatchOp" ],
                                       "operations" : [{
                                         "op" : "remove",
                                         "path" : "urn:ietf:params:scim:schemas:extension:wire:1.0:User:oldAttr"
                                       }]
                                     }|]
        let (Aeson.Success (PatchOp [operation])) = Aeson.parse (parseJSON @(PatchOp SparTag)) operationJSON
        applyOperation (ScimUserExtra (RichInfo (Map.singleton "oldAttr" "oldValue") mempty)) operation
          `shouldBe` (Right (ScimUserExtra (RichInfo mempty mempty)))

      it "adds new fields to rich info assoc list at the end" $ do
        let operationJSON = [aesonQQ|{
                                       "schemas" : [ "urn:ietf:params:scim:api:messages:2.0:PatchOp" ],
                                       "operations" : [{
                                         "op" : "add",
                                         "path" : "urn:wire:scim:schemas:profile:1.0:newAttr",
                                         "value" : "newValue"
                                       }]
                                     }|]
        let (Aeson.Success (PatchOp [operation])) = Aeson.parse (parseJSON @(PatchOp SparTag)) operationJSON
        applyOperation (ScimUserExtra (RichInfo mempty [RichField "oldAttr" "oldValue"])) operation
          `shouldBe` (Right (ScimUserExtra (RichInfo mempty [RichField "oldAttr" "oldValue", RichField "newAttr" "newValue"])))

      it "can replace in rich info assoc list while maintaining order" $ do
        let operationJSON = [aesonQQ|{
                                       "schemas" : [ "urn:ietf:params:scim:api:messages:2.0:PatchOp" ],
                                       "operations" : [{
                                         "op" : "replace",
                                         "path" : "urn:wire:scim:schemas:profile:1.0:secondAttr",
                                         "value" : "newSecondVal"
                                       }]
                                     }|]
        let (Aeson.Success (PatchOp [operation])) = Aeson.parse (parseJSON @(PatchOp SparTag)) operationJSON
        let origAssocList = [ RichField "firstAttr" "firstVal"
                            , RichField "secondAttr" "secondVal"
                            , RichField "thirdAttr" "thirdVal"
                            ]
        let expectedAssocList = [ RichField "firstAttr" "firstVal"
                                , RichField "secondAttr" "newSecondVal"
                                , RichField "thirdAttr" "thirdVal"
                                ]
        applyOperation (ScimUserExtra (RichInfo mempty origAssocList)) operation
          `shouldBe` (Right (ScimUserExtra (RichInfo mempty expectedAssocList)))

      it "can remove from rich info assoc list" $ do
        let operationJSON = [aesonQQ|{
                                       "schemas" : [ "urn:ietf:params:scim:api:messages:2.0:PatchOp" ],
                                       "operations" : [{
                                         "op" : "remove",
                                         "path" : "urn:wire:scim:schemas:profile:1.0:oldAttr"
                                       }]
                                     }|]
        let (Aeson.Success (PatchOp [operation])) = Aeson.parse (parseJSON @(PatchOp SparTag)) operationJSON
        applyOperation (ScimUserExtra (RichInfo mempty [RichField "oldAttr" "oldValue"])) operation
          `shouldBe` (Right (ScimUserExtra (RichInfo mempty mempty)))

      it "throws error if asked to patch an recognized schema" $ do
        let schema = Just (CustomSchema "wrong-schema")
            path = Just (NormalPath (AttrPath schema "oldAttr" Nothing))
            operation = Operation Remove path Nothing
        isLeft (applyOperation (ScimUserExtra (RichInfo mempty [RichField "oldAttr" "oldValue"])) operation)
          `shouldBe` True

instance Arbitrary ScimUserExtra where
  arbitrary = ScimUserExtra <$> arbitrary
