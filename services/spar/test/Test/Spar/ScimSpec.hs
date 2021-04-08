{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Spar.ScimSpec where

import Arbitrary ()
import Brig.Types.Common (Email (..), fromEmail)
import Brig.Types.Test.Arbitrary
import Data.Aeson (eitherDecode', encode, parseJSON)
import Data.Aeson.QQ (aesonQQ)
import qualified Data.Aeson.Types as Aeson
import Data.Id
import Data.Json.Util (fromUTCTimeMillis, toUTCTimeMillis)
import Data.String.Conversions (cs)
import qualified Data.UUID as UUID
import Imports
import Network.URI (parseURI)
import qualified SAML2.WebSSO as SAML
import SAML2.WebSSO.Types (_idpExtraInfo)
import Spar.Scim
import Spar.Types (IdP, _wiTeam)
import Test.Hspec
import Test.QuickCheck
import qualified Text.Email.Parser as Email
import URI.ByteString
import qualified Web.Scim.Class.User as ScimC
import Web.Scim.Filter (AttrPath (..))
import qualified Web.Scim.Schema.Common as Scim
import qualified Web.Scim.Schema.Error as Scim
import qualified Web.Scim.Schema.Meta as Scim
import Web.Scim.Schema.PatchOp (Op (Remove), Operation (..), PatchOp (..), Path (NormalPath), applyOperation)
import qualified Web.Scim.Schema.ResourceType as ScimR
import Web.Scim.Schema.Schema (Schema (CustomSchema))
import qualified Web.Scim.Schema.Schema as Scim
import qualified Web.Scim.Schema.User as Scim
import qualified Web.Scim.Schema.User.Email as Email
import qualified Web.Scim.Schema.User.Name as ScimN
import Wire.API.User.Identity.AuthId (AuthId (..), authIdScimEmailWithSource, ewsEmail)
import Wire.API.User.RichInfo

unsafeEmailToScimEmail :: Email -> Email.Email
unsafeEmailToScimEmail email =
  let typ :: Maybe Text = Nothing
      primary :: Maybe Scim.ScimBool = Nothing
      value = Email.EmailAddress2 $ Email.unsafeEmailAddress (cs . emailLocal $ email) (cs . emailDomain $ email)
   in Email.Email {..}

spec :: Spec
spec = do
  describe "mkAuth" $ do
    describe "preference of scim fields" $ do
      it "exernalId is used as email if emails is empty" . property $
        \(email :: Email, mbIdP :: Maybe IdP, teamId' :: TeamId) ->
          let teamId = maybe teamId' (_wiTeam . _idpExtraInfo) mbIdP
              emails = []
              authId :: Either Scim.ScimError AuthId = mkAuth (Just (fromEmail email)) mbIdP teamId emails
           in counterexample (show authId) $
                (fmap ewsEmail . authIdScimEmailWithSource <$> authId) === Right (Just email)

      it "email field is used as email if externalId contains a parseable email address, but emails field is non-empty" . property $
        \(emailInExtId :: Email, emailInField :: Email, mbIdP :: Maybe IdP, teamId' :: TeamId) ->
          let teamId = maybe teamId' (_wiTeam . _idpExtraInfo) mbIdP
              emails = [unsafeEmailToScimEmail emailInField]
              authId :: Either Scim.ScimError AuthId = mkAuth (Just (fromEmail emailInExtId)) mbIdP teamId emails
           in counterexample (show authId) $
                (fmap ewsEmail . authIdScimEmailWithSource <$> authId) === Right (Just emailInField)

  describe "toScimStoredUser'" $ do
    it "works" $ do
      let usr :: Scim.User SparTag
          usr =
            Scim.User
              { Scim.schemas =
                  [ Scim.User20,
                    Scim.CustomSchema "urn:wire:scim:schemas:profile:1.0",
                    Scim.CustomSchema "urn:ietf:params:scim:schemas:extension:wire:1.0:User"
                  ],
                Scim.userName = "02b35298-088f-11e9-b4a4-478635dd0d2b",
                Scim.externalId = Just "c1704a48-0a1e-11e9-9186-9b185fe892e8",
                Scim.name =
                  Just
                    ( ScimN.Name
                        { ScimN.formatted = Nothing,
                          ScimN.familyName = Just "",
                          ScimN.givenName = Just "",
                          ScimN.middleName = Nothing,
                          ScimN.honorificPrefix = Nothing,
                          ScimN.honorificSuffix = Nothing
                        }
                    ),
                Scim.displayName = Just "67d0268e-088e-11e9-a400-b71b4d4d2275",
                Scim.nickName = Nothing,
                Scim.profileUrl = Nothing,
                Scim.title = Nothing,
                Scim.userType = Nothing,
                Scim.preferredLanguage = Nothing,
                Scim.locale = Nothing,
                Scim.active = Nothing,
                Scim.password = Nothing,
                Scim.emails = [],
                Scim.phoneNumbers = [],
                Scim.ims = [],
                Scim.photos = [],
                Scim.addresses = [],
                Scim.entitlements = [],
                Scim.roles = [],
                Scim.x509Certificates = [],
                Scim.extra = ScimUserExtra mempty
              }
          meta :: Scim.Meta
          meta =
            Scim.Meta
              { Scim.resourceType = ScimR.UserResource,
                Scim.created = fromUTCTimeMillis now,
                Scim.lastModified = fromUTCTimeMillis now,
                Scim.version = Scim.Weak "46246ab15ccab8a70b59f97f7182d6fb557dd454c0f06cdcb83d99d027cff08e",
                Scim.location =
                  Scim.URI . fromJust $
                    Network.URI.parseURI
                      "https://127.0.0.1/scim/v2/Users/90b5ee1c-088e-11e9-9a16-73f80f483813"
              }
          SAML.Time (toUTCTimeMillis -> now) = SAML.unsafeReadTime "1918-04-14T09:58:58.457Z"
          baseuri :: URI =
            either (error . show) id $
              URI.ByteString.parseURI laxURIParserOptions "https://127.0.0.1/scim/v2/"
          uid = Id . fromJust . UUID.fromText $ "90b5ee1c-088e-11e9-9a16-73f80f483813"

          result :: ScimC.StoredUser SparTag
          result = toScimStoredUser' now now baseuri uid usr

      Scim.meta result `shouldBe` meta
      Scim.value (Scim.thing result) `shouldBe` usr

  describe "ScimUserExtra" $ do
    it "roundtrips" . property $ do
      \(sue :: ScimUserExtra) ->
        eitherDecode' (encode sue) `shouldBe` Right sue

    describe "Patchable" $ do
      it "can add to rich info map" $ do
        let operationJSON =
              [aesonQQ|{
                                       "schemas" : [ "urn:ietf:params:scim:api:messages:2.0:PatchOp" ],
                                       "operations" : [{
                                         "op" : "add",
                                         "path" : "urn:ietf:params:scim:schemas:extension:wire:1.0:User:newAttr",
                                         "value" : "newValue"
                                       }]
                                     }|]
        let (Aeson.Success (PatchOp [operation])) = Aeson.parse (parseJSON @(PatchOp SparTag)) operationJSON
        applyOperation (ScimUserExtra mempty) operation
          `shouldBe` (Right (ScimUserExtra (RichInfo (RichInfoAssocList [RichField "newAttr" "newValue"]))))
      it "can replace in rich info map" $ do
        let operationJSON =
              [aesonQQ|{
                                       "schemas" : [ "urn:ietf:params:scim:api:messages:2.0:PatchOp" ],
                                       "operations" : [{
                                         "op" : "replace",
                                         "path" : "urn:ietf:params:scim:schemas:extension:wire:1.0:User:oldAttr",
                                         "value" : "newValue"
                                       }]
                                     }|]
        let (Aeson.Success (PatchOp [operation])) = Aeson.parse (parseJSON @(PatchOp SparTag)) operationJSON
        applyOperation (ScimUserExtra (RichInfo (RichInfoAssocList [RichField "oldAttr" "oldValue"]))) operation
          `shouldBe` (Right (ScimUserExtra (RichInfo (RichInfoAssocList [RichField "oldAttr" "newValue"]))))
      it "treats rich info map case insensitively" $ do
        let operationJSON =
              [aesonQQ|{
                                       "schemas" : [ "urn:ietf:params:scim:api:messages:2.0:PatchOp" ],
                                       "operations" : [{
                                         "op" : "replace",
                                         "path" : "urn:ietf:params:scim:schemas:extension:wire:1.0:User:OLDATTR",
                                         "value" : "newValue"
                                       }]
                                     }|]
        let (Aeson.Success (PatchOp [operation])) = Aeson.parse (parseJSON @(PatchOp SparTag)) operationJSON
        applyOperation (ScimUserExtra (RichInfo (RichInfoAssocList [RichField "oldAttr" "oldValue"]))) operation
          `shouldBe` (Right (ScimUserExtra (RichInfo (RichInfoAssocList [RichField "oldAttr" "newValue"]))))
      it "can remove from rich info map" $ do
        let operationJSON =
              [aesonQQ|{
                                       "schemas" : [ "urn:ietf:params:scim:api:messages:2.0:PatchOp" ],
                                       "operations" : [{
                                         "op" : "remove",
                                         "path" : "urn:ietf:params:scim:schemas:extension:wire:1.0:User:oldAttr"
                                       }]
                                     }|]
        let (Aeson.Success (PatchOp [operation])) = Aeson.parse (parseJSON @(PatchOp SparTag)) operationJSON
        applyOperation (ScimUserExtra (RichInfo (RichInfoAssocList [RichField "oldAttr" "oldValue"]))) operation
          `shouldBe` (Right (ScimUserExtra mempty))
      it "adds new fields to rich info assoc list at the end" $ do
        let operationJSON =
              [aesonQQ|{
                                       "schemas" : [ "urn:ietf:params:scim:api:messages:2.0:PatchOp" ],
                                       "operations" : [{
                                         "op" : "add",
                                         "path" : "urn:wire:scim:schemas:profile:1.0:newAttr",
                                         "value" : "newValue"
                                       }]
                                     }|]
        let (Aeson.Success (PatchOp [operation])) = Aeson.parse (parseJSON @(PatchOp SparTag)) operationJSON
        applyOperation (ScimUserExtra (RichInfo (RichInfoAssocList [RichField "oldAttr" "oldValue"]))) operation
          `shouldBe` (Right (ScimUserExtra (RichInfo (RichInfoAssocList [RichField "oldAttr" "oldValue", RichField "newAttr" "newValue"]))))
      it "can replace in rich info assoc list while maintaining order" $ do
        let operationJSON =
              [aesonQQ|{
                                       "schemas" : [ "urn:ietf:params:scim:api:messages:2.0:PatchOp" ],
                                       "operations" : [{
                                         "op" : "replace",
                                         "path" : "urn:wire:scim:schemas:profile:1.0:secondAttr",
                                         "value" : "newSecondVal"
                                       }]
                                     }|]
        let (Aeson.Success (PatchOp [operation])) = Aeson.parse (parseJSON @(PatchOp SparTag)) operationJSON
        let origAssocList =
              [ RichField "firstAttr" "firstVal",
                RichField "secondAttr" "secondVal",
                RichField "thirdAttr" "thirdVal"
              ]
        let expectedAssocList =
              [ RichField "firstAttr" "firstVal",
                RichField "secondAttr" "newSecondVal",
                RichField "thirdAttr" "thirdVal"
              ]
        applyOperation (ScimUserExtra (RichInfo (RichInfoAssocList origAssocList))) operation
          `shouldBe` (Right (ScimUserExtra (RichInfo (RichInfoAssocList expectedAssocList))))
      it "can remove from rich info assoc list" $ do
        let operationJSON =
              [aesonQQ|{
                                       "schemas" : [ "urn:ietf:params:scim:api:messages:2.0:PatchOp" ],
                                       "operations" : [{
                                         "op" : "remove",
                                         "path" : "urn:wire:scim:schemas:profile:1.0:oldAttr"
                                       }]
                                     }|]
        let (Aeson.Success (PatchOp [operation])) = Aeson.parse (parseJSON @(PatchOp SparTag)) operationJSON
        applyOperation (ScimUserExtra (RichInfo (RichInfoAssocList [RichField "oldAttr" "oldValue"]))) operation
          `shouldBe` (Right (ScimUserExtra mempty))
      it "throws error if asked to patch an recognized schema" $ do
        let schema = Just (CustomSchema "wrong-schema")
            path = Just (NormalPath (AttrPath schema "oldAttr" Nothing))
            operation = Operation Remove path Nothing
        isLeft (applyOperation (ScimUserExtra (RichInfo (RichInfoAssocList [RichField "oldAttr" "oldValue"]))) operation)
          `shouldBe` True
      it "treats rich info assoc list case insensitively" $ do
        let operationJSON =
              [aesonQQ|{
                                       "schemas" : [ "urn:ietf:params:scim:api:messages:2.0:PatchOp" ],
                                       "operations" : [{
                                         "op" : "replace",
                                         "path" : "urn:wire:scim:schemas:profile:1.0:secondAttr",
                                         "value" : "newSecondVal"
                                       }]
                                     }|]
        let (Aeson.Success (PatchOp [operation])) = Aeson.parse (parseJSON @(PatchOp SparTag)) operationJSON
        let origAssocList =
              [ RichField "firstAttr" "firstVal",
                RichField "SECONDATTR" "secondVal",
                RichField "thirdAttr" "thirdVal"
              ]
        let expectedAssocList =
              [ RichField "firstAttr" "firstVal",
                RichField "secondAttr" "newSecondVal",
                RichField "thirdAttr" "thirdVal"
              ]
        applyOperation (ScimUserExtra (RichInfo (RichInfoAssocList origAssocList))) operation
          `shouldBe` (Right (ScimUserExtra (RichInfo (RichInfoAssocList expectedAssocList))))

instance Arbitrary ScimUserExtra where
  arbitrary = ScimUserExtra <$> arbitrary
