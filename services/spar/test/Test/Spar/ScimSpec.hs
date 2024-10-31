{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

module Test.Spar.ScimSpec where

import Control.Lens (view)
import Data.Aeson
import Data.Aeson.QQ (aesonQQ)
import qualified Data.Aeson.Types as Aeson
import Data.Id
import Data.Json.Util (fromUTCTimeMillis, toUTCTimeMillis)
import qualified Data.UUID as UUID
import Imports
import Network.URI (parseURI)
import qualified SAML2.WebSSO as SAML
import Spar.Scim
import Spar.Scim.Types (normalizeLikeStored)
import Test.Hspec
import Test.QuickCheck
import URI.ByteString
import qualified Web.Scim.Class.User as ScimC
import Web.Scim.Filter (AttrPath (..))
import qualified Web.Scim.Schema.Common as Scim
import qualified Web.Scim.Schema.Meta as Scim
import Web.Scim.Schema.PatchOp (Op (Remove), Operation (..), PatchOp (..), Path (NormalPath), applyOperation)
import qualified Web.Scim.Schema.ResourceType as ScimR
import Web.Scim.Schema.Schema as Scim
import Web.Scim.Schema.User as Scim
import qualified Web.Scim.Schema.User.Name as ScimN
import Wire.API.User.RichInfo

spec :: Spec
spec = describe "toScimStoredUser" $ do
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
              Scim.version = Scim.Weak "ee3ebd2f5722d0b95e20ded809a81321b3810543457ca6ca459d822294c12c71",
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
        result = toScimStoredUser now now baseuri uid usr
    Scim.meta result `shouldBe` meta
    Scim.value (Scim.thing result) `shouldBe` usr
  it "roundtrips" . property $ do
    \(sue :: ScimUserExtra) ->
      eitherDecode' (encode sue) `shouldBe` Right sue

  describe "ScimUserExtra" $ do
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
          `shouldBe` Right (ScimUserExtra (RichInfo (mkRichInfoAssocList [RichField "newAttr" "newValue"])))
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
        applyOperation (ScimUserExtra (RichInfo (mkRichInfoAssocList [RichField "oldAttr" "oldValue"]))) operation
          `shouldBe` Right (ScimUserExtra (RichInfo (mkRichInfoAssocList [RichField "oldAttr" "newValue"])))
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
        applyOperation (ScimUserExtra (RichInfo (mkRichInfoAssocList [RichField "oldAttr" "oldValue"]))) operation
          `shouldBe` Right (ScimUserExtra (RichInfo (mkRichInfoAssocList [RichField "oldAttr" "newValue"])))
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
        applyOperation (ScimUserExtra (RichInfo (mkRichInfoAssocList [RichField "oldAttr" "oldValue"]))) operation
          `shouldBe` Right (ScimUserExtra mempty)
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
        applyOperation (ScimUserExtra (RichInfo (mkRichInfoAssocList [RichField "oldAttr" "oldValue"]))) operation
          `shouldBe` Right (ScimUserExtra (RichInfo (mkRichInfoAssocList [RichField "oldAttr" "oldValue", RichField "newAttr" "newValue"])))
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
        applyOperation (ScimUserExtra (RichInfo (mkRichInfoAssocList origAssocList))) operation
          `shouldBe` Right (ScimUserExtra (RichInfo (mkRichInfoAssocList expectedAssocList)))
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
        applyOperation (ScimUserExtra (RichInfo (mkRichInfoAssocList [RichField "oldAttr" "oldValue"]))) operation
          `shouldBe` Right (ScimUserExtra mempty)
      it "throws error if asked to patch an recognized schema" $ do
        let schema = Just (CustomSchema "wrong-schema")
            path = Just (NormalPath (AttrPath schema "oldAttr" Nothing))
            operation = Operation Remove path Nothing
        isLeft (applyOperation (ScimUserExtra (RichInfo (mkRichInfoAssocList [RichField "oldAttr" "oldValue"]))) operation)
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
        applyOperation (ScimUserExtra (RichInfo (mkRichInfoAssocList origAssocList))) operation
          `shouldBe` Right (ScimUserExtra (RichInfo (mkRichInfoAssocList expectedAssocList)))

  describe "normalization" $ do
    let usr :: User SparTag
        usr = User {schemas = [PatchOp20, CustomSchema "asdf", ResourceType20, CustomSchema "", CustomSchema "", Group20, ServiceProviderConfig20], userName = ">/nP6S3|)RBmeJ/'PqYzRr\96446F\42072HS_izq", externalId = Just "nZ\179219)DZ\13375\\v", name = Nothing, displayName = Just "`b++0RD Ty~ z/S`Z\\\"bDE-\13666\&32>%<\189311", nickName = Nothing, profileUrl = Nothing, title = Nothing, userType = Nothing, preferredLanguage = Nothing, locale = Nothing, active = Just (Scim.ScimBool True), password = Nothing, emails = [], phoneNumbers = [], ims = [], photos = [], addresses = [], entitlements = [], roles = [], x509Certificates = [], extra = ScimUserExtra {_sueRichInfo = RichInfo {unRichInfo = assocs}}}

        assocs :: RichInfoAssocList
        assocs = mkRichInfoAssocList [RichField {richFieldType = "0-plIe\176041Sdu]\129492ouXy*]j\49123`jDNJ:N%\32939\&6\183443\\>HSi\6502q,\28951wZ].\11331w`", richFieldValue = "C ny6Nx0f&b\121034\29092r"}, RichField {richFieldType = "[&c;VP9\42304Q.I\43963OS\83057}G ]\175364xYLqO\156677q*ZBtZ`vKc", richFieldValue = "+FEv\28180"}, RichField {richFieldType = "}121@^z{", richFieldValue = "{KZQqjqs Py%ETB>;y1}\142167\181794\164475p"}, RichField {richFieldType = "\48098\&2#-p\68080\&9\37971|\190007K|m(", richFieldValue = ":j7\83424lQ\19571\188281*[)D8\50056\9019n\189416\100233]*!={FX|/!!&my]+8\175071\135759\&0\13316K'(\14120\172092w,2"}, RichField {richFieldType = "\50520MX>\\kQcBz\169538\147873\\\177286FqS!GW]#\20027_n", richFieldValue = "53\190108.?%t[ &9=hd9t:}Q@yj#w~B\164946B# fs!\39091}eEP"}, RichField {richFieldType = "sE7hmj\164437:", richFieldValue = "ns\"EJftf6~g5U\"&tt\20456@]M"}, RichField {richFieldType = "\172698p\41097sHk \37897X0Io\8286OU\173780\18370h\46873&GAOpuQU+T)]rC\5068WCA\68875(-\175596'", richFieldValue = "lRiP"}]

    describe "'normalizeLikeStored'" $ do
      it "works (counter-example of earlier bug)" $ do
        let f = length . unRichInfoAssocList . unRichInfo . view sueRichInfo . Scim.extra
        f (normalizeLikeStored usr) `shouldBe` f usr
        normalizeLikeStored usr `shouldBe` usr

      it "keeps (already normalized) user record intact (property)" . property $
        \(usr' :: Scim.User SparTag) -> counterexample (show usr') $ do
          normalizeLikeStored usr' `shouldBe` usr'
