{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
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

module Test.Schema.UserSpec
  ( spec,
  )
where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Either (isLeft, isRight)
import Data.Foldable (for_)
import Data.Text (Text)
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Lens.Micro
import Network.URI.Static (uri)
import Test.Hspec
import Test.Schema.Util (genUri, mk_prop_caseInsensitive)
import Text.Email.Validate (emailAddress, validate)
import qualified Web.Scim.Class.User as UserClass
import Web.Scim.Filter (AttrPath (..))
import Web.Scim.Schema.Common (ScimBool (ScimBool), URI (..), WithId (..), lowerKey)
import qualified Web.Scim.Schema.ListResponse as ListResponse
import Web.Scim.Schema.Meta (ETag (Strong, Weak), Meta (..), WithMeta (..))
import Web.Scim.Schema.PatchOp (Op (..), Operation (..), PatchOp (..), Patchable (..), Path (..))
import qualified Web.Scim.Schema.PatchOp as PatchOp
import Web.Scim.Schema.Schema (Schema (..))
import Web.Scim.Schema.User (NoUserExtra (..), User (..))
import qualified Web.Scim.Schema.User as User
import Web.Scim.Schema.User.Address as Address
import Web.Scim.Schema.User.Certificate as Certificate
import Web.Scim.Schema.User.Email as Email
import Web.Scim.Schema.User.IM as IM
import Web.Scim.Schema.User.Name as Name
import Web.Scim.Schema.User.Phone as Phone
import Web.Scim.Schema.User.Photo as Photo
import Web.Scim.Test.Util

prop_roundtrip :: Property
prop_roundtrip = property $ do
  user <- forAll genUser
  tripping user toJSON fromJSON

type PatchTag = TestTag Text () () UserExtraPatch

type UserExtraPatch = KeyMap.KeyMap Text

spec :: Spec
spec = do
  describe "scimEmailsToEmailAddress" $ do
    let Right adr1 = validate "one@example.com"
        Right adr2 = validate "two@example.com"
        Right adr3 = validate "three@example.com"

        false1 = Nothing
        false2 = Just (ScimBool False)
        true = Just (ScimBool True)

    it "returns Nothing if empty" $ do
      scimEmailsToEmailAddress [] `shouldBe` Nothing

    it "returns first primary if it exists" $ do
      scimEmailsToEmailAddress
        [ Email Nothing (EmailAddress adr1) false1,
          Email Nothing (EmailAddress adr2) false2,
          Email (Just "this is ignored") (EmailAddress adr3) true
        ]
        `shouldBe` Just adr3

    it "returns first entry if no primary exists" $ do
      scimEmailsToEmailAddress
        [ Email Nothing (EmailAddress adr1) false1,
          Email Nothing (EmailAddress adr2) false2
        ]
        `shouldBe` Just adr1
      scimEmailsToEmailAddress
        [ Email Nothing (EmailAddress adr1) false2,
          Email Nothing (EmailAddress adr2) false1
        ]
        `shouldBe` Just adr1

  describe "applyPatch" $ do
    it "only applies patch for supported fields" $ do
      let schemas' = []
      let extras = KeyMap.empty
      let user :: User PatchTag = User.empty schemas' "hello" extras
      for_
        [ ("username", String "lol"),
          ("displayname", String "lol"),
          ("externalid", String "lol"),
          ("active", Bool True)
        ]
        $ \(key, upd) -> do
          let operation = Operation Replace (Just (NormalPath (AttrPath Nothing key Nothing))) (Just upd)
          let patchOp = PatchOp [operation]
          User.applyPatch user patchOp `shouldSatisfy` isRight
    it "does not support multi-value attributes" $ do
      let schemas' = []
      let extras = KeyMap.empty
      let user :: User PatchTag = User.empty schemas' "hello" extras
      for_
        [ ("schemas", toJSON @[Schema] mempty),
          ("name", toJSON @Name emptyName),
          ("nickName", toJSON @Text mempty),
          ("profileUrl", toJSON @URI (URI [uri|https://example.com|])),
          ("title", toJSON @Text mempty),
          ("userType", toJSON @Text mempty),
          ("preferredLanguage", toJSON @Text mempty),
          ("locale", toJSON @Text mempty),
          ("password", toJSON @Text mempty),
          ("emails", toJSON @[Email] mempty),
          ("phoneNumbers", toJSON @[Phone] mempty),
          ("ims", toJSON @[IM] mempty),
          ("photos", toJSON @[Photo] mempty),
          ("addresses", toJSON @[Address] mempty),
          ("entitlements", toJSON @[Text] mempty),
          ("x509Certificates", toJSON @[Certificate] mempty)
        ]
        $ \(key, upd) -> do
          let operation = Operation Replace (Just (NormalPath (AttrPath Nothing key Nothing))) (Just upd)
          let patchOp = PatchOp [operation]
          User.applyPatch user patchOp `shouldSatisfy` isLeft
    it "applies patch to `extra`" $ do
      let schemas' = []
      let extras = KeyMap.empty
      let user :: User PatchTag = User.empty schemas' "hello" extras
      let Right programmingLanguagePath = PatchOp.parsePath (User.supportedSchemas @PatchTag) "urn:hscim:test:programmingLanguage"
      let operation = Operation Replace (Just programmingLanguagePath) (Just (toJSON @Text "haskell"))
      let patchOp = PatchOp [operation]
      User.extra <$> User.applyPatch user patchOp `shouldBe` Right (KeyMap.singleton "programmingLanguage" "haskell")
  describe "JSON serialization" $ do
    it "handles all fields" $ do
      require prop_roundtrip
      toJSON completeUser `shouldBe` completeUserJson
      eitherDecode (encode completeUserJson) `shouldBe` Right completeUser
    it "has defaults for all optional and multi-valued fields" $ do
      toJSON minimalUser `shouldBe` minimalUserJson
      eitherDecode (encode minimalUserJson) `shouldBe` Right minimalUser
    it "treats 'null' and '[]' as absence of fields"
      $ eitherDecode (encode minimalUserJsonRedundant)
      `shouldBe` Right minimalUser
    it "allows casing variations in field names" $ do
      require $ mk_prop_caseInsensitive genUser
      require $ mk_prop_caseInsensitive (ListResponse.fromList . (: []) <$> genStoredUser)
      eitherDecode (encode minimalUserJsonNonCanonical) `shouldBe` Right minimalUser
    it "doesn't require the 'schemas' field"
      $ eitherDecode (encode minimalUserJsonNoSchemas)
      `shouldBe` Right minimalUser
    it "doesn't add 'extra' if it's an empty object" $ do
      toJSON (extendedUser UserExtraEmpty) `shouldBe` extendedUserEmptyJson
      eitherDecode (encode extendedUserEmptyJson)
        `shouldBe` Right (extendedUser UserExtraEmpty)
    it "encodes and decodes 'extra' correctly" $ do
      toJSON (extendedUser (UserExtraObject "foo")) `shouldBe` extendedUserObjectJson
      eitherDecode (encode extendedUserObjectJson)
        `shouldBe` Right (extendedUser (UserExtraObject "foo"))

genName :: Gen Name
genName =
  Name
    <$> Gen.maybe (Gen.text (Range.constant 0 20) Gen.unicode)
    <*> Gen.maybe (Gen.text (Range.constant 0 20) Gen.unicode)
    <*> Gen.maybe (Gen.text (Range.constant 0 20) Gen.unicode)
    <*> Gen.maybe (Gen.text (Range.constant 0 20) Gen.unicode)
    <*> Gen.maybe (Gen.text (Range.constant 0 20) Gen.unicode)
    <*> Gen.maybe (Gen.text (Range.constant 0 20) Gen.unicode)

genStoredUser :: Gen (UserClass.StoredUser (TestTag Text () () NoUserExtra))
genStoredUser = do
  m <- genMeta
  i <- Gen.element ["wef", "asdf", "@", "#", "1"]
  WithMeta m . WithId i <$> genUser

genMeta :: Gen Meta
genMeta =
  Meta
    <$> Gen.enumBounded
    <*> Gen.element [read "2021-08-23 13:13:31.450140036 UTC", read "2019-01-01 09:55:59 UTC"]
    <*> Gen.element [read "2021-08-23 13:13:31.450140036 UTC", read "2022-01-01 09:55:59 UTC"]
    <*> (Gen.element [Weak, Strong] <*> Gen.text (Range.constant 0 20) Gen.unicode)
    <*> genUri

-- TODO(arianvp) Generate the lists too, but first need better support for SCIM
-- lists in the first place
genUser :: Gen (User (TestTag Text () () NoUserExtra))
genUser = do
  let schemas' = [User20] -- TODO random schemas or?
  userName' <- Gen.text (Range.constant 0 20) Gen.unicode
  externalId' <- Gen.maybe $ Gen.text (Range.constant 0 20) Gen.unicode
  name' <- Gen.maybe genName
  displayName' <- Gen.maybe $ Gen.text (Range.constant 0 20) Gen.unicode
  nickName' <- Gen.maybe $ Gen.text (Range.constant 0 20) Gen.unicode
  profileUrl' <- Gen.maybe $ genUri
  title' <- Gen.maybe $ Gen.text (Range.constant 0 20) Gen.unicode
  userType' <- Gen.maybe $ Gen.text (Range.constant 0 20) Gen.unicode
  preferredLanguage' <- Gen.maybe $ Gen.text (Range.constant 0 20) Gen.unicode
  locale' <- Gen.maybe $ Gen.text (Range.constant 0 20) Gen.unicode
  active' <- Gen.maybe $ (ScimBool <$> Gen.bool)
  password' <- Gen.maybe $ Gen.text (Range.constant 0 20) Gen.unicode
  let emails' = [] -- Gen.list (Range.constant 0 20) genEmail
  let phoneNumbers' = [] -- Gen.list (Range.constant 0 20) genPhone
  let ims' = [] -- Gen.list (Range.constant 0 20) genIM
  let photos' = [] -- Gen.list (Range.constant 0 20) genPhoto
  let addresses' = [] -- Gen.list (Range.constant 0 20) genAddress
  let entitlements' = [] -- Gen.list (Range.constant 0 20) (Gen.text (Range.constant 0 20) Gen.unicode)
  let roles' = [] -- Gen.list (Range.constant 0 20) (Gen.text (Range.constant 0 10) Gen.unicode)
  let x509Certificates' = [] -- Gen.list (Range.constant 0 20) genCertificate
  pure
    $ User
      { schemas = schemas',
        userName = userName',
        externalId = externalId',
        name = name',
        displayName = displayName',
        nickName = nickName',
        profileUrl = profileUrl',
        title = title',
        userType = userType',
        preferredLanguage = preferredLanguage',
        locale = locale',
        active = active',
        password = password',
        emails = emails',
        phoneNumbers = phoneNumbers',
        ims = ims',
        photos = photos',
        addresses = addresses',
        entitlements = entitlements',
        roles = roles',
        x509Certificates = x509Certificates',
        extra = NoUserExtra
      }

-- | A 'User' with all attributes present.
completeUser :: User (TestTag Text () () NoUserExtra)
completeUser =
  User
    { schemas = [User20],
      userName = "sample userName",
      externalId = Just "sample externalId",
      name =
        Just
          $ Name
            { Name.formatted = Just "sample formatted name",
              Name.familyName = Nothing,
              Name.givenName = Nothing,
              Name.middleName = Nothing,
              Name.honorificPrefix = Nothing,
              Name.honorificSuffix = Nothing
            },
      displayName = Just "sample displayName",
      nickName = Just "sample nickName",
      profileUrl = Just (URI [uri|https://example.com|]),
      title = Just "sample title",
      userType = Just "sample userType",
      preferredLanguage = Just "da, en-gb;q=0.8, en;q=0.7",
      locale = Just "en-US",
      active = Just (ScimBool True),
      password = Just "sample password",
      emails =
        [ Email
            { Email.typ = Just "work",
              Email.value =
                maybe
                  (error "couldn't parse email")
                  EmailAddress
                  (emailAddress "user@example.com"),
              Email.primary = Nothing
            }
        ],
      phoneNumbers =
        [ Phone
            { Phone.typ = Just "work",
              Phone.value = Just "+15417543010"
            }
        ],
      ims =
        [ IM
            { IM.typ = Just "Wire",
              IM.value = Just "@user"
            }
        ],
      photos =
        [ Photo
            { Photo.typ = Just "userpic",
              Photo.value = Just (URI [uri|https://example.com/userpic.png|])
            }
        ],
      addresses =
        [ Address
            { Address.formatted = Just "sample Address",
              Address.streetAddress = Nothing,
              Address.locality = Nothing,
              Address.region = Nothing,
              Address.postalCode = Nothing,
              Address.country = Nothing,
              Address.typ = Just "home",
              Address.primary = Just (ScimBool True)
            }
        ],
      entitlements = ["sample entitlement"],
      roles = ["sample role"],
      x509Certificates =
        [ Certificate
            { Certificate.typ = Just "sample certificate type",
              Certificate.value = Just "sample certificate"
            }
        ],
      extra = NoUserExtra
    }

-- | Reference encoding of 'completeUser'.
completeUserJson :: Value
completeUserJson =
  [scim|
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
minimalUser :: User (TestTag Text () () NoUserExtra)
minimalUser = User.empty [User20] "sample userName" NoUserExtra

-- | Reference encoding of 'minimalUser'.
minimalUserJson :: Value
minimalUserJson =
  [scim|
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
minimalUserJsonRedundant =
  [scim|
{
  "schemas": [
    "urn:ietf:params:scim:schemas:core:2.0:User"
  ],
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
minimalUserJsonNonCanonical =
  [scim|
{
  "schemas": [
    "urn:ietf:params:scim:schemas:core:2.0:User"
  ],
  "USERname": "sample userName"
}
|]

-- | An encoding of 'minimalUser' without the @schemas@ field.
minimalUserJsonNoSchemas :: Value
minimalUserJsonNoSchemas =
  [scim|
{
  "userName": "sample userName"
}
|]

data UserExtraTest = UserExtraEmpty | UserExtraObject {test :: Text}
  deriving (Show, Eq)

instance FromJSON UserExtraTest where
  parseJSON = withObject "UserExtraObject" $ \(lowercase -> o) -> do
    o .:? "urn:hscim:test" >>= \case
      Nothing -> pure UserExtraEmpty
      Just (lowercase -> o2) -> UserExtraObject <$> o2 .: "test"
    where
      lowercase = KeyMap.fromList . map (over _1 lowerKey) . KeyMap.toList

instance ToJSON UserExtraTest where
  toJSON UserExtraEmpty = object []
  toJSON (UserExtraObject t) =
    object ["urn:hscim:test" .= object ["test" .= t]]

instance Patchable UserExtraTest where
  applyOperation _ _ = undefined

-- | A 'User' with extra fields present.
extendedUser :: UserExtraTest -> User (TestTag Text () () UserExtraTest)
extendedUser e =
  User.empty [User20, CustomSchema "urn:hscim:test"] "sample userName" e

-- | Encoding of @extendedUser UserExtraEmpty@.
extendedUserEmptyJson :: Value
extendedUserEmptyJson =
  [scim|
{
  "schemas": [
    "urn:ietf:params:scim:schemas:core:2.0:User",
    "urn:hscim:test"
  ],
  "userName": "sample userName"
}
|]

-- | Encoding of @extendedUser (UserExtraObject "foo")@.
extendedUserObjectJson :: Value
extendedUserObjectJson =
  [scim|
{
  "schemas": [
    "urn:ietf:params:scim:schemas:core:2.0:User",
    "urn:hscim:test"
  ],
  "userName": "sample userName",
  "urn:hscim:test": {
    "test": "foo"
  }
}
|]
