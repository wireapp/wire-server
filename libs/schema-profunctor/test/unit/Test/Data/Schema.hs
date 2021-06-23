-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Data.Schema where

import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens (Prism', at, ix, makePrisms, nullOf, prism', (?~), (^.), _1)
import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), Value (..), decode, encode, fromJSON)
import Data.Aeson.QQ
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Proxy
import Data.Schema hiding (getName)
import qualified Data.Swagger as S
import qualified Data.Swagger.Declare as S
import qualified Data.Text as Text
import Imports
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "Swagger.Typed"
    [ testFooToJSON,
      testFooFromJSON,
      testFooFromJSONFailure,
      testFooSchema,
      testBarAToJSON,
      testBarAFromJSON,
      testBarBToJSON,
      testBarBFromJSON,
      testAccessToJSON,
      testAccessFromJSON,
      testUser1ToJSON,
      testUser1FromJSON,
      testUser2ToJSON,
      testUser2FromJSON,
      testUserSchema,
      testTaggedObjectToJSON,
      testTaggedObjectFromJSON,
      testTaggedObject2ToJSON,
      testTaggedObject2FromJSON,
      testTaggedObject3FromJSON,
      testNonEmptyParseFailure,
      testNonEmptyParseSuccess,
      testNonEmptyToJSON,
      testNonEmptySchema,
      testRefField,
      testRmClientWrong,
      testRmClient
    ]

testFooToJSON :: TestTree
testFooToJSON =
  testCase "toJSON Foo" $
    assertEqual
      "toJSON should match handwritten JSON"
      exampleFooJSON
      (toJSON exampleFoo)

testFooFromJSON :: TestTree
testFooFromJSON =
  testCase "fromJSON Foo" $
    assertEqual
      "fromJSON should match example"
      (Success exampleFoo)
      (fromJSON exampleFooJSON)

testFooFromJSONFailure :: TestTree
testFooFromJSONFailure =
  testCase "fromJSON Foo failure" $
    case fromJSON @Foo exampleFooInvalidJSON of
      Success _ -> assertFailure "fromJSON should fail"
      Error err -> do
        assertBool
          "fromJSON error should mention missing key"
          ("\"str\"" `isInfixOf` err)

testFooSchema :: TestTree
testFooSchema =
  testCase "Foo schema" $ do
    let s = S.toSchema (Proxy @Foo)
    assertEqual
      "Description should match"
      (Just "A Foo object")
      (s ^. description)
    assertEqual
      "a, b and str should be required"
      ["a", "b", "str"]
      (s ^. S.required)
    assertEqual
      "Schema for \"a\" should be referenced"
      (Just (S.Ref (S.Reference "A")))
      (s ^. S.properties . at "a")
    case s ^. S.properties . at "str" of
      Nothing -> assertFailure "\"str\" field should be present"
      Just (S.Ref _) ->
        assertFailure "Schema for \"str\" field should be inlined"
      Just (S.Inline _) -> pure ()

testBarAToJSON :: TestTree
testBarAToJSON =
  testCase "toJSON BarA" $
    assertEqual
      "toJSON should match handwritten JSON"
      exampleBarAJSON
      (toJSON exampleBarA)

testBarAFromJSON :: TestTree
testBarAFromJSON =
  testCase "fromJSON BarA" $
    assertEqual
      "fromJSON should match example"
      (Success exampleBarA)
      (fromJSON exampleBarAJSON)

testBarBToJSON :: TestTree
testBarBToJSON =
  testCase "toJSON BarB" $
    assertEqual
      "toJSON should match handwritten JSON"
      exampleBarBJSON
      (toJSON exampleBarB)

testBarBFromJSON :: TestTree
testBarBFromJSON =
  testCase "fromJSON BarB" $
    assertEqual
      "fromJSON should match example"
      (Success exampleBarB)
      (fromJSON exampleBarBJSON)

testAccessToJSON :: TestTree
testAccessToJSON =
  testCase "toJSON Access" $
    assertEqual
      "toJSON should match handwritten JSON"
      "link"
      (toJSON Link)

testAccessFromJSON :: TestTree
testAccessFromJSON =
  testCase "fromJSON Access" $
    assertEqual
      "fromJSON should match example"
      (Success Link)
      (fromJSON "link")

testUser1ToJSON :: TestTree
testUser1ToJSON =
  testCase "toJSON User" $
    assertEqual
      "toJSON should match handwritten JSON"
      exampleUser1JSON
      (encode exampleUser1)

testUser1FromJSON :: TestTree
testUser1FromJSON =
  testCase "fromJSON User" $
    assertEqual
      "fromJSON should match example"
      (Just exampleUser1)
      (decode exampleUser1JSON)

testUser2ToJSON :: TestTree
testUser2ToJSON =
  testCase "toJSON User" $
    assertEqual
      "toJSON should match handwritten JSON"
      exampleUser2JSON
      (encode exampleUser2)

testUser2FromJSON :: TestTree
testUser2FromJSON =
  testCase "fromJSON User" $
    assertEqual
      "fromJSON should match example"
      (Just exampleUser2)
      (decode exampleUser2JSON)

testUserSchema :: TestTree
testUserSchema =
  testCase "User schema" $ do
    let s = S.toSchema (Proxy @User)
    assertEqual
      "only name should be required"
      ["name"]
      (s ^. S.required)

testTaggedObjectToJSON :: TestTree
testTaggedObjectToJSON =
  testCase "toJSON TaggedObject" $
    assertEqual
      "toJSON should match handwritten JSON"
      exampleTaggedObjectJSON
      (toJSON exampleTaggedObject)

testTaggedObjectFromJSON :: TestTree
testTaggedObjectFromJSON =
  testCase "fromJSON TaggedObject" $
    assertEqual
      "fromJSON should match example"
      (Success exampleTaggedObject)
      (fromJSON exampleTaggedObjectJSON)

testTaggedObject2ToJSON :: TestTree
testTaggedObject2ToJSON =
  testCase "toJSON TaggedObject 2" $
    assertEqual
      "toJSON should match handwritten JSON"
      exampleTaggedObject2JSON
      (toJSON exampleTaggedObject2)

testTaggedObject2FromJSON :: TestTree
testTaggedObject2FromJSON =
  testCase "fromJSON TaggedObject 2" $
    assertEqual
      "fromJSON should match example"
      (Success exampleTaggedObject2)
      (fromJSON exampleTaggedObject2JSON)

testTaggedObject3FromJSON :: TestTree
testTaggedObject3FromJSON =
  testCase "fromJSON TaggedObject failure" $
    case fromJSON @TaggedObject exampleTaggedObject3JSON of
      Success _ -> assertFailure "fromJSON should fail"
      Error err -> do
        assertBool
          "fromJSON error should mention missing key"
          ("\"tag1_data\"" `isInfixOf` err)

testNonEmptyParseFailure :: TestTree
testNonEmptyParseFailure =
  testCase "NonEmpty parse failure" $ do
    let invalidJSON = [aesonQQ|{"nl": []}|]
    case fromJSON @NonEmptyTest invalidJSON of
      Success _ -> assertFailure "fromJSON should fail"
      Error err -> do
        assertEqual
          "fromJSON error should mention that list is not empty"
          "Unexpected empty array found while parsing a NonEmpty"
          err

testNonEmptyParseSuccess :: TestTree
testNonEmptyParseSuccess =
  testCase "NonEmpty parse success" $ do
    let json = [aesonQQ|{"nl": ["something", "other thing"]}|]
        expected = NonEmptyTest ("something" :| ["other thing"])
    assertEqual
      "fromJSON should mention that list is not empty"
      (Success expected)
      (fromJSON json)

testNonEmptyToJSON :: TestTree
testNonEmptyToJSON =
  testCase "NonEmpty ToJSON" $ do
    let expected = [aesonQQ|{"nl": ["something", "other thing"]}|]
        testVal = NonEmptyTest ("something" :| ["other thing"])
    assertEqual
      "fromJSON should mention that list is not empty"
      expected
      (toJSON testVal)

testNonEmptySchema :: TestTree
testNonEmptySchema =
  testCase "NonEmpty Schema" $ do
    let sch = S.toSchema (Proxy @NonEmptyTest)
    case InsOrdHashMap.lookup "nl" $ sch ^. S.properties of
      Nothing -> assertFailure "expected schema to have a property called 'nl'"
      Just (S.Ref _) -> assertFailure "expected property 'nl' to have inline schema"
      Just (S.Inline nlSch) -> do
        assertEqual "type should be Array" (Just S.SwaggerArray) (nlSch ^. S.type_)
        assertEqual "minItems should be 1" (Just 1) (nlSch ^. S.minItems)

testRefField :: TestTree
testRefField =
  testCase "Reference in a field" $ do
    let (defs, _) = S.runDeclare (S.declareSchemaRef (Proxy @Named)) mempty
    assertBool "Referenced schema should be declared" $
      not . nullOf (ix "Name") $ defs

testRmClientWrong :: TestTree
testRmClientWrong =
  testCase "Parser succeeds when password is invalid" $ do
    let json = [aesonQQ|{"password": "a"}|]
        expected = RmClient Nothing
    assertEqual
      "fromJSON should succeed"
      (Success expected)
      (A.parse (schemaIn rmClientSchema) json)

testRmClient :: TestTree
testRmClient =
  testCase "Parser fails when password is invalid" $ do
    let json = [aesonQQ|{"password": "a"}|]
    case fromJSON @RmClient json of
      Success _ -> assertFailure "fromJSON should fail"
      Error err -> do
        assertEqual
          "fromJSON error should mention password too short"
          "password too short"
          err

---

data A = A {thing :: Text, other :: Int}
  deriving (Eq, Show)

instance ToSchema A where
  schema =
    object "A" $
      A
        <$> thing .= field "thing" schema
        <*> other .= field "other" schema

newtype B = B {bThing :: Int}
  deriving (Eq, Show)

instance ToSchema B where
  schema = object "B" $ B <$> bThing .= field "b_thing" schema

data Foo = Foo {fooA :: A, fooB :: B, fooStr :: Text}
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema Foo

exampleFoo :: Foo
exampleFoo = Foo (A "a-thing" 42) (B 99) "raw string"

exampleFooJSON :: Value
exampleFooJSON =
  [aesonQQ|{ "a": {"thing": "a-thing", "other": 42},
          "a_thing": "a-thing",
          "b": {"b_thing": 99},
          "str": "raw string"
        }|]

exampleFooInvalidJSON :: Value
exampleFooInvalidJSON =
  [aesonQQ| { "a": {"thing": "a-thing", "other": 42},
              "b": {"b_thing": 99}} |]

instance ToSchema Foo where
  schema =
    (doc . description ?~ "A Foo object")
      . object "Foo"
      $ Foo
        <$> fooA .= field "a" schema
        <* (thing . fooA) .= optional (field "a_thing" (unnamed schema))
        <*> fooB .= field "b" schema
        <*> fooStr .= field "str" (unnamed schema)

data Bar = BarA A | BarB B
  deriving (Eq, Show)
  deriving (ToJSON, FromJSON) via Schema Bar

_BarA :: Prism' Bar A
_BarA = prism' BarA $ \case
  BarA a -> Just a
  _ -> Nothing

_BarB :: Prism' Bar B
_BarB = prism' BarB $ \case
  BarB b -> Just b
  _ -> Nothing

instance ToSchema Bar where
  schema =
    named "Bar" $
      tag _BarA (unnamed schema)
        <> tag _BarB (unnamed schema)

exampleBarA :: Bar
exampleBarA = BarA (A "cthulhu" 711)

exampleBarAJSON :: Value
exampleBarAJSON = [aesonQQ| {"thing": "cthulhu", "other": 711} |]

exampleBarB :: Bar
exampleBarB = BarB (B 831)

exampleBarBJSON :: Value
exampleBarBJSON = [aesonQQ| {"b_thing": 831} |]

data Access = Public | Private | Link | Code
  deriving (Eq, Show)
  deriving (ToJSON, FromJSON) via Schema Access

instance ToSchema Access where
  schema =
    enum @Text "Access" $
      element "public" Public
        <> element "private" Private
        <> element "link" Link
        <> element "code" Code

-- optional fields

data User = User
  { userName :: Text,
    userHandle :: Maybe Text,
    userExpire :: Maybe Int
  }
  deriving (Eq, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema User

instance ToSchema User where
  schema =
    object "User" $
      User
        <$> userName .= field "name" schema
        <*> userHandle .= opt (field "handle" schema)
        <*> userExpire .= opt (field "expire" schema)

exampleUser1 :: User
exampleUser1 = User "Alice" (Just "alice") Nothing

exampleUser1JSON :: LByteString
exampleUser1JSON = "{\"handle\":\"alice\",\"name\":\"Alice\"}"

exampleUser2 :: User
exampleUser2 = User "Bob" Nothing (Just 100)

exampleUser2JSON :: LByteString
exampleUser2JSON = "{\"expire\":100,\"name\":\"Bob\"}"

-- bind schemas

data TaggedObject = TO
  { toTag :: Tag,
    toObj :: UntaggedObject
  }
  deriving (Eq, Show)
  deriving (FromJSON, ToJSON, S.ToSchema) via Schema TaggedObject

data UntaggedObject = Obj1 String | Obj2 Int
  deriving (Eq, Show)

data Tag = Tag1 | Tag2
  deriving (Eq, Show, Enum, Bounded)

_Obj1 :: Prism' UntaggedObject String
_Obj1 = prism' Obj1 $ \case
  Obj1 a -> Just a
  _ -> Nothing

_Obj2 :: Prism' UntaggedObject Int
_Obj2 = prism' Obj2 $ \case
  Obj2 b -> Just b
  _ -> Nothing

instance ToSchema Tag where
  schema = enum @Text "Tag" (element "tag1" Tag1 <> element "tag2" Tag2)

instance ToSchema TaggedObject where
  schema =
    object "TaggedObject" $
      uncurry TO <$> (toTag &&& toObj)
        .= bind
          (fst .= field "tag" schema)
          (snd .= fieldOver _1 "obj" (objectOver _1 "UntaggedObject" untaggedSchema))
    where
      untaggedSchema = dispatch $ \case
        Tag1 -> tag _Obj1 (field "tag1_data" schema)
        Tag2 -> tag _Obj2 (field "tag2_data" schema)

exampleTaggedObject :: TaggedObject
exampleTaggedObject = TO Tag1 (Obj1 "foo")

exampleTaggedObjectJSON :: Value
exampleTaggedObjectJSON = [aesonQQ| {"tag": "tag1", "obj": { "tag1_data": "foo" } } |]

exampleTaggedObject2 :: TaggedObject
exampleTaggedObject2 = TO Tag2 (Obj2 44)

exampleTaggedObject2JSON :: Value
exampleTaggedObject2JSON = [aesonQQ| {"tag": "tag2", "obj": { "tag2_data": 44 } } |]

exampleTaggedObject3JSON :: Value
exampleTaggedObject3JSON = [aesonQQ| {"tag": "tag1", "obj": { "tag2_data": 44 } } |]

-- non empty

newtype NonEmptyTest = NonEmptyTest {nl :: NonEmpty Text}
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema NonEmptyTest

instance ToSchema NonEmptyTest where
  schema = object "NonEmptyTest" $ NonEmptyTest <$> nl .= field "nl" (nonEmptyArray schema)

-- references

newtype Named = Named {getName :: Text}

instance ToSchema Named where
  schema = Named <$> getName .= object "Named" (field "name" (text "Name"))

instance S.ToSchema Named where
  declareNamedSchema = schemaToSwagger

--- optional fields

data RmClient = RmClient {rmPassword :: Maybe Text}
  deriving (Eq, Show)
  deriving (FromJSON) via Schema RmClient

passwordSchema :: ValueSchema NamedSwaggerDoc Text
passwordSchema = schema `withParser` validate
  where
    validate :: Text -> A.Parser Text
    validate x =
      if Text.length x < 6
        then fail "password too short"
        else pure x

-- this is "wrong", because it succeeds even if password validation fails
rmClientSchema :: ValueSchema NamedSwaggerDoc RmClient
rmClientSchema =
  object "RmClient" $
    RmClient
      <$> rmPassword .= lax (field "password" (optWithDefault Null passwordSchema))

instance ToSchema RmClient where
  schema =
    object "RmClient" $
      RmClient
        <$> rmPassword .= optField "password" Nothing passwordSchema

-- examples from documentation (only type-checked)

data Detail
  = Name Text
  | Age Int

makePrisms ''Detail

data DetailTag = NameTag | AgeTag
  deriving (Eq, Enum, Bounded)

tagSchema :: ValueSchema NamedSwaggerDoc DetailTag
tagSchema =
  enum @Text "Detail Tag" $
    mconcat [element "name" NameTag, element "age" AgeTag]

detailSchema :: ValueSchema NamedSwaggerDoc Detail
detailSchema =
  object "Detail" $
    fromTagged <$> toTagged
      .= bind
        (fst .= field "tag" tagSchema)
        (snd .= fieldOver _1 "value" untaggedSchema)
  where
    toTagged :: Detail -> (DetailTag, Detail)
    toTagged d@(Name _) = (NameTag, d)
    toTagged d@(Age _) = (AgeTag, d)

    fromTagged :: (DetailTag, Detail) -> Detail
    fromTagged = snd

    untaggedSchema = dispatch $ \case
      NameTag -> tag _Name (unnamed schema)
      AgeTag -> tag _Age (unnamed schema)

userSchemaWithDefaultName' :: ValueSchema NamedSwaggerDoc User
userSchemaWithDefaultName' =
  object "User" $
    User
      <$> (getOptText . userName) .= (fromMaybe "" <$> opt (field "name" schema))
      <*> userHandle .= opt (field "handle" schema)
      <*> userExpire .= opt (field "expire" schema)
  where
    getOptText :: Text -> Maybe Text
    getOptText "" = Nothing
    getOptText t = Just t

userSchemaWithDefaultName :: ValueSchema NamedSwaggerDoc User
userSchemaWithDefaultName =
  object "User" $
    User
      <$> userName .= (field "name" schema <|> pure "")
      <*> userHandle .= opt (field "handle" schema)
      <*> userExpire .= opt (field "expire" schema)
