
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
import Control.Lens (Prism', at, prism', (?~), (^.))
import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), Value, decode, encode, fromJSON)
import Data.Aeson.QQ
import qualified Data.HashMap.Strict.InsOrd as InsOrdHashMap
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Proxy
import Data.Schema
import qualified Data.Swagger as S
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
      testNonEmptyParseFailure,
      testNonEmptyParseSuccess,
      testNonEmptyToJSON,
      testNonEmptySchema
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
  deriving (ToJSON, FromJSON) via Schema User

instance ToSchema User where
  schema =
    object "User" $
      User
        <$> userName .= field "name" (unnamed schema)
        <*> userHandle .= opt (field "handle" (unnamed schema))
        <*> userExpire .= opt (field "expire" (unnamed schema))

exampleUser1 :: User
exampleUser1 = User "Alice" (Just "alice") Nothing

exampleUser1JSON :: LByteString
exampleUser1JSON = "{\"handle\":\"alice\",\"name\":\"Alice\"}"

exampleUser2 :: User
exampleUser2 = User "Bob" Nothing (Just 100)

exampleUser2JSON :: LByteString
exampleUser2JSON = "{\"expire\":100,\"name\":\"Bob\"}"

newtype NonEmptyTest = NonEmptyTest {nl :: NonEmpty Text}
  deriving stock (Eq, Show)
  deriving (ToJSON, FromJSON, S.ToSchema) via Schema NonEmptyTest

instance ToSchema NonEmptyTest where
  schema = object "NonEmptyTest" $ NonEmptyTest <$> nl .= field "nl" (nonEmptyArray schema)
