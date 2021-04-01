module Test.Wire.API.User.Search where

import Data.Aeson (encode, toJSON)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import Data.String.Conversions (cs)
import Imports
import qualified Test.Tasty as T
import Test.Tasty.QuickCheck (counterexample, testProperty)
import Wire.API.User.Search (Contact)

tests :: T.TestTree
tests = T.testGroup "Search" [searchResultBackwardsCompatibility]

searchResultBackwardsCompatibility :: T.TestTree
searchResultBackwardsCompatibility =
  testProperty
    "Contact always contains id"
    $ \(c :: Contact) ->
      let prop =
            case toJSON c of
              Aeson.Object o -> "id" `elem` HashMap.keys o
              _ -> False
       in counterexample ("This json doesn't contain 'id': \n" <> cs (encode c)) prop
