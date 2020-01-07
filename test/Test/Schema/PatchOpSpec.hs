{-# LANGUAGE QuasiQuotes #-}
module Test.Schema.PatchOpSpec where
import Data.Foldable (for_)
import Test.Hspec (Spec, describe, xit, it, shouldSatisfy, shouldBe)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Web.Scim.Test.Util (scim)
import Web.Scim.Schema.PatchOp
import Data.Aeson.Types (toJSON, fromJSON, Result(Success, Error), Value(String))
import Data.Attoparsec.ByteString (parseOnly)
import HaskellWorks.Hspec.Hedgehog (require)
import Hedgehog (Gen, tripping, forAll, Property, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.FilterSpec (genValuePath, genAttrPath, genSubAttr)

isSuccess :: Result a -> Bool
isSuccess (Success _) = True
isSuccess (Error _) = False

genPatchOp :: Gen Value -> Gen PatchOp
genPatchOp genValue = PatchOp <$> Gen.list (Range.constant 0 20) (genOperation genValue)

genOperation :: Gen Value -> Gen Operation
genOperation genValue = Operation <$> Gen.enumBounded <*> Gen.maybe genPath <*> Gen.maybe genValue

genPath :: Gen Path
genPath = Gen.choice
  [ IntoValuePath <$> genValuePath <*> Gen.maybe genSubAttr
  , NormalPath <$> genAttrPath
  ]

prop_roundtrip :: Property
prop_roundtrip = property $ do
  x <- forAll genPath
  tripping x (encodeUtf8 . rPath) (parseOnly pPath)

prop_roundtrip_PatchOp :: Property
prop_roundtrip_PatchOp = property $ do
  -- Just some strings for now. However, should be constrained to what the
  -- PatchOp is operating on in the future... We need better typed PatchOp for
  -- this. TODO(arianvp)
  x <- forAll (genPatchOp (String <$> Gen.text (Range.constant 0 20) Gen.unicode))
  tripping x toJSON fromJSON
  
spec :: Spec
spec = do
  describe "urn:ietf:params:scim:api:messages:2.0:PatchOp" $ do
    describe "The body of each request MUST contain the \"schemas\" attribute with the URI value of \"urn:ietf:params:scim:api:messages:2.0:PatchOp\"." $ do
      it "rejects an empty schemas list" $ do
        fromJSON @PatchOp [scim| { 
          "schemas": [],
          "operations": []
        }|] `shouldSatisfy`  (not . isSuccess)
    --TODO(arianvp): We don't support arbitrary path names (yet)
    it "roundtrips Path" $ require prop_roundtrip
    it "roundtrips PatchOp" $ require prop_roundtrip_PatchOp
    it "rejects invalid operations" $ do
      fromJSON @PatchOp [scim| {
          "schemas": ["urn:ietf:params:scim:api:messages:2.0:PatchOp"],
          "operations": [{"op":"unknown"}]
      }|] `shouldSatisfy` (not . isSuccess)

    -- TODO(arianvp): Only makes sense if we have a Patch type _specifically_ for User
    xit "rejects unknown paths" $ do
      fromJSON @Path "unknown.field" `shouldSatisfy` (not . isSuccess)
    it "rejects invalid paths" $ do
      fromJSON @Path "unknown]field" `shouldSatisfy` (not . isSuccess)
    describe "Examples from https://tools.ietf.org/html/rfc7644#section-3.5.2 Figure 8" $ do
      let
        examples =
          [ "members"
          , "name.familyname"
          , "addresses[type eq \"work\"]"
          , "members[value eq \"2819c223-7f76-453a-919d-413861904646\"]"
          , "members[value eq \"2819c223-7f76-453a-919d-413861904646\"].displayname"
          ]
      for_ examples $ \p -> it ("parses " ++ show p) $ (rPath <$> parseOnly pPath p) `shouldBe` Right (decodeUtf8 p)

  
