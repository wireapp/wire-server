module FindTests.ParseSpec where

import Control.Lens ((^?))
import Data.Aeson
import Data.Aeson.Diff qualified as AD
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Lens
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map qualified as Map
import FindTests.Load
import FindTests.Parse
import GHC
import GHC.Driver.Ppr
import GHC.Driver.Session
import GHC.Paths (libdir)
import GHC.Types.Name.Occurrence
import GHC.Utils.Logger
import GHC.Utils.Outputable
import Imports
import Imports as I
import System.FilePath
import Test.HUnit (assertFailure)
import Test.Hspec

readTestSourceFile :: FilePath -> IO (Value, DynFlags, ParsedSource)
readTestSourceFile filename = do
  wireServerRoot <- lookupEnv "WIRE_SERVER_ROOT" >>= maybe (error "*** $WIRE_SERVER_ROOT is not defined") pure
  let testDataRoot = wireServerRoot </> "tools/find-tests/test-data"
      testDataHs = wireServerRoot </> "tools/find-tests/test-data" </> filename
      testDataJson = wireServerRoot </> "tools/find-tests/test-data" </> (takeBaseName filename <.> "json")
  expected <- either (error . show . ((testDataJson I.<> ": ") I.<>)) pure =<< (eitherDecode . BS.pack <$> readFile testDataJson)
  runApp $ \dflags -> (expected,dflags,) <$> loadHsModule testDataHs

tcase :: HasCallStack => FilePath -> Spec
tcase filename =
  before (readTestSourceFile filename) $
    it ("file: " I.<> filename) $ \(expected, dflags, parsed) -> do
      toJSON (parseTestCases dflags filename parsed) `jsonShouldBe` expected

jsonShouldBe :: HasCallStack => Value -> Value -> Expectation
jsonShouldBe actual expected = unless (actual == expected) $ assertFailure (BS.unpack msg)
  where
    -- (see also: `shouldMatchWithMsg` in package `integration` in this repo.)
    msg = "Actual:\n" I.<> pa I.<> "\nExpected:\n" I.<> pe I.<> "\nDiff:\n" I.<> diff
    pa = encodePretty actual
    pe = encodePretty expected
    diff =
      -- show diff, but only in the interesting cases.
      if (isJust (actual ^? _Object) && isJust (expected ^? _Object))
        || (isJust (actual ^? _Array) && isJust (expected ^? _Array))
        then encodePretty (AD.diff actual expected)
        else ""

spec :: Spec
spec = do
  describe "parseTestCases" $ do
    tcase "HelloWorld.hs" -- FUTUREWORK: collect all test cases with getDirectory or something.
