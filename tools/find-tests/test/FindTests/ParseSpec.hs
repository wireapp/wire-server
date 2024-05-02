module FindTests.ParseSpec where

import Data.Aeson
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
import Test.Hspec

readTestSourceFile :: FilePath -> IO ParsedSource
readTestSourceFile filename = do
  wireServerRoot <- lookupEnv "WIRE_SERVER_ROOT" >>= maybe (error "*** $WIRE_SERVER_ROOT is not defined") pure
  runApp $ \_dflags -> loadHsModule (wireServerRoot </> "tools/find-tests/test-data" </> filename)

tcase :: FilePath -> (DynFlags -> ParsedSource -> Expectation) -> Spec
tcase filename action =
  before ((,) <$> (runApp pure) <*> (readTestSourceFile filename)) $
    it ("file: " I.<> filename) (uncurry action)

spec :: Spec
spec = do
  describe "parseTestCases" $ do
    tcase "HelloWorld.hs" $ \dflags hsmod -> parseTestCases dflags hsmod `shouldBe` mempty
