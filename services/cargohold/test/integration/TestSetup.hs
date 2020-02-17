module TestSetup
  ( test,
    tsManager,
    tsCargohold,
    TestSignature,
    TestSetup (..),
    CargoHold,
  )
where

import Bilge (Request)
import Bilge.IO (Http, Manager, runHttpT)
import Control.Lens ((^.), makeLenses)
import Imports
import Test.Tasty
import Test.Tasty.HUnit

type CargoHold = Request -> Request

type TestSignature a = CargoHold -> Http a

data TestSetup
  = TestSetup
      { _tsManager :: Manager,
        _tsCargohold :: CargoHold
      }

makeLenses ''TestSetup

test :: IO TestSetup -> TestName -> TestSignature a -> TestTree
test s n h = testCase n runTest
  where
    runTest = do
      setup <- s
      (void $ runHttpT (setup ^. tsManager) (h (setup ^. tsCargohold)))
