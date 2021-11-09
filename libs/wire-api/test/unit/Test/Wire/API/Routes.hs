module Test.Wire.API.Routes where

import Data.Metrics.Servant
import Data.Tree
import Imports
import Servant.API
import qualified Test.Tasty as T
import Test.Tasty.HUnit
import Wire.API.Routes.QualifiedCapture

tests :: T.TestTree
tests =
  T.testGroup "Routes" $
    [T.testGroup "QualifiedCapture" [testCase "must expose the captures in metrics" qualifiedCaptureMetrics]]

type QualifiedCaptureAPI = "users" :> QualifiedCapture' '[] "uid" Int :> Get '[] Int

qualifiedCaptureMetrics :: Assertion
qualifiedCaptureMetrics =
  assertEqual
    "match metrics path"
    [Node (Right "users") [Node (Left ":uid_domain") [Node (Left ":uid") []]]]
    (getRoutes @QualifiedCaptureAPI)
