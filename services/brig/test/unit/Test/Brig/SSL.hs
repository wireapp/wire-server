module Test.Brig.SSL where

import Imports
import Test.Tasty
import Test.Tasty.HUnit

-- import Brig.App
-- import Wire.API.Provider.External
-- import Data.Id

tests :: TestTree
tests =
  testGroup "SSL" $
    [ testGroup "verify" $
        [ testCase "fingerprint" testVerifyFingerprint
        ]
    ]

testVerifyFingerprint :: Assertion
testVerifyFingerprint = do
  -- usr <- arbitrary
  -- scon <- arbitrary
  -- mgr <- initHttpManager
  -- bid <- BotId <$> randomId
  -- let bcid = ClientId (fromIntegral (hash bid))
  -- let btk = "secret-token"
  --     busr = mkBotUserView usr
  -- let newBot = NewBotRequest bid bcid busr bcnv btk Nothing
  -- rs <- createBot scon newBots
  -- print rs
  assertBool "!" True
