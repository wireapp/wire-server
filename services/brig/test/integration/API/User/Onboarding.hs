module API.User.Onboarding (tests) where

import API.User.Util
import Bilge hiding (accept, timeout)
import qualified Brig.Options as Opt
import Brig.Types
import Brig.Types.Intra
import Imports
import Test.Tasty hiding (Timeout)
import Util

tests :: ConnectionLimit -> Opt.Timeout -> Opt.Opts -> Manager -> Brig -> Cannon -> Galley -> TestTree
tests _cl _at _conf p b _c _g =
  testGroup
    "onboarding"
    [ test p "post /onboarding/v3 - 200" $ testOnboarding b
    ]

testOnboarding :: Brig -> Http ()
testOnboarding brig = do
  usr1 <- randomUser brig
  let uid1 = userId usr1
      em1 = fromEmail $ fromMaybe (error "Should have an email!") (userEmail usr1)
  (uid2, phn2) <- createRandomPhoneUser brig
  -- We do not match on emails (nor on other phone numbers obviously)
  ab2 <- liftIO $ toAddressBook [("random1", [em1]), ("random2", ["+0123456789"])]
  let expect2 = toMatchingResult []
  uploadAddressBook brig uid1 ab2 expect2
  -- Simple test with a single user, single entry
  ab3 <- liftIO $ toAddressBook [("random", [fromPhone phn2])]
  let expect3 = toMatchingResult [(uid2, "random")]
  uploadAddressBook brig uid1 ab3 expect3
  -- Ensure we really got auto-connected
  assertConnections brig uid1 [ConnectionStatus uid1 uid2 Accepted]
  assertConnections brig uid2 [ConnectionStatus uid2 uid1 Accepted]
  -- Ensure we only auto-connect once
  uploadAddressBook brig uid1 ab3 (toMatchingResult [])
  -- Single user, multiple entries
  (uid4, ph4) <- createRandomPhoneUser brig
  ab4 <-
    liftIO $
      toAddressBook
        [ ("first", [fromPhone ph4]),
          ("second", [fromPhone ph4])
        ]
  let expect4 = toMatchingResult [(uid4, "first"), (uid4, "second")]
  uploadAddressBook brig uid1 ab4 expect4
  -- Multiple user, multiple entries
  (uid5, ph5) <- createRandomPhoneUser brig
  (uid6, ph6) <- createRandomPhoneUser brig
  ab5 <-
    liftIO $
      toAddressBook
        [ ("first", [fromPhone ph5]),
          ("second", [fromPhone ph5]),
          ("third", [fromPhone ph6]),
          ("fourth", [fromPhone ph6])
        ]
  let expect5 =
        toMatchingResult
          [ (uid5, "first"),
            (uid5, "second"),
            (uid6, "third"),
            (uid6, "fourth")
          ]
  -- Check upload and results
  uploadAddressBook brig uid1 ab5 expect5
