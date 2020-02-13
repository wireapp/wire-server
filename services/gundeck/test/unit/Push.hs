{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Push where

import qualified Data.Aeson as Aeson
import Gundeck.Push (pushAll, pushAny)
import Gundeck.Push.Websocket as Web (bulkPush)
import Gundeck.Types
import Imports
import MockGundeck
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
  testGroup "bulkpush" $
    [ testProperty "web sockets" webBulkPushProps,
      testProperty "native pushes" pushAllProps
    ]

mkEnv :: (Pretty MockEnv -> Property) -> Positive Int -> Property
mkEnv prop (Positive len) = forAllShrink (Pretty <$> resize len genMockEnv) (shrinkPretty shrinkMockEnv) prop

webBulkPushProps :: Positive Int -> Property
webBulkPushProps plen@(Positive len) = mkEnv mkNotifs plen
  where
    mkNotifs :: Pretty MockEnv -> Property
    mkNotifs (Pretty env) =
      forAllShrink
        (Pretty <$> resize len (genNotifs env))
        (shrinkPretty shrinkNotifs)
        (webBulkPushProp env)

webBulkPushProp :: MockEnv -> Pretty [(Notification, [Presence])] -> Property
webBulkPushProp env (Pretty notifs) =
  counterexample "^ environment, notifications\n" $
    conjoin props
  where
    (realout, realst) = runMockGundeck env $ Web.bulkPush notifs
    (mockout, mockst) = runMockGundeck env $ mockBulkPush notifs
    props =
      [ realst === mockst,
        sort realout === sort mockout
      ]

pushAllProps :: Positive Int -> Property
pushAllProps plen@(Positive len) = mkEnv mkPushes plen
  where
    mkPushes :: Pretty MockEnv -> Property
    mkPushes (Pretty env) =
      forAllShrink
        (Pretty <$> resize len (genPushes env))
        (shrinkPretty shrinkPushes)
        (pushAllProp env)

pushAllProp :: MockEnv -> Pretty [Push] -> Property
pushAllProp env (Pretty pushes) =
  counterexample "^ environment, pushes\n" $
    conjoin props
  where
    ((), realst) = runMockGundeck env (pushAll pushes)
    ((), mockst) = runMockGundeck env (mockPushAll pushes)
    (errs, oldst) = runMockGundeck env (pushAny pushes)
    props =
      [ (Aeson.eitherDecode . Aeson.encode) pushes === Right pushes,
        (Aeson.eitherDecode . Aeson.encode) env === Right env,
        counterexample "real vs. mock:" $ realst === mockst,
        counterexample "real vs. old:" $ realst === oldst,
        counterexample "old errors:" $ isRight errs === True
      ]
