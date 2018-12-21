{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Push where

import Imports
import Data.String.Conversions (cs)
import Gundeck.Push (pushAll, pushAny)
import Gundeck.Push.Websocket as Web (bulkPush)
import Gundeck.Types
import MockGundeck
import System.FilePath ((</>))
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS


tests :: TestTree
tests = testGroup "bulkpush" $
    (test "pushAllProp" pushAllProp <$> [1..11]) <>
    (test "webBulkPushProp" webBulkPushProp <$> [1..2]) <>
    [ testProperty "web sockets" webBulkPushProps
    , testProperty "native pushes" pushAllProps
    ]


mkEnv :: (Pretty MockEnv -> Property) -> Positive Int -> Property
mkEnv prop (Positive len) = forAllShrink (Pretty <$> resize len genMockEnv) (shrinkPretty shrinkMockEnv) prop


testRootPath :: FilePath
testRootPath = "test/mock-samples"

test
  :: forall input. Aeson.FromJSON input
  => String -> (MockEnv -> Pretty input -> Property) -> Int -> TestTree
test testname runtest i = testCase (testname <> "-" <> show i) $ do
  let fulltestname = testRootPath </> testname <> "-" <> show i <> ".json"
  Just ((env, input) :: (MockEnv, input))
    <- either (error . show) pure =<< (Aeson.eitherDecode <$> LBS.readFile fulltestname)
  runProp $ runtest env (Pretty input)

runProp :: Property -> Assertion
runProp propty = quickCheckWithResult stdArgs { maxSuccess = 1, chatty = False } propty >>= \case
  Success {} -> pure ()
  bad@(Failure {}) -> assertBool (intercalate "\n" (failingTestCase bad)) False
  bad -> assertBool (output bad) False


webBulkPushProps :: Positive Int -> Property
webBulkPushProps plen@(Positive len) = mkEnv mkNotifs plen
  where
    mkNotifs :: Pretty MockEnv -> Property
    mkNotifs (Pretty env) = forAllShrink
      (Pretty <$> resize len (genNotifs env))
      (shrinkPretty shrinkNotifs)
      (webBulkPushProp env)

webBulkPushProp :: MockEnv -> Pretty [(Notification, [Presence])] -> Property
webBulkPushProp env (Pretty notifs) = counterexample (cs $ Aeson.encode (env, notifs))
                                    $ foldl' (.&&.) (once True) props
      where
        (realout, realst) = runMockGundeck env $ Web.bulkPush notifs
        (mockout, mockst) = runMockGundeck env $ mockBulkPush notifs
        props = [ realst === mockst
                , sort realout === sort mockout
                ]


pushAllProps :: Positive Int -> Property
pushAllProps plen@(Positive len) = mkEnv mkPushes plen
  where
    mkPushes :: Pretty MockEnv -> Property
    mkPushes (Pretty env) = forAllShrink
      (Pretty <$> resize len (genPushes env))
      (shrinkPretty shrinkPushes)
      (pushAllProp env)

pushAllProp :: MockEnv -> Pretty [Push] -> Property
pushAllProp env (Pretty pushes) = counterexample (cs $ Aeson.encode (env, pushes))
                                $ foldl' (.&&.) (once True) props
  where
    ((), realst) = runMockGundeck env (pushAll pushes)
    ((), mockst) = runMockGundeck env (mockPushAll pushes)
    (errs, oldst) = runMockGundeck env (pushAny pushes)
    props = [ (Aeson.eitherDecode . Aeson.encode) pushes === Right pushes
            , (Aeson.eitherDecode . Aeson.encode) env === Right env
            , counterexample "real vs. old"  $ realst === oldst
            , counterexample "old errors"    $ isRight errs === True
            , counterexample "real vs. mock" $ realst === mockst
            ]
