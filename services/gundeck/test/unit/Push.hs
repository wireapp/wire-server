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
import Control.Lens
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
    ((\n -> testCase (show n) $ test pushAllProp n) <$> ([1..9] <> [12..13])) <>
    ((\n -> testCase (show n) $ test webBulkPushProp n) <$> [10..11]) <>
    [ testProperty "web sockets" webBulkPushProps
    , testProperty "native pushes" pushAllProps
    ]


mkEnv :: (Pretty MockEnv -> Property) -> Positive Int -> Property
mkEnv prop (Positive len) = forAllShrink (Pretty <$> resize len genMockEnv) (shrinkPretty shrinkMockEnv) prop


testRootPath :: FilePath
testRootPath = "test/mock-samples"

test
  :: forall input. Aeson.FromJSON input
  => (MockEnv -> Pretty input -> Property) -> Int -> Assertion
test runtest i = do
  Just ((env, input) :: (MockEnv, input))
    <- Aeson.decode <$> LBS.readFile (testRootPath </> show i <> ".json")
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
      (Pretty <$> resize len (genNotifs (env ^. meRecipients)))
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
      (Pretty <$> resize len (genPushes (env ^. meRecipients)))
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
            , realst === oldst
            , isRight errs === True
            , realst === mockst
            ]
