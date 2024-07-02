{-# OPTIONS -Wno-ambiguous-fields #-}
module Test.Presence where

import API.Common
import API.Gundeck
import API.GundeckInternal
import SetupHelpers
import Testlib.Prelude

ensurePresent :: (HasCallStack, MakesValue u) => u -> Int -> App ()
ensurePresent u n = retryT $ do
  ps <- getPresence u >>= getJSON 200 >>= asList
  length ps `shouldMatchInt` n

registerUser :: (HasCallStack) => App (Value, String)
registerUser = do
  alice <- randomUserId OwnDomain
  c <- randomClientId
  withWebSocket (alice, "conn", c) $ \_ ->
    ensurePresent alice 1
  pure (alice, c)

testAddUser :: (HasCallStack) => App ()
testAddUser = void registerUser

testRemoveUser :: (HasCallStack) => App ()
testRemoveUser = do
  -- register alice and add a push token
  (alice, c) <- registerUser
  void $ generateAndPostPushToken alice c def >>= getJSON 201
  do
    t <- getPushTokens alice >>= getJSON 200
    tokens <- t %. "tokens" & asList
    length tokens `shouldMatchInt` 1

  -- push something to alice
  do
    r <- recipient alice
    let push =
          object
            [ "recipients" .= [r],
              "payload" .= [object ["foo" .= "bar"]]
            ]
    void $ postPush alice [push] >>= getBody 200

  -- unregister alice
  void $ unregisterUser alice >>= getBody 200

  -- check that the token is deleted
  do
    t <- getPushTokens alice >>= getJSON 200
    t %. "tokens" `shouldMatch` ([] :: [Value])

  -- check that notifications are deleted
  do
    ns <- getNotifications alice def {client = Just c} >>= getJSON 200
    ns %. "notifications" `shouldMatch` ([] :: [Value])
    ns %. "has_more" `shouldMatch` False
