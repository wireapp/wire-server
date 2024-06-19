module API.GundeckInternal where

import Testlib.Prelude

postPush ::
  ( HasCallStack,
    MakesValue user,
    MakesValue a
  ) =>
  user ->
  [a] ->
  App Response
postPush user payloads = do
  req <- baseRequest user Gundeck Unversioned "/i/push/v2"
  body <- traverse make payloads
  submit "POST" $ req & addJSON body

getPresence ::
  (HasCallStack, MakesValue user) =>
  user ->
  App Response
getPresence u = do
  uid <- u %. "id" & asString
  req <-
    baseRequest u Gundeck Unversioned
      $ "/i/presences/"
      <> uid
  submit "GET" req

unregisterUser ::
  (HasCallStack, MakesValue user) =>
  user ->
  App Response
unregisterUser u = do
  req <- baseRequest u Gundeck Unversioned "/i/user/"
  submit "DELETE" req

getPushTokens ::
  (HasCallStack, MakesValue user) =>
  user ->
  App Response
getPushTokens u = do
  uid <- u %. "id" & asString
  req <- baseRequest u Gundeck Unversioned ("/i/push-tokens/" <> uid)
  submit "GET" req
