module API.Brig where

import API.Common
import Data.Function
import Data.Maybe
import GHC.Stack
import Testlib.Prelude

data AddClient = AddClient
  { ctype :: String,
    internal :: Bool,
    clabel :: String,
    model :: String,
    prekeys :: Maybe [Value],
    lastPrekey :: Maybe Value,
    password :: String
  }

instance Default AddClient where
  def =
    AddClient
      { ctype = "permanent",
        internal = False,
        clabel = "Test Device",
        model = "Test Model",
        prekeys = Nothing,
        lastPrekey = Nothing,
        password = defPassword
      }

addClient ::
  (HasCallStack, MakesValue user) =>
  user ->
  AddClient ->
  App Response
addClient user args = do
  uid <- objId user
  req <- baseRequest user Brig Unversioned $ "/i/clients/" <> uid
  pks <- maybe (fmap pure getPrekey) pure args.prekeys
  lpk <- maybe getLastPrekey pure args.lastPrekey
  submit "POST" $
    req
      & addJSONObject
        [ "prekeys" .= pks,
          "lastkey" .= lpk,
          "type" .= args.ctype,
          "label" .= args.clabel,
          "model" .= args.model,
          "password" .= args.password
        ]

deleteClient ::
  (HasCallStack, MakesValue user, MakesValue client) =>
  user ->
  Maybe String ->
  client ->
  App Response
deleteClient user mconn client = do
  let conn = fromMaybe "0" mconn
  uid <- objId user
  cid <- objId client
  req <- baseRequest user Brig Versioned $ "/clients/" <> cid
  submit "DELETE" $
    req
      & zUser uid
      & zConnection conn
      & addJSONObject
        [ "password" .= defPassword
        ]

searchContacts ::
  ( MakesValue searchingUserId,
    MakesValue searchTerm
  ) =>
  searchingUserId ->
  searchTerm ->
  App Response
searchContacts searchingUserId searchTerm = do
  req <- baseRequest searchingUserId Brig Versioned "/search/contacts"
  q <- asString searchTerm
  uid <- objId searchingUserId
  submit
    "GET"
    ( req
        & addQueryParams [("q", q)]
        & zUser uid
    )

getAPIVersion :: (HasCallStack, MakesValue domain) => domain -> App Response
getAPIVersion domain = do
  req <- baseRequest domain Brig Unversioned $ "/api-version"
  submit "GET" req

postConnection ::
  ( HasCallStack,
    MakesValue userFrom,
    MakesValue userTo
  ) =>
  userFrom ->
  userTo ->
  App Response
postConnection userFrom userTo = do
  uidFrom <- objId userFrom
  (userToDomain, userToId) <- objQid userTo
  req <-
    baseRequest userFrom Brig Versioned $
      joinHttpPath ["/connections", userToDomain, userToId]
  submit
    "POST"
    ( req
        & zUser uidFrom
        & zConnection "conn"
    )

putConnection ::
  ( HasCallStack,
    MakesValue userFrom,
    MakesValue userTo,
    MakesValue status
  ) =>
  userFrom ->
  userTo ->
  status ->
  App Response
putConnection userFrom userTo status = do
  uidFrom <- objId userFrom
  (userToDomain, userToId) <- objQid userTo
  req <-
    baseRequest userFrom Brig Versioned $
      joinHttpPath ["/connections", userToDomain, userToId]
  statusS <- asString status
  submit
    "POST"
    ( req
        & zUser uidFrom
        & zConnection "conn"
        & contentTypeJSON
        & addJSONObject ["status" .= statusS]
    )
