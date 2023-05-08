module API.Brig where

import API.Common
import qualified Data.ByteString.Base64 as Base64
import Data.Foldable
import Data.Function
import Data.Maybe
import qualified Data.Text.Encoding as T
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

data UpdateClient = UpdateClient
  { prekeys :: [Value],
    lastPrekey :: Maybe Value,
    label :: Maybe String,
    capabilities :: Maybe [Value],
    mlsPublicKeys :: Maybe Value
  }

instance Default UpdateClient where
  def =
    UpdateClient
      { prekeys = [],
        lastPrekey = Nothing,
        label = Nothing,
        capabilities = Nothing,
        mlsPublicKeys = Nothing
      }

updateClient ::
  HasCallStack =>
  ClientIdentity ->
  UpdateClient ->
  App Response
updateClient cid args = do
  uid <- objId cid
  req <- baseRequest cid Brig Versioned $ "/clients/" <> cid.client
  submit "PUT" . zUser uid $
    addJSONObject
      ( ["prekeys" .= args.prekeys]
          <> ["lastkey" .= k | k <- toList args.lastPrekey]
          <> ["label" .= l | l <- toList args.label]
          <> ["capabilities" .= c | c <- toList args.capabilities]
          <> ["mls_public_keys" .= k | k <- toList args.mlsPublicKeys]
      )
      req

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

uploadKeyPackage :: ClientIdentity -> ByteString -> App Response
uploadKeyPackage cid kp = do
  req <-
    baseRequest cid Brig Versioned $
      "/mls/key-packages/self/" <> cid.client
  uid <- objId cid
  submit
    "POST"
    ( req
        & zUser uid
        & addJSONObject ["key_packages" .= [T.decodeUtf8 (Base64.encode kp)]]
    )

claimKeyPackages :: (MakesValue u, MakesValue v) => u -> v -> App Response
claimKeyPackages u v = do
  (targetDom, targetUid) <- objQid v
  req <-
    baseRequest u Brig Versioned $
      "/mls/key-packages/claim/" <> targetDom <> "/" <> targetUid
  uid <- objId u
  submit "POST" (req & zUser uid)
