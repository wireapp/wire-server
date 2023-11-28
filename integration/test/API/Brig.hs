module API.Brig where

import API.Common
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as Base64
import Data.Foldable
import Data.Function
import qualified Data.Text.Encoding as T
import GHC.Stack
import Testlib.Prelude

data AddUser = AddUser
  { name :: Maybe String,
    email :: Maybe String,
    teamCode :: Maybe String,
    password :: Maybe String
  }

instance Default AddUser where
  def = AddUser Nothing Nothing Nothing Nothing

addUser :: (HasCallStack, MakesValue dom) => dom -> AddUser -> App Response
addUser dom opts = do
  req <- baseRequest dom Brig Versioned "register"
  name <- maybe randomName pure opts.name
  submit "POST" $
    req
      & addJSONObject
        [ "name" .= name,
          "email" .= opts.email,
          "team_code" .= opts.teamCode,
          "password" .= fromMaybe defPassword opts.password
        ]

getUser ::
  (HasCallStack, MakesValue user, MakesValue target) =>
  user ->
  target ->
  App Response
getUser user target = do
  (domain, uid) <- objQid target
  req <-
    baseRequest user Brig Versioned $
      joinHttpPath ["users", domain, uid]
  submit "GET" req

getUserByHandle :: (HasCallStack, MakesValue user, MakesValue domain) => user -> domain -> String -> App Response
getUserByHandle user domain handle = do
  domainStr <- asString domain
  req <-
    baseRequest user Brig Versioned $
      joinHttpPath ["users", "by-handle", domainStr, handle]
  submit "GET" req

getClient ::
  (HasCallStack, MakesValue user, MakesValue client) =>
  user ->
  client ->
  App Response
getClient u cli = do
  c <- make cli & asString
  req <-
    baseRequest u Brig Versioned $
      joinHttpPath ["clients", c]
  submit "GET" req

deleteUser :: (HasCallStack, MakesValue user) => user -> App Response
deleteUser user = do
  req <- baseRequest user Brig Versioned "/self"
  submit "DELETE" $
    req & addJSONObject ["password" .= defPassword]

putHandle :: (HasCallStack, MakesValue user) => user -> String -> App Response
putHandle user handle = do
  req <- baseRequest user Brig Versioned "/self/handle"
  submit "PUT" $
    req & addJSONObject ["handle" .= handle]

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
  req <- baseRequest cid Brig Versioned $ "/clients/" <> cid.client
  submit "PUT" $
    req
      & addJSONObject
        ( ["prekeys" .= args.prekeys]
            <> ["lastkey" .= k | k <- toList args.lastPrekey]
            <> ["label" .= l | l <- toList args.label]
            <> ["capabilities" .= c | c <- toList args.capabilities]
            <> ["mls_public_keys" .= k | k <- toList args.mlsPublicKeys]
        )

deleteClient ::
  (HasCallStack, MakesValue user, MakesValue client) =>
  user ->
  client ->
  App Response
deleteClient user client = do
  cid <- objId client
  req <- baseRequest user Brig Versioned $ "/clients/" <> cid
  submit "DELETE" $
    req
      & addJSONObject
        [ "password" .= defPassword
        ]

getClientsQualified ::
  ( HasCallStack,
    MakesValue user,
    MakesValue domain,
    MakesValue otherUser
  ) =>
  user ->
  domain ->
  otherUser ->
  App Response
getClientsQualified user domain otherUser = do
  ouid <- objId otherUser
  d <- objDomain domain
  req <-
    baseRequest user Brig Versioned $
      "/users/"
        <> d
        <> "/"
        <> ouid
        <> "/clients"
  submit "GET" req

listUsersClients :: (HasCallStack, MakesValue user, MakesValue qualifiedUserIds) => user -> [qualifiedUserIds] -> App Response
listUsersClients usr qualifiedUserIds = do
  qUsers <- mapM objQidObject qualifiedUserIds
  req <- baseRequest usr Brig Versioned $ joinHttpPath ["users", "list-clients"]
  submit "POST" (req & addJSONObject ["qualified_users" .= qUsers])

searchContacts ::
  ( MakesValue user,
    MakesValue searchTerm,
    MakesValue domain
  ) =>
  user ->
  searchTerm ->
  domain ->
  App Response
searchContacts user searchTerm domain = do
  req <- baseRequest user Brig Versioned "/search/contacts"
  q <- asString searchTerm
  d <- objDomain domain
  submit "GET" (req & addQueryParams [("q", q), ("domain", d)])

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
  (userToDomain, userToId) <- objQid userTo
  req <-
    baseRequest userFrom Brig Versioned $
      joinHttpPath ["/connections", userToDomain, userToId]
  submit "POST" req

getConnection ::
  ( HasCallStack,
    MakesValue userFrom,
    MakesValue userTo
  ) =>
  userFrom ->
  userTo ->
  App Response
getConnection userFrom userTo = do
  (userToDomain, userToId) <- objQid userTo
  req <-
    baseRequest userFrom Brig Versioned $
      joinHttpPath ["/connections", userToDomain, userToId]
  submit "GET" req

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
  (userToDomain, userToId) <- objQid userTo
  req <-
    baseRequest userFrom Brig Versioned $
      joinHttpPath ["/connections", userToDomain, userToId]
  statusS <- asString status
  submit "PUT" (req & addJSONObject ["status" .= statusS])

getConnections :: (HasCallStack, MakesValue user) => user -> App Response
getConnections user = do
  req <- baseRequest user Brig Versioned "/list-connections"
  submit "POST" (req & addJSONObject ["size" .= Aeson.Number 500])

uploadKeyPackages :: ClientIdentity -> [ByteString] -> App Response
uploadKeyPackages cid kps = do
  req <-
    baseRequest cid Brig Versioned $
      "/mls/key-packages/self/" <> cid.client
  submit
    "POST"
    ( req
        & addJSONObject ["key_packages" .= map (T.decodeUtf8 . Base64.encode) kps]
    )

claimKeyPackages :: (MakesValue u, MakesValue v) => Ciphersuite -> u -> v -> App Response
claimKeyPackages suite u v = do
  (targetDom, targetUid) <- objQid v
  req <-
    baseRequest u Brig Versioned $
      "/mls/key-packages/claim/" <> targetDom <> "/" <> targetUid
  submit "POST" $
    req
      & addQueryParams [("ciphersuite", suite.code)]

countKeyPackages :: Ciphersuite -> ClientIdentity -> App Response
countKeyPackages suite cid = do
  req <- baseRequest cid Brig Versioned ("/mls/key-packages/self/" <> cid.client <> "/count")
  submit "GET" $
    req
      & addQueryParams [("ciphersuite", suite.code)]

deleteKeyPackages :: ClientIdentity -> [String] -> App Response
deleteKeyPackages cid kps = do
  req <- baseRequest cid Brig Versioned ("/mls/key-packages/self/" <> cid.client)
  submit "DELETE" $ req & addJSONObject ["key_packages" .= kps]

replaceKeyPackages :: ClientIdentity -> [Ciphersuite] -> [ByteString] -> App Response
replaceKeyPackages cid suites kps = do
  req <-
    baseRequest cid Brig Versioned $
      "/mls/key-packages/self/" <> cid.client
  submit "PUT" $
    req
      & addQueryParams [("ciphersuites", intercalate "," (map (.code) suites))]
      & addJSONObject ["key_packages" .= map (T.decodeUtf8 . Base64.encode) kps]

getSelf :: HasCallStack => String -> String -> App Response
getSelf domain uid = do
  let user = object ["domain" .= domain, "id" .= uid]
  req <- baseRequest user Brig Versioned "/self"
  submit "GET" req

getUserSupportedProtocols ::
  (HasCallStack, MakesValue user, MakesValue target) =>
  user ->
  target ->
  App Response
getUserSupportedProtocols user target = do
  (domain, uid) <- objQid target
  req <-
    baseRequest user Brig Versioned $
      joinHttpPath ["users", domain, uid, "supported-protocols"]
  submit "GET" req

putUserSupportedProtocols ::
  (HasCallStack, MakesValue user) =>
  user ->
  [String] ->
  App Response
putUserSupportedProtocols user ps = do
  req <-
    baseRequest user Brig Versioned $
      joinHttpPath ["self", "supported-protocols"]
  submit "PUT" (req & addJSONObject ["supported_protocols" .= ps])

data PostInvitation = PostInvitation
  { email :: Maybe String
  }

instance Default PostInvitation where
  def = PostInvitation Nothing

postInvitation ::
  (HasCallStack, MakesValue user) =>
  user ->
  PostInvitation ->
  App Response
postInvitation user inv = do
  tid <- user %. "team" & asString
  req <-
    baseRequest user Brig Versioned $
      joinHttpPath ["teams", tid, "invitations"]
  email <- maybe randomEmail pure inv.email
  submit "POST" $
    req & addJSONObject ["email" .= email]

getApiVersions :: HasCallStack => App Response
getApiVersions = do
  req <-
    rawBaseRequest OwnDomain Brig Unversioned $
      joinHttpPath ["api-version"]
  submit "GET" req

getSwaggerPublicTOC :: HasCallStack => App Response
getSwaggerPublicTOC = do
  req <-
    rawBaseRequest OwnDomain Brig Unversioned $
      joinHttpPath ["api", "swagger-ui"]
  submit "GET" req

getSwaggerInternalTOC :: HasCallStack => App Response
getSwaggerInternalTOC = error "FUTUREWORK: this API end-point does not exist."

getSwaggerPublicAllUI :: HasCallStack => Int -> App Response
getSwaggerPublicAllUI version = do
  req <-
    rawBaseRequest OwnDomain Brig (ExplicitVersion version) $
      joinHttpPath ["api", "swagger-ui"]
  submit "GET" req

getSwaggerPublicAllJson :: HasCallStack => Int -> App Response
getSwaggerPublicAllJson version = do
  req <-
    rawBaseRequest OwnDomain Brig (ExplicitVersion version) $
      joinHttpPath ["api", "swagger.json"]
  submit "GET" req

getSwaggerInternalUI :: HasCallStack => String -> App Response
getSwaggerInternalUI service = do
  req <-
    rawBaseRequest OwnDomain Brig Unversioned $
      joinHttpPath ["api-internal", "swagger-ui", service]
  submit "GET" req

getSwaggerInternalJson :: HasCallStack => String -> App Response
getSwaggerInternalJson service = do
  req <-
    rawBaseRequest OwnDomain Nginz Unversioned $
      joinHttpPath ["api-internal", "swagger-ui", service <> "-swagger.json"]
  submit "GET" req
