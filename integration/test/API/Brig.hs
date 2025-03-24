module API.Brig where

import API.BrigCommon
import API.Common
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as Base64
import Data.Foldable
import Data.Function
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import GHC.Stack
import Testlib.Prelude

data AddUser = AddUser
  { name :: Maybe String,
    email :: Maybe String,
    teamCode :: Maybe String,
    password :: Maybe String,
    newTeamName :: Maybe String
  }

instance Default AddUser where
  def = AddUser Nothing Nothing Nothing Nothing Nothing

data NewProvider = NewProvider
  { newProviderName :: String,
    newProviderDesc :: String,
    newProviderPassword :: Maybe String,
    newProviderUrl :: String
  }

instance Default NewProvider where
  def =
    NewProvider
      "New Provider"
      "Just a provider"
      Nothing
      "https://example.com"

data NewService = NewService
  { newServiceName :: String,
    newServiceSummary :: String,
    newServiceDescr :: String,
    newServiceUrl :: String,
    newServiceKey :: ByteString,
    newServiceToken :: Maybe String,
    newServiceAssets :: [String],
    newServiceTags :: [String]
  }

instance Default NewService where
  def =
    NewService
      "New Service"
      "Just a service"
      "Just a service description"
      "https://example.com"
      ( T.encodeUtf8 . T.unlines . fmap T.pack $
          [ "-----BEGIN PUBLIC KEY-----",
            "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu+Kg/PHHU3atXrUbKnw0",
            "G06FliXcNt3lMwl2os5twEDcPPFw/feGiAKymxp+7JqZDrseS5D9THGrW+OQRIPH",
            "WvUBdiLfGrZqJO223DB6D8K2Su/odmnjZJ2z23rhXoEArTplu+Dg9K+c2LVeXTKV",
            "VPOaOzgtAB21XKRiQ4ermqgi3/njr03rXyq/qNkuNd6tNcg+HAfGxfGvvCSYBfiS",
            "bUKr/BeArYRcjzr/h5m1In6fG/if9GEI6m8dxHT9JbY53wiksowy6ajCuqskIFg8",
            "7X883H+LA/d6X5CTiPv1VMxXdBUiGPuC9IT/6CNQ1/LFt0P37ax58+LGYlaFo7la",
            "nQIDAQAB",
            "-----END PUBLIC KEY-----"
          ]
      )
      (Just "secret-token")
      []
      ["music", "quiz", "weather"]

instance ToJSON NewService where
  toJSON NewService {..} =
    Aeson.object
      [ "name" .= newServiceName,
        "summary" .= newServiceSummary,
        "description" .= newServiceDescr,
        "base_url" .= newServiceUrl,
        "public_key" .= (T.unpack . T.decodeUtf8) newServiceKey,
        "auth_token" .= newServiceToken,
        "assets" .= Aeson.Array (V.fromList (Aeson.String . T.pack <$> newServiceAssets)),
        "tags" .= Aeson.Array (V.fromList (Aeson.String . T.pack <$> newServiceTags))
      ]

addUser :: (HasCallStack, MakesValue dom) => dom -> AddUser -> App Response
addUser dom opts = do
  req <- baseRequest dom Brig Versioned "register"
  name <- maybe randomName pure opts.name
  submit "POST" $
    req
      & addClientIP
      & addJSONObject
        ( [ "name" .= name,
            "email" .= opts.email,
            "team_code" .= opts.teamCode,
            "password" .= fromMaybe defPassword opts.password
          ]
            <> ["team" .= object ["name" .= n, "icon" .= "default"] | n <- toList opts.newTeamName]
        )

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/get_users__uid_domain___uid_
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

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/get_clients__client_
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

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/get_clients
getSelfClients ::
  (HasCallStack, MakesValue user) =>
  user ->
  App Response
getSelfClients u =
  baseRequest u Brig Versioned (joinHttpPath ["clients"])
    >>= submit "GET"

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/delete_self
deleteUser :: (HasCallStack, MakesValue user) => user -> App Response
deleteUser user = do
  req <- baseRequest user Brig Versioned "/self"
  submit "DELETE" $
    req & addJSONObject ["password" .= defPassword]

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/post_clients
addClient ::
  (HasCallStack, MakesValue user) =>
  user ->
  AddClient ->
  App Response
addClient user args = do
  req <- baseRequest user Brig Versioned $ "/clients"
  val <- mkAddClientValue args
  submit "POST" $ req & addJSONObject val

data UpdateClient = UpdateClient
  { prekeys :: [Value],
    lastPrekey :: Maybe Value,
    label :: Maybe String,
    capabilities :: Maybe [String],
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
  (HasCallStack) =>
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

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/delete_clients__client_
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

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/get_users__uid_domain___uid__clients
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

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/post_users_list_clients
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

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/get_teams__tid__search
searchTeam :: (HasCallStack, MakesValue user) => user -> String -> App Response
searchTeam user q = do
  tid <- user %. "team" & asString
  req <- baseRequest user Brig Versioned $ joinHttpPath ["teams", tid, "search"]
  submit "GET" (req & addQueryParams [("q", q)])

searchTeamAll :: (HasCallStack, MakesValue user) => user -> App Response
searchTeamAll user = do
  tid <- user %. "team" & asString
  req <- baseRequest user Brig Versioned $ joinHttpPath ["teams", tid, "search"]
  submit "GET" (req & addQueryParams [("q", ""), ("size", "100"), ("sortby", "created_at"), ("sortorder", "desc")])

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
    (req & addJSONObject ["key_packages" .= map (T.decodeUtf8 . Base64.encode) kps])

claimKeyPackagesWithParams :: (MakesValue u, MakesValue v) => Ciphersuite -> u -> v -> [(String, String)] -> App Response
claimKeyPackagesWithParams suite u v params = do
  (targetDom, targetUid) <- objQid v
  req <-
    baseRequest u Brig Versioned $
      "/mls/key-packages/claim/" <> targetDom <> "/" <> targetUid
  submit "POST" $
    req
      & addQueryParams ([("ciphersuite", suite.code)] <> params)

claimKeyPackages :: (HasCallStack, MakesValue u, MakesValue v) => Ciphersuite -> u -> v -> App Response
claimKeyPackages suite u v = claimKeyPackagesWithParams suite u v []

countKeyPackages :: Ciphersuite -> ClientIdentity -> App Response
countKeyPackages suite cid = do
  req <- baseRequest cid Brig Versioned ("/mls/key-packages/self/" <> cid.client <> "/count")
  submit "GET" $
    req
      & addQueryParams [("ciphersuite", suite.code)]

deleteKeyPackages :: Ciphersuite -> ClientIdentity -> [String] -> App Response
deleteKeyPackages suite cid kps = do
  req <- baseRequest cid Brig Versioned ("/mls/key-packages/self/" <> cid.client)
  submit "DELETE" $
    req
      & addQueryParams [("ciphersuite", suite.code)]
      & addJSONObject ["key_packages" .= kps]

replaceKeyPackages :: ClientIdentity -> [Ciphersuite] -> [ByteString] -> App Response
replaceKeyPackages cid suites kps = do
  req <-
    baseRequest cid Brig Versioned $
      "/mls/key-packages/self/" <> cid.client
  submit "PUT" $
    req
      & addQueryParams [("ciphersuites", intercalate "," (map (.code) suites))]
      & addJSONObject ["key_packages" .= map (T.decodeUtf8 . Base64.encode) kps]

replaceKeyPackagesV7 :: ClientIdentity -> Maybe [Ciphersuite] -> [ByteString] -> App Response
replaceKeyPackagesV7 cid mSuites kps = do
  req <-
    baseRequest cid Brig (ExplicitVersion 7) $
      "/mls/key-packages/self/" <> cid.client
  submit "PUT" $
    req
      & maybe id (\suites -> addQueryParams [("ciphersuites", intercalate "," (map (.code) suites))]) mSuites
      & addJSONObject ["key_packages" .= map (T.decodeUtf8 . Base64.encode) kps]

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/get_self
getSelf :: (HasCallStack, MakesValue user) => user -> App Response
getSelf = getSelfWithVersion Versioned

getSelfWithVersion :: (HasCallStack, MakesValue user) => Versioned -> user -> App Response
getSelfWithVersion v user = baseRequest user Brig v "/self" >>= submit "GET"

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/get_self
-- this is a low-level version of `getSelf` for testing some error conditions.
getSelf' :: (HasCallStack) => String -> String -> App Response
getSelf' domain uid = getSelfWithVersion Versioned $ object ["domain" .= domain, "id" .= uid]

data PutSelf = PutSelf
  { accent :: Maybe Int,
    assets :: Maybe [Value], -- [{"key":"string", "size":"string", "type":"string"}]
    name :: Maybe String,
    picture :: Maybe [String]
  }

instance Default PutSelf where
  def = PutSelf Nothing Nothing Nothing Nothing

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/put_self
putSelf :: (HasCallStack, MakesValue caller) => caller -> PutSelf -> App Response
putSelf caller body = do
  req <- baseRequest caller Brig Versioned "/self"
  submit "PUT" $
    req
      & addJSONObject
        [ "accent_id" .= body.accent,
          "assets" .= body.assets,
          "name" .= body.name,
          "picture" .= body.picture
        ]

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/put_self_locale
putSelfLocale :: (HasCallStack, MakesValue caller) => caller -> String -> App Response
putSelfLocale caller locale = do
  req <- baseRequest caller Brig Versioned "/self/locale"
  submit "PUT" $ req & addJSONObject ["locale" .= locale]

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/put_users__uid__email
--
-- NOTE: the full process of changing (and confirming) the email address is more complicated.
-- see /services/brig/test/integration for details.
putSelfEmail :: (HasCallStack, MakesValue caller) => caller -> String -> App Response
putSelfEmail caller emailAddress = do
  callerid <- asString $ caller %. "id"
  req <- baseRequest caller Brig Versioned $ joinHttpPath ["users", callerid, "email"]
  submit "PUT" $ req & addJSONObject ["email" .= emailAddress]

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/delete_self_email
deleteSelfEmail :: (HasCallStack, MakesValue caller) => caller -> App Response
deleteSelfEmail caller = do
  req <- baseRequest caller Brig Versioned $ joinHttpPath ["self", "email"]
  submit "DELETE" req

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/put_self_handle
-- FUTUREWORK: rename to putSelfHandle for consistency
putHandle :: (HasCallStack, MakesValue user) => user -> String -> App Response
putHandle user handle = do
  req <- baseRequest user Brig Versioned "/self/handle"
  submit "PUT" $
    req & addJSONObject ["handle" .= handle]

putPassword :: (MakesValue user) => user -> String -> String -> App Response
putPassword user oldPassword newPassword = do
  req <- baseRequest user Brig Versioned "/self/password"
  submit "PUT" $
    req
      & addJSONObject
        [ "old_password" .= oldPassword,
          "new_password" .= newPassword
        ]

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
  { email :: Maybe String,
    role :: Maybe String
  }

instance Default PostInvitation where
  def = PostInvitation Nothing Nothing

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
    req & addJSONObject (["email" .= email] <> ["role" .= r | r <- toList inv.role])

getApiVersions :: (HasCallStack) => App Response
getApiVersions = do
  req <-
    rawBaseRequest OwnDomain Brig Unversioned $
      joinHttpPath ["api-version"]
  submit "GET" req

getSwaggerPublicTOC :: (HasCallStack) => App Response
getSwaggerPublicTOC = do
  req <-
    rawBaseRequest OwnDomain Brig Unversioned $
      joinHttpPath ["api", "swagger-ui"]
  submit "GET" req

getSwaggerPublicAllUI :: (HasCallStack) => Int -> App Response
getSwaggerPublicAllUI version = do
  req <-
    rawBaseRequest OwnDomain Brig (ExplicitVersion version) $
      joinHttpPath ["api", "swagger-ui"]
  submit "GET" req

getSwaggerPublicAllJson :: (HasCallStack) => Int -> App Response
getSwaggerPublicAllJson version = do
  req <-
    rawBaseRequest OwnDomain Brig (ExplicitVersion version) $
      joinHttpPath ["api", "swagger.json"]
  submit "GET" req

getSwaggerInternalUI :: (HasCallStack) => String -> App Response
getSwaggerInternalUI service = do
  req <-
    rawBaseRequest OwnDomain Brig Unversioned $
      joinHttpPath ["api-internal", "swagger-ui", service]
  submit "GET" req

getSwaggerInternalJson :: (HasCallStack) => String -> App Response
getSwaggerInternalJson service = do
  req <-
    rawBaseRequest OwnDomain Nginz Unversioned $
      joinHttpPath ["api-internal", "swagger-ui", service <> "-swagger.json"]
  submit "GET" req

newProvider ::
  ( HasCallStack,
    MakesValue provider,
    MakesValue user
  ) =>
  user ->
  provider ->
  App Value
newProvider user provider = do
  p <- make provider
  req <-
    baseRequest user Brig Versioned $
      joinHttpPath ["provider", "register"]
  submit "POST" (req & addJSON p & addClientIP) `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json

getProvider ::
  (HasCallStack, MakesValue domain) =>
  domain ->
  String ->
  App Response
getProvider domain pid = do
  req <- rawBaseRequest domain Brig Versioned $ joinHttpPath ["provider"]
  submit "GET" $
    req
      & zType "provider"
      & zProvider pid

activateProvider ::
  ( HasCallStack,
    MakesValue dom
  ) =>
  dom ->
  String ->
  String ->
  App ()
activateProvider dom key code = do
  d <- make dom
  req <-
    rawBaseRequest d Brig Versioned $
      joinHttpPath ["provider", "activate"]
  let ps = [("key", key), ("code", code)]
  submit "GET" (addQueryParams ps req) `bindResponse` \resp -> do
    resp.status `shouldMatchOneOf` [Number 200, Number 204]

activateUserV5 :: (HasCallStack, MakesValue dom, MakesValue bdy) => dom -> bdy -> App Response
activateUserV5 dom bdy = do
  b <- make bdy
  req <- rawBaseRequest dom Brig (ExplicitVersion 5) $ joinHttpPath ["activate", "send"]
  submit "POST" $ (addJSON b req)

-- | Returns the value of the Set-Cookie header that is to be used to
-- authenticate to provider endpoints.
loginProvider ::
  ( HasCallStack,
    MakesValue dom
  ) =>
  dom ->
  String ->
  String ->
  App Response
loginProvider dom email pass = do
  d <- asString dom
  req <-
    rawBaseRequest d Brig Versioned $
      joinHttpPath ["provider", "login"]
  submit "POST" (addJSONObject ["email" .= email, "password" .= pass] req)

requestProviderPasswordResetCode ::
  (HasCallStack, MakesValue domain) =>
  domain ->
  String ->
  App Response
requestProviderPasswordResetCode domain email = do
  req <- rawBaseRequest domain Brig Versioned $ joinHttpPath ["provider", "password-reset"]
  submit "POST" $ req & addJSONObject ["email" .= email]

completeProviderPasswordReset ::
  (HasCallStack, MakesValue domain) =>
  domain ->
  Value ->
  String ->
  App Response
completeProviderPasswordReset domain resetCode newPassword = do
  req <- rawBaseRequest domain Brig Versioned $ joinHttpPath ["provider", "password-reset", "complete"]
  body <- make (setField "password" newPassword resetCode)
  submit "POST" $ req & addJSON body

requestProviderEmailUpdateCode ::
  (HasCallStack, MakesValue domain) =>
  domain ->
  String ->
  String ->
  App Response
requestProviderEmailUpdateCode domain pid newEmail = do
  req <- rawBaseRequest domain Brig Versioned $ joinHttpPath ["provider", "email"]
  submit "PUT" $
    req
      & zType "provider"
      & zProvider pid
      & addJSONObject ["email" .= newEmail]

newService ::
  ( HasCallStack,
    MakesValue dom
  ) =>
  dom ->
  String ->
  NewService ->
  App Value
newService dom providerId service = do
  s <- make service
  domain <- asString dom
  req <-
    rawBaseRequest domain Brig Versioned $
      joinHttpPath ["provider", "services"]
  let addHdrs =
        addHeader "Z-Type" "provider"
          . addHeader "Z-Provider" providerId
  submit "POST" (addJSON s . addHdrs $ req) `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json

getService :: (HasCallStack, MakesValue domain) => domain -> String -> String -> App Response
getService domain pid sid = do
  req <- baseRequest domain Brig Versioned $ joinHttpPath ["provider", "services", sid]
  submit "GET" $
    req
      & addHeader "Z-Type" "provider"
      & addHeader "Z-Provider" pid

updateService ::
  ( HasCallStack,
    MakesValue dom,
    MakesValue serviceId
  ) =>
  dom ->
  String ->
  serviceId ->
  Maybe String ->
  Maybe String ->
  App Response
updateService dom providerId serviceId mAcceptHeader newName = do
  sId <- asString serviceId
  domain <- asString dom
  req <-
    rawBaseRequest domain Brig Versioned $
      joinHttpPath ["provider", "services", sId]
  let addHdrs =
        zType "provider"
          . zProvider providerId
          . maybe id (addHeader "Accept") mAcceptHeader
  submit "PUT"
    . addHdrs
    . addJSONObject ["name" .= n | n <- maybeToList newName]
    $ req

updateServiceConn ::
  ( HasCallStack,
    MakesValue domain,
    MakesValue conn
  ) =>
  domain ->
  -- | providerId
  String ->
  -- | serviceId
  String ->
  -- | connection update as a Json object, with an obligatory "password" field
  conn ->
  App Response
updateServiceConn domain providerId serviceId connectionUpdate = do
  req <- baseRequest domain Brig Versioned do
    joinHttpPath ["provider", "services", serviceId, "connection"]
  upd <- make connectionUpdate
  submit "PUT"
    . zType "provider"
    . zProvider providerId
    . addJSON upd
    $ req

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/get_users__uid_domain___uid__prekeys__client_
getUsersPrekeysClient :: (HasCallStack, MakesValue caller, MakesValue targetUser) => caller -> targetUser -> String -> App Response
getUsersPrekeysClient caller targetUser targetClient = do
  dom <- asString $ targetUser %. "domain"
  uid <- asString $ targetUser %. "id"
  req <- baseRequest caller Brig Versioned $ joinHttpPath ["users", dom, uid, "prekeys", targetClient]
  submit "GET" req

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/get_users__uid_domain___uid__prekeys
getUsersPrekeyBundle :: (HasCallStack, MakesValue caller, MakesValue targetUser) => caller -> targetUser -> App Response
getUsersPrekeyBundle caller targetUser = do
  dom <- asString $ targetUser %. "domain"
  uid <- asString $ targetUser %. "id"
  req <- baseRequest caller Brig Versioned $ joinHttpPath ["users", dom, uid, "prekeys"]
  submit "GET" req

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/post_users_list_prekeys
getMultiUserPrekeyBundle :: (HasCallStack, MakesValue caller, ToJSON userClients) => caller -> userClients -> App Response
getMultiUserPrekeyBundle caller userClients = do
  req <- baseRequest caller Brig Versioned $ joinHttpPath ["users", "list-prekeys"]
  submit "POST" (addJSON userClients req)

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/post_access
renewToken :: (HasCallStack, MakesValue uid) => uid -> String -> App Response
renewToken caller cookie = do
  req <- baseRequest caller Brig Versioned "access"
  submit "POST" (addHeader "Cookie" ("zuid=" <> cookie) req)

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/get_calls_config_v2
getCallsConfigV2 :: (HasCallStack, MakesValue user) => user -> App Response
getCallsConfigV2 user = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["calls", "config", "v2"]
  submit "GET" req

addBot :: (HasCallStack, MakesValue user) => user -> String -> String -> String -> App Response
addBot user providerId serviceId convId = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["bot", "conversations", convId]
  submit "POST" $
    req
      & zType "access"
      & addJSONObject ["provider" .= providerId, "service" .= serviceId]

setProperty :: (MakesValue user, ToJSON val) => user -> String -> val -> App Response
setProperty user propName val = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["properties", propName]
  submit "PUT" $ req & addJSON val

getProperty :: (MakesValue user) => user -> String -> App Response
getProperty user propName = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["properties", propName]
  submit "GET" req

deleteProperty :: (MakesValue user) => user -> String -> App Response
deleteProperty user propName = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["properties", propName]
  submit "DELETE" req

getAllPropertyNames :: (MakesValue user) => user -> App Response
getAllPropertyNames user = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["properties"]
  submit "GET" req

getAllPropertyValues :: (MakesValue user) => user -> App Response
getAllPropertyValues user = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["properties-values"]
  submit "GET" req

clearProperties :: (MakesValue user) => user -> App Response
clearProperties user = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["properties"]
  submit "DELETE" req

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/post_oauth_authorization_codes
generateOAuthAuthorizationCode :: (HasCallStack, MakesValue user, MakesValue cid) => user -> cid -> [String] -> String -> App Response
generateOAuthAuthorizationCode user cid scopes redirectUrl = do
  cidStr <- asString cid
  req <- baseRequest user Brig Versioned "/oauth/authorization/codes"
  submit "POST" $
    req
      & addJSONObject
        [ "client_id" .= cidStr,
          "scope" .= unwords scopes,
          "redirect_uri" .= redirectUrl,
          "code_challenge" .= "G7CWLBqYDT8doT_oEIN3un_QwZWYKHmOqG91nwNzITc",
          "code_challenge_method" .= "S256",
          "response_type" .= "code",
          "state" .= "abc"
        ]

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/post_oauth_token
createOAuthAccessToken :: (HasCallStack, MakesValue user, MakesValue cid) => user -> cid -> String -> String -> App Response
createOAuthAccessToken user cid code redirectUrl = do
  cidStr <- asString cid
  req <- baseRequest user Brig Versioned "/oauth/token"
  submit "POST" $
    req
      & addUrlEncodedForm
        [ ("grant_type", "authorization_code"),
          ("client_id", cidStr),
          ("code_verifier", "nE3k3zykOmYki~kriKzAmeFiGT7cWugcuToFwo1YPgrZ1cFvaQqLa.dXY9MnDj3umAmG-8lSNIYIl31Cs_.fV5r2psa4WWZcB.Nlc3A-t3p67NDZaOJjIiH~8PvUH_hR"),
          ("code", code),
          ("redirect_uri", redirectUrl)
        ]

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/post_oauth_token
createOAuthAccessTokenWithRefreshToken :: (HasCallStack, MakesValue user, MakesValue cid) => user -> cid -> String -> App Response
createOAuthAccessTokenWithRefreshToken user cid token = do
  cidStr <- asString cid
  req <- baseRequest user Brig Versioned "/oauth/token"
  submit "POST" $
    req
      & addUrlEncodedForm
        [ ("grant_type", "refresh_token"),
          ("client_id", cidStr),
          ("refresh_token", token)
        ]

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/get_oauth_applications
getOAuthApplications :: (HasCallStack, MakesValue user) => user -> App Response
getOAuthApplications user = do
  req <- baseRequest user Brig Versioned "/oauth/applications"
  submit "GET" req

deleteOAuthSession :: (HasCallStack, MakesValue user, MakesValue cid) => user -> cid -> String -> String -> App Response
deleteOAuthSession user cid password tokenId = do
  cidStr <- asString cid
  req <- baseRequest user Brig Versioned $ joinHttpPath ["oauth", "applications", cidStr, "sessions", tokenId]
  submit "DELETE" $ req & addJSONObject ["password" .= password]

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/delete_oauth_applications__OAuthClientId_
revokeApplicationAccessV6 :: (HasCallStack, MakesValue user, MakesValue cid) => user -> cid -> App Response
revokeApplicationAccessV6 user cid = do
  cidStr <- asString cid
  req <- baseRequest user Brig (ExplicitVersion 6) $ joinHttpPath ["oauth", "applications", cidStr]
  submit "DELETE" req

revokeApplicationAccess :: (HasCallStack, MakesValue user, MakesValue cid) => user -> cid -> String -> App Response
revokeApplicationAccess user cid password = do
  cidStr <- asString cid
  req <- baseRequest user Brig Versioned $ joinHttpPath ["oauth", "applications", cidStr, "sessions"]
  submit "DELETE" $ req & addJSONObject ["password" .= password]

registerUser :: (HasCallStack, MakesValue domain) => domain -> String -> String -> App Response
registerUser domain email inviteeCode = do
  req <- baseRequest domain Brig Versioned "register"
  submit "POST" $
    req
      & addClientIP
      & addJSONObject
        [ "name" .= "Alice",
          "email" .= email,
          "password" .= defPassword,
          "team_code" .= inviteeCode
        ]

activate :: (HasCallStack, MakesValue domain) => domain -> String -> String -> App Response
activate domain key code = do
  req <- rawBaseRequest domain Brig Versioned $ joinHttpPath ["activate"]
  submit "GET" $
    req
      & addQueryParams [("key", key), ("code", code)]

acceptTeamInvitation :: (HasCallStack, MakesValue user) => user -> String -> Maybe String -> App Response
acceptTeamInvitation user code mPw = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["teams", "invitations", "accept"]
  submit "POST" $ req & addJSONObject (["code" .= code] <> maybeToList (((.=) "password") <$> mPw))

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/get_teams__tid__invitations
listInvitations :: (HasCallStack, MakesValue user) => user -> String -> App Response
listInvitations user tid = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["teams", tid, "invitations"]
  submit "GET" req

-- | https://staging-nginz-https.zinfra.io/v7/api/swagger-ui/#/default/get-team-invitation-info
getInvitationByCode :: (HasCallStack, MakesValue user) => user -> String -> App Response
getInvitationByCode user code = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["teams", "invitations", "info"]
  submit "GET" (req & addQueryParams [("code", code)])

passwordReset :: (HasCallStack, MakesValue domain) => domain -> String -> App Response
passwordReset domain email = do
  req <- baseRequest domain Brig Versioned "password-reset"
  submit "POST" $ req & addJSONObject ["email" .= email]

completePasswordReset :: (HasCallStack, MakesValue domain) => domain -> String -> String -> String -> App Response
completePasswordReset domain key code pw = do
  req <- baseRequest domain Brig Versioned $ joinHttpPath ["password-reset", "complete"]
  submit "POST" $ req & addJSONObject ["key" .= key, "code" .= code, "password" .= pw]

login :: (HasCallStack, MakesValue domain) => domain -> String -> String -> App Response
login domain email password = do
  req <- baseRequest domain Brig Versioned "login"
  submit "POST" $ req & addJSONObject ["email" .= email, "password" .= password] & addQueryParams [("persist", "true")]

loginWithSessionCookie :: (HasCallStack, MakesValue domain) => domain -> String -> String -> App Response
loginWithSessionCookie domain email password = do
  req <- baseRequest domain Brig Versioned "login"
  submit "POST" $ req & addJSONObject ["email" .= email, "password" .= password]

updateEmail :: (HasCallStack, MakesValue user) => user -> String -> String -> String -> App Response
updateEmail user email cookie token = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["access", "self", "email"]
  submit "PUT" $ req & addJSONObject ["email" .= email] & setCookie cookie & addHeader "Authorization" ("Bearer " <> token)

upgradePersonalToTeam :: (HasCallStack, MakesValue user) => user -> String -> App Response
upgradePersonalToTeam user name = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["upgrade-personal-to-team"]
  submit "POST" $ req & addJSONObject ["name" .= name, "icon" .= "default"]

postServiceWhitelist ::
  ( HasCallStack,
    MakesValue user,
    MakesValue tid,
    MakesValue update
  ) =>
  user ->
  tid ->
  update ->
  App Response
postServiceWhitelist user tid update = do
  tidStr <- asString tid
  updateJson <- make update
  req <-
    baseRequest user Brig Versioned $
      joinHttpPath
        [ "teams",
          tidStr,
          "services",
          "whitelist"
        ]
  submit "POST" (addJSON updateJson req)

getDomainVerificationChallenge :: (HasCallStack, MakesValue domain) => domain -> String -> App Response
getDomainVerificationChallenge domain emailDomain = do
  req <- baseRequest domain Brig Versioned $ joinHttpPath ["domain-verification", emailDomain, "challenges"]
  submit "POST" req

verifyDomain :: (HasCallStack, MakesValue domain) => domain -> String -> String -> String -> App Response
verifyDomain domain emailDomain challengeId challengeToken = do
  req <-
    baseRequest domain Brig Versioned $
      joinHttpPath
        [ "domain-verification",
          emailDomain,
          "challenges",
          challengeId
        ]
  submit "POST" $ req & addJSONObject ["challenge_token" .= challengeToken]

verifyDomainForTeam :: (HasCallStack, MakesValue user) => user -> String -> String -> String -> App Response
verifyDomainForTeam user emailDomain challengeId challengeToken = do
  req <-
    baseRequest user Brig Versioned $
      joinHttpPath
        [ "domain-verification",
          emailDomain,
          "team",
          "challenges",
          challengeId
        ]
  submit "POST" $ req & addJSONObject ["challenge_token" .= challengeToken]

authorizeTeam :: (HasCallStack, MakesValue user) => user -> String -> String -> App Response
authorizeTeam user emailDomain ownershipToken = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["domain-verification", emailDomain, "authorize-team"]
  submit "POST" $ req & addJSONObject ["domain_ownership_token" .= ownershipToken]

-- brig expects an auth-token for this request. @mAuthToken@ is only `Maybe` for testing error cases!
updateDomainRedirect :: (HasCallStack, MakesValue domain) => domain -> String -> Maybe String -> Value -> App Response
updateDomainRedirect domain emailDomain mAuthToken config = do
  req <-
    baseRequest domain Brig Versioned $
      joinHttpPath ["domain-verification", emailDomain, "backend"]
  let req' = case mAuthToken of
        Just authToken -> addHeader "Authorization" ("Bearer " <> authToken) req
        Nothing -> req
  submit "POST" $ req' & addJSON config

updateTeamInvite :: (HasCallStack, MakesValue user, MakesValue payload) => user -> String -> payload -> App Response
updateTeamInvite user emailDomain payload = do
  req <-
    baseRequest user Brig Versioned $
      joinHttpPath ["domain-verification", emailDomain, "team"]
  p <- make payload
  submit "POST" $ req & addJSON p

getDomainRegistrationFromEmail :: (HasCallStack, MakesValue domain) => domain -> String -> App Response
getDomainRegistrationFromEmail domain email = do
  req <- baseRequest domain Brig Versioned $ joinHttpPath ["get-domain-registration"]
  submit "POST" $ req & addJSONObject ["email" .= email]

getRegisteredDomainsByTeam :: (HasCallStack, MakesValue user) => user -> String -> App Response
getRegisteredDomainsByTeam user tid = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["teams", tid, "registered-domains"]
  submit "GET" req

deleteRegisteredTeamDomain :: (HasCallStack, MakesValue user) => user -> String -> String -> App Response
deleteRegisteredTeamDomain user tid registeredDomain = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["teams", tid, "registered-domains", registeredDomain]
  submit "DELETE" req

listTeamServiceProfilesByPrefix :: (MakesValue user) => user -> String -> Maybe String -> Bool -> Int -> App Response
listTeamServiceProfilesByPrefix user tid mPrefix filterDisabled size = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["teams", tid, "services", "whitelisted"]
  submit "GET" $
    req
      & addQueryParams
        ( catMaybes
            [ ("prefix",) <$> mPrefix,
              if filterDisabled then Nothing else Just ("filter_disabled", "false"),
              Just ("size", show size)
            ]
        )
