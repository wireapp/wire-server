module API.Brig where

import API.BrigCommon
import API.Common
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Base64 as Base64
import qualified Data.CaseInsensitive as CI
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
    password :: Maybe String
  }

instance Default AddUser where
  def = AddUser Nothing Nothing Nothing Nothing

data NewProvider = NewProvider
  { newProviderName :: String,
    newProviderDesc :: String,
    newProviderEmail :: String,
    newProviderPassword :: Maybe String,
    newProviderUrl :: String
  }

instance Default NewProvider where
  def =
    NewProvider
      "New Provider"
      "Just a provider"
      "provider@example.com"
      Nothing
      "https://example.com"

instance ToJSON NewProvider where
  toJSON NewProvider {..} =
    Aeson.object
      [ "name" .= newProviderName,
        "description" .= newProviderDesc,
        "email" .= newProviderEmail,
        "password" .= newProviderPassword,
        "url" .= newProviderUrl
      ]

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
      & addJSONObject
        [ "name" .= name,
          "email" .= opts.email,
          "team_code" .= opts.teamCode,
          "password" .= fromMaybe defPassword opts.password
        ]

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

getUserByHandle :: (HasCallStack, MakesValue user, MakesValue domain) => user -> domain -> String -> App Response
getUserByHandle user domain handle = do
  domainStr <- asString domain
  req <-
    baseRequest user Brig Versioned $
      joinHttpPath ["users", "by-handle", domainStr, handle]
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

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/get_self
getSelf :: (HasCallStack, MakesValue user) => user -> App Response
getSelf = getSelfWithVersion Versioned

getSelfWithVersion :: (HasCallStack, MakesValue user) => Versioned -> user -> App Response
getSelfWithVersion v user = baseRequest user Brig v "/self" >>= submit "GET"

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/get_self
-- this is a low-level version of `getSelf` for testing some error conditions.
getSelf' :: HasCallStack => String -> String -> App Response
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

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/put_self_handle
-- FUTUREWORK: rename to putSelfHandle for consistency
putHandle :: (HasCallStack, MakesValue user) => user -> String -> App Response
putHandle user handle = do
  req <- baseRequest user Brig Versioned "/self/handle"
  submit "PUT" $
    req & addJSONObject ["handle" .= handle]

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
  submit "POST" (addJSON p req) `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json

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

-- | Returns the value of the Set-Cookie header that is to be used to
-- authenticate to provider endpoints.
loginProvider ::
  ( HasCallStack,
    MakesValue dom
  ) =>
  dom ->
  String ->
  String ->
  App ByteString
loginProvider dom email pass = do
  d <- asString dom
  req <-
    rawBaseRequest d Brig Versioned $
      joinHttpPath ["provider", "login"]
  submit "POST" (addJSONObject ["email" .= email, "password" .= pass] req) `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    let hs = headers resp
        setCookieHeader = CI.mk (T.encodeUtf8 . T.pack $ "Set-Cookie")
    pure . fromJust . foldMap (\(k, v) -> guard (k == setCookieHeader) $> v) $ hs

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
  MakesValue conn =>
  -- | providerId
  String ->
  -- | serviceId
  String ->
  -- | connection update as a Json object, with an obligatory "password" field
  conn ->
  App Response
updateServiceConn providerId serviceId connectionUpdate = do
  req <- baseRequest OwnDomain Brig Versioned do
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

addBot :: MakesValue user => user -> String -> String -> String -> App Response
addBot user providerId serviceId convId = do
  req <- baseRequest user Brig Versioned $ joinHttpPath ["conversations", convId, "bots"]
  submit "POST" $
    req
      & zType "access"
      & addJSONObject ["provider" .= providerId, "service" .= serviceId]
