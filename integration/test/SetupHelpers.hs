{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module SetupHelpers where

import API.Brig
import API.BrigInternal
import API.Cargohold
import API.Common
import API.Galley
import API.GalleyInternal (legalholdWhitelistTeam)
import Control.Monad.Reader
import Crypto.Random (getRandomBytes)
import Data.Aeson hiding ((.=))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64.URL as B64Url
import Data.ByteString.Char8 (unpack)
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.Function
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Data.UUID.V1 (nextUUID)
import Data.UUID.V4 (nextRandom)
import GHC.Stack
import Testlib.MockIntegrationService (mkLegalHoldSettings)
import Testlib.Prelude
import Testlib.VersionedFed (StaticDomain (StaticFedDomain))

randomUser :: (HasCallStack, MakesValue domain) => domain -> CreateUser -> App Value
randomUser domain cu = bindResponse (createUser domain cu) $ \resp -> do
  resp.status `shouldMatchInt` 201
  resp.json

deleteUser :: (HasCallStack, MakesValue user) => user -> App ()
deleteUser user = bindResponse (API.Brig.deleteUser user) $ \resp -> do
  resp.status `shouldMatchInt` 200

-- | returns (owner, team id, members)
createTeam :: (HasCallStack, MakesValue domain) => domain -> Int -> App (Value, String, [Value])
createTeam domain memberCount = do
  res <- createUser domain def {team = True}
  owner <- res.json
  tid <- owner %. "team" & asString
  members <- for [2 .. memberCount] $ \_ -> createTeamMember owner tid
  pure (owner, tid, members)

createTeamMember ::
  (HasCallStack, MakesValue inviter) =>
  inviter ->
  String ->
  App Value
createTeamMember inviter tid = createTeamMemberWithRole inviter tid "member"

createTeamMemberWithRole ::
  (HasCallStack, MakesValue inviter) =>
  inviter ->
  String ->
  String ->
  App Value
createTeamMemberWithRole inviter tid role = do
  newUserEmail <- randomEmail
  let invitationJSON = ["role" .= role, "email" .= newUserEmail]
  invitationReq <-
    baseRequest inviter Brig Versioned $
      joinHttpPath ["teams", tid, "invitations"]
  invitation <- getJSON 201 =<< submit "POST" (addJSONObject invitationJSON invitationReq)
  invitationId <- objId invitation
  invitationCodeReq <-
    rawBaseRequest inviter Brig Unversioned "/i/teams/invitation-code"
      <&> addQueryParams [("team", tid), ("invitation_id", invitationId)]
  invitationCode <- bindResponse (submit "GET" invitationCodeReq) $ \res -> do
    res.status `shouldMatchInt` 200
    res.json %. "code" & asString
  let registerJSON =
        [ "name" .= newUserEmail,
          "email" .= newUserEmail,
          "password" .= defPassword,
          "team_code" .= invitationCode
        ]
  registerReq <-
    rawBaseRequest inviter Brig Versioned "/register"
      <&> addJSONObject registerJSON
  getJSON 201 =<< submit "POST" registerReq

connectTwoUsers ::
  ( HasCallStack,
    MakesValue alice,
    MakesValue bob
  ) =>
  alice ->
  bob ->
  App ()
connectTwoUsers alice bob = do
  postConnection alice bob >>= assertSuccess
  putConnection bob alice "accepted" >>= assertSuccess

connectUsers :: (HasCallStack, MakesValue usr) => [usr] -> App ()
connectUsers users = traverse_ (uncurry connectTwoUsers) $ do
  t <- tails users
  (a, others) <- maybeToList (uncons t)
  b <- others
  pure (a, b)

assertConnection :: (HasCallStack, MakesValue alice, MakesValue bob) => alice -> bob -> String -> App ()
assertConnection alice bob status =
  getConnection alice bob `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "status" `shouldMatch` status

createAndConnectUsers :: (HasCallStack, MakesValue domain) => [domain] -> App [Value]
createAndConnectUsers domains = do
  users <- for domains (flip randomUser def)
  connectUsers users
  pure users

createUsers :: (HasCallStack, MakesValue domain) => [domain] -> App [Value]
createUsers domains = for domains (flip randomUser def)

getAllConvs :: (HasCallStack, MakesValue u) => u -> App [Value]
getAllConvs u = do
  page <- bindResponse (listConversationIds u def) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json
  ids <- page %. "qualified_conversations" & asList
  result <- bindResponse (listConversations u ids) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json
  result %. "found" & asList

-- | Setup a team user, another user, connect the two, create a proteus
-- conversation, upgrade to mixed. Return the two users and the conversation.
simpleMixedConversationSetup ::
  (HasCallStack, MakesValue domain) =>
  domain ->
  App (Value, Value, Value)
simpleMixedConversationSetup secondDomain = do
  (alice, tid, _) <- createTeam OwnDomain 1
  bob <- randomUser secondDomain def
  connectUsers [alice, bob]

  conv <-
    postConversation alice defProteus {qualifiedUsers = [bob], team = Just tid}
      >>= getJSON 201

  bindResponse (putConversationProtocol bob conv "mixed") $ \resp -> do
    resp.status `shouldMatchInt` 200

  modifyMLSState $ \mls -> mls {protocol = MLSProtocolMixed}

  conv' <- getConversation alice conv >>= getJSON 200

  pure (alice, bob, conv')

supportMLS :: (HasCallStack, MakesValue u) => u -> App ()
supportMLS u = do
  prots <- bindResponse (getUserSupportedProtocols u u) $ \resp -> do
    resp.status `shouldMatchInt` 200
    prots <- resp.json & asList
    traverse asString prots
  let prots' = "mls" : prots
  bindResponse (putUserSupportedProtocols u prots') $ \resp ->
    resp.status `shouldMatchInt` 200

addUserToTeam :: (HasCallStack, MakesValue u) => u -> App Value
addUserToTeam u = do
  inv <- postInvitation u def >>= getJSON 201
  email <- inv %. "email" & asString
  resp <- getInvitationCode u inv >>= getJSON 200
  code <- resp %. "code" & asString
  addUser u def {email = Just email, teamCode = Just code} >>= getJSON 201

-- | Create a user on the given domain, such that the 1-1 conversation with
-- 'other' resides on 'convDomain'. This connects the two users as a side-effect.
createMLSOne2OnePartner ::
  (MakesValue user, MakesValue domain, MakesValue convDomain, HasCallStack) =>
  domain ->
  user ->
  convDomain ->
  App Value
createMLSOne2OnePartner domain other convDomain = loop
  where
    loop = do
      u <- randomUser domain def
      connectTwoUsers u other
      apiVersion <- getAPIVersionFor domain
      conv <-
        if apiVersion < 6
          then getMLSOne2OneConversation other u >>= getJSON 200
          else getMLSOne2OneConversation other u >>= getJSON 200 >>= (%. "conversation")

      desiredConvDomain <- make convDomain & asString
      actualConvDomain <- conv %. "qualified_id.domain" & asString

      if desiredConvDomain == actualConvDomain
        then pure u
        else loop

-- | Create a user on the given domain, such that the 1-1 conversation with
-- 'other' resides on 'convDomain'. This connects the two users as a side-effect.
createMLSOne2OnePartnerFedV1 ::
  (MakesValue user, MakesValue convDomain, HasCallStack) =>
  user ->
  convDomain ->
  App Value
createMLSOne2OnePartnerFedV1 other convDomain = loop
  where
    loop = do
      u <- randomUser (StaticFedDomain 1) def
      connectTwoUsers u other
      resp <- getMLSOne2OneConversation other u
      if resp == 533 && convDomain ~= user.domain
        then 

      desiredConvDomain <- make convDomain & asString
      actualConvDomain <- conv %. "qualified_id.domain" & asString

      if desiredConvDomain == actualConvDomain
        then pure u
        else loop

-- Copied from `src/CargoHold/API/V3.hs` and inlined to avoid pulling in `types-common`
randomToken :: (HasCallStack) => App String
randomToken = unpack . B64Url.encode <$> liftIO (getRandomBytes 16)

data TokenLength = GCM | APNS

randomSnsToken :: (HasCallStack) => TokenLength -> App String
randomSnsToken = \case
  GCM -> mkTok 16
  APNS -> mkTok 32
  where
    mkTok = fmap (Text.unpack . decodeUtf8 . Base16.encode) . randomBytes

randomId :: (HasCallStack) => App String
randomId = liftIO (show <$> nextRandom)

randomUUIDv1 :: (HasCallStack) => App String
randomUUIDv1 = liftIO (show . fromJust <$> nextUUID)

randomUserId :: (HasCallStack, MakesValue domain) => domain -> App Value
randomUserId domain = do
  d <- make domain
  uid <- randomId
  pure $ object ["id" .= uid, "domain" .= d]

withFederatingBackendsAllowDynamic :: (HasCallStack) => ((String, String, String) -> App a) -> App a
withFederatingBackendsAllowDynamic k = do
  let setFederationConfig =
        setField "optSettings.setFederationStrategy" "allowDynamic"
          >=> setField "optSettings.setFederationDomainConfigsUpdateFreq" (Aeson.Number 1)
  startDynamicBackends
    [ def {brigCfg = setFederationConfig},
      def {brigCfg = setFederationConfig},
      def {brigCfg = setFederationConfig}
    ]
    $ \[domainA, domainB, domainC] -> k (domainA, domainB, domainC)

-- | Create two users on different domains such that the one-to-one
-- conversation, once finalised, will be hosted on the backend given by the
-- first domain.
createOne2OneConversation ::
  (HasCallStack, MakesValue domain1, MakesValue domain2) =>
  domain1 ->
  domain2 ->
  App (Value, Value, Value)
createOne2OneConversation owningDomain otherDomain = do
  owningUser <- randomUser owningDomain def
  domainName <- owningUser %. "qualified_id.domain"
  let go = do
        otherUser <- randomUser otherDomain def
        otherUserId <- otherUser %. "qualified_id"
        conn <-
          postConnection owningUser otherUser `bindResponse` \resp -> do
            resp.status `shouldMatchInt` 201
            payload <- resp.json
            payload %. "status" `shouldMatch` "sent"
            payload %. "qualified_to" `shouldMatch` otherUserId
            pure payload
        one2one <- conn %. "qualified_conversation"
        one2oneDomain <- one2one %. "domain"
        if domainName == one2oneDomain
          then pure (owningUser, otherUser, one2one)
          else SetupHelpers.deleteUser otherUser >> go
  go

data One2OneConvState = Established | Connect

-- | Converts to an integer corresponding to the numeric representation of the
-- 'Wire.API.Conversation.ConvType' type.
toConvType :: One2OneConvState -> Int
toConvType = \case
  Established -> 2
  Connect -> 3

-- | Fetch the one-to-one conversation between the two users that is in one of
-- two possible states.
getOne2OneConversation :: (HasCallStack) => Value -> Value -> One2OneConvState -> App Value
getOne2OneConversation user1 user2 cnvState = do
  l <- getAllConvs user1
  let isWith users c = do
        -- The conversation type 2 is for 1-to-1 conversations. Type 3 is for
        -- the connection conversation, which is the state of the conversation
        -- before the connection is fully established.
        t <- (== toConvType cnvState) <$> (c %. "type" & asInt)
        others <- c %. "members.others" & asList
        qIds <- for others (%. "qualified_id")
        pure $ qIds == users && t
  head <$> filterM (isWith [user2]) l

-- | Create a provider, get an activation code, activate the provider and log it
-- in. The return value is the created provider.
setupProvider ::
  ( HasCallStack,
    MakesValue user
  ) =>
  user ->
  NewProvider ->
  App Value
setupProvider u np@(NewProvider {..}) = do
  dom <- objDomain u
  provider <- newProvider u np
  pass <- case newProviderPassword of
    Nothing -> provider %. "password" & asString
    Just pass -> pure pass
  (key, code) <- do
    pair <-
      getProviderActivationCodeInternal dom newProviderEmail `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json
    k <- pair %. "key" & asString
    c <- pair %. "code" & asString
    pure (k, c)
  activateProvider dom key code
  loginProvider dom newProviderEmail pass $> provider

-- | setup a legalhold device for @uid@, authorised by @owner@
--   at the specified port
setUpLHDevice ::
  (HasCallStack, MakesValue tid, MakesValue owner, MakesValue uid) =>
  tid ->
  owner ->
  uid ->
  -- | the host and port the LH service is running on
  (String, Int) ->
  App ()
setUpLHDevice tid alice bob lhPort = do
  legalholdWhitelistTeam tid alice
    >>= assertStatus 200

  -- the status messages for these have already been tested
  postLegalHoldSettings tid alice (mkLegalHoldSettings lhPort)
    >>= assertStatus 201

  requestLegalHoldDevice tid alice bob
    >>= assertStatus 201

  approveLegalHoldDevice tid bob defPassword
    >>= assertStatus 200

lhDeviceIdOf :: (MakesValue user) => user -> App String
lhDeviceIdOf bob = do
  bobId <- objId bob
  getClientsFull bob [bobId] `bindResponse` \resp ->
    do
      resp.json %. bobId
        & asList
          >>= filterM \val -> (== "legalhold") <$> (val %. "type" & asString)
      >>= assertOne
      >>= (%. "id")
      >>= asString

randomScimUser :: App Value
randomScimUser = do
  email <- randomEmail
  handle <- randomHandleWithRange 12 128
  pure $
    object
      [ "schemas" .= ["urn:ietf:params:scim:schemas:core:2.0:User"],
        "externalId" .= email,
        "userName" .= handle,
        "displayName" .= handle
      ]

-- | This adds one random asset to the `assets` field in the user record and returns an asset
-- key.  The asset carries a fresh UUIDv4 in text form (even though it is typed 'preview` and
-- `image').
uploadProfilePicture :: (HasCallStack, MakesValue usr) => usr -> App (String, String, String)
uploadProfilePicture usr = do
  payload <- ("asset_contents=" <>) <$> randomId
  asset <- bindResponse (uploadFreshAsset usr payload) (getJSON 201)
  dom <- asset %. "domain" & asString
  key <- asset %. "key" & asString
  Success (oldAssets :: [Value]) <- bindResponse (getSelf usr) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "assets" <&> fromJSON
  bindResponse
    (putSelf usr def {assets = Just (object ["key" .= key, "size" .= "preview", "type" .= "image"] : oldAssets)})
    assertSuccess
  pure (dom, key, payload)

-- | Take a calling user (any user will do) and an asset domain and key, and return a
-- (temporarily valid) s3 url plus asset payload (if created with `uploadProfilePicture`,
-- that's a UUIDv4).
downloadProfilePicture :: (HasCallStack, MakesValue caller) => caller -> String -> String -> App (String, String)
downloadProfilePicture caller assetDomain assetKey = do
  locurl <- bindResponse (downloadAsset caller caller assetKey assetDomain noRedirect) $ \resp -> do
    resp.status `shouldMatchInt` 302
    maybe
      (error "no location header in 302 response!?")
      (pure . cs)
      (lookup (CI.mk (cs "Location")) resp.headers)

  payload <- bindResponse (downloadAsset caller caller assetKey assetDomain id) $ \resp -> do
    resp.status `shouldMatchInt` 200
    pure $ cs resp.body

  pure (locurl, payload)

-- | Call 'uploadProfilePicture' and 'downloadPicture', returning the return value of the
-- latter.
uploadDownloadProfilePicture :: (HasCallStack, MakesValue usr) => usr -> App (String, String)
uploadDownloadProfilePicture usr = do
  (dom, key, _payload) <- uploadProfilePicture usr
  downloadProfilePicture usr dom key
