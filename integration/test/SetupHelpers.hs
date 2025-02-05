{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module SetupHelpers where

import API.Brig
import API.BrigInternal
import API.Cargohold
import API.Common
import API.Galley
import API.Spar
import Control.Monad.Reader
import Crypto.Random (getRandomBytes)
import Data.Aeson hiding ((.=))
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64.Lazy as EL
import qualified Data.ByteString.Base64.URL as B64Url
import Data.ByteString.Char8 (unpack)
import qualified Data.CaseInsensitive as CI
import Data.Default
import Data.Function
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import qualified Data.UUID as UUID
import Data.UUID.V1 (nextUUID)
import Data.UUID.V4 (nextRandom)
import Data.Vector (fromList)
import GHC.Stack
import qualified SAML2.WebSSO as SAML
import qualified SAML2.WebSSO.API.Example as SAML
import qualified SAML2.WebSSO.Test.MockResponse as SAML
import SAML2.WebSSO.Test.Util (SampleIdP (..), makeSampleIdPMetadata)
import Test.DNSMock
import Testlib.JSON
import Testlib.Prelude
import Testlib.Printing (indent)
import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML
import qualified Text.XML.DSig as SAML
import UnliftIO (pooledForConcurrentlyN)

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
  owner <- createUser domain def {team = True} >>= getJSON 201
  tid <- owner %. "team" & asString
  members <- pooledForConcurrentlyN 64 [2 .. memberCount] $ \_ -> createTeamMember owner def
  pure (owner, tid, members)

data CreateTeamMember = CreateTeamMember
  { role :: String
  }

instance Default CreateTeamMember where
  def = CreateTeamMember {role = "member"}

createTeamMember ::
  (HasCallStack, MakesValue inviter) =>
  inviter ->
  CreateTeamMember ->
  App Value
createTeamMember inviter args = do
  newUserEmail <- randomEmail
  invitation <-
    postInvitation
      inviter
      def
        { email = Just newUserEmail,
          role = Just args.role
        }
      >>= getJSON 201
  invitationCode <-
    (getInvitationCode inviter invitation >>= getJSON 200)
      %. "code"
      & asString
  let body =
        def
          { name = Just newUserEmail,
            email = Just newUserEmail,
            password = Just defPassword,
            teamCode = Just invitationCode
          }
  addUser inviter body >>= getJSON 201

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
  App (Value, Value, ConvId)
simpleMixedConversationSetup secondDomain = do
  (alice, tid, _) <- createTeam OwnDomain 1
  bob <- randomUser secondDomain def
  connectUsers [alice, bob]

  conv <-
    postConversation alice defProteus {qualifiedUsers = [bob], team = Just tid}
      >>= getJSON 201
      >>= objConvId

  bindResponse (putConversationProtocol bob conv "mixed") $ \resp -> do
    resp.status `shouldMatchInt` 200

  convId <-
    getConversation alice (convIdToQidObject conv)
      >>= getJSON 200
      >>= objConvId

  pure (alice, bob, convId)

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
setupProvider u (NewProvider {..}) = do
  dom <- objDomain u
  providerEmail <- randomEmail
  newProviderResponse <-
    newProvider u $
      object
        [ "name" .= newProviderName,
          "description" .= newProviderDesc,
          "email" .= providerEmail,
          "password" .= newProviderPassword,
          "url" .= newProviderUrl
        ]
  pass <- case newProviderPassword of
    Nothing -> newProviderResponse %. "password" & asString
    Just pass -> pure pass
  (key, code) <- do
    pair <-
      getProviderActivationCodeInternal dom providerEmail `bindResponse` \resp -> do
        resp.status `shouldMatchInt` 200
        resp.json
    k <- pair %. "key" & asString
    c <- pair %. "code" & asString
    pure (k, c)
  activateProvider dom key code
  void $ loginProvider dom providerEmail pass
  pid <- asString $ newProviderResponse %. "id"
  getProvider dom pid >>= getJSON 200

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
  randomScimUserWith email email

randomScimUserWith :: (HasCallStack) => String -> String -> App Value
randomScimUserWith extId email = do
  handle <- randomHandleWithRange 12 128
  pure $
    object
      [ "schemas" .= ["urn:ietf:params:scim:schemas:core:2.0:User"],
        "externalId" .= extId,
        "emails" .= Array (fromList [object ["value" .= email]]),
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

addUsersToFailureContext :: (MakesValue user) => [(String, user)] -> App a -> App a
addUsersToFailureContext namesAndUsers action = do
  let mkLine (name, user) = do
        (domain, id_) <- objQid user
        pure $ name <> ": " <> id_ <> "@" <> domain
  allLines <- unlines <$> (mapM mkLine namesAndUsers)
  addFailureContext allLines action

addJSONToFailureContext :: (MakesValue a) => String -> a -> App b -> App b
addJSONToFailureContext name ctx action = do
  jsonStr <- prettyJSON ctx
  let ctxStr = unlines [name <> ":", indent 2 jsonStr]
  addFailureContext ctxStr action

registerTestIdPWithMeta :: (HasCallStack, MakesValue owner) => owner -> App Response
registerTestIdPWithMeta owner = fst <$> registerTestIdPWithMetaWithPrivateCreds owner

registerTestIdPWithMetaWithPrivateCreds :: (HasCallStack, MakesValue owner) => owner -> App (Response, (SAML.IdPMetadata, SAML.SignPrivCreds))
registerTestIdPWithMetaWithPrivateCreds owner = do
  SampleIdP idpmeta pCreds _ _ <- makeSampleIdPMetadata
  (,(idpmeta, pCreds)) <$> createIdp owner idpmeta

-- | Given a team configured with saml sso, attempt a login with valid credentials.  This
-- function simulates client *and* IdP (instead of talking to an IdP).  It can be used to test
-- scim-provisioned users as well as saml auto-provisioning without scim.
loginWithSaml :: (HasCallStack) => Bool -> String -> String -> (String, (SAML.IdPMetadata, SAML.SignPrivCreds)) -> App ()
loginWithSaml expectSuccess tid email (iid, (meta, privcreds)) = do
  let idpConfig = SAML.IdPConfig (SAML.IdPId (fromMaybe (error "invalid idp id") (UUID.fromString iid))) meta ()
  spmeta <- getSPMetadata OwnDomain tid
  authnreq <- initiateSamlLogin OwnDomain iid
  let nameId = fromRight (error "could not create name id") $ SAML.emailNameID (cs email)
  authnResp <- runSimpleSP $ SAML.mkAuthnResponseWithSubj nameId privcreds idpConfig (toSPMetaData spmeta.body) (parseAuthnReqResp authnreq.body) True
  finalizeSamlLogin OwnDomain tid authnResp `bindResponse` validateLoginResp
  where
    toSPMetaData :: ByteString -> SAML.SPMetadata
    toSPMetaData bs = fromRight (error "could not decode spmetatdata") $ SAML.decode $ cs bs

    validateLoginResp :: (HasCallStack) => Response -> App ()
    validateLoginResp resp =
      if expectSuccess
        then do
          resp.status `shouldMatchInt` 200
          let bdy = cs resp.body
          bdy `shouldContain` "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
          bdy `shouldContain` "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
          bdy `shouldContain` "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
          bdy `shouldContain` "<title>wire:sso:success</title>"
          bdy `shouldContain` "window.opener.postMessage({type: 'AUTH_SUCCESS'}, receiverOrigin)"
          hasPersistentCookieHeader resp
        else do
          resp.status `shouldMatchInt` 200
          let bdy = cs resp.body
          bdy `shouldContain` "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
          bdy `shouldContain` "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.1//EN\" \"http://www.w3.org/TR/xhtml11/DTD/xhtml11.dtd\">"
          bdy `shouldContain` "<title>wire:sso:error:"
          bdy `shouldContain` "window.opener.postMessage({"
          bdy `shouldContain` "\"type\":\"AUTH_ERROR\""
          bdy `shouldContain` "\"payload\":{"
          bdy `shouldContain` "\"label\":\"forbidden\""
          bdy `shouldContain` "}, receiverOrigin)"
          hasPersistentCookieHeader resp

    hasPersistentCookieHeader :: Response -> App ()
    hasPersistentCookieHeader rsp = do
      let cookie = getCookie "zuid" rsp
      case cookie of
        Nothing -> expectSuccess `shouldMatch` False
        Just _ -> expectSuccess `shouldMatch` True

    runSimpleSP :: SAML.SimpleSP a -> App a
    runSimpleSP action = liftIO $ do
      ctx <- SAML.mkSimpleSPCtx undefined []
      result <- SAML.runSimpleSP ctx action
      pure $ fromRight (error "simple sp action failed") result

    parseAuthnReqResp ::
      ByteString ->
      SAML.AuthnRequest
    parseAuthnReqResp bs = reqBody
      where
        xml :: XML.Document
        xml =
          fromRight (error "malformed html in response body") $
            XML.parseText XML.def (cs bs)

        reqBody :: SAML.AuthnRequest
        reqBody =
          (XML.fromDocument xml XML.$// XML.element (XML.Name (cs "input") (Just (cs "http://www.w3.org/1999/xhtml")) Nothing))
            & head
            & XML.attribute (fromString "value")
            & head
            & cs
            & EL.decode
            & fromRight (error "")
            & cs
            & SAML.decodeElem
            & fromRight (error "")

-- helpers

data ChallengeSetup = ChallengeSetup
  { dnsToken :: String,
    challengeId :: String,
    challengeToken :: String,
    technitiumToken :: String
  }

setupChallenge :: (MakesValue domain, HasCallStack) => domain -> String -> App ChallengeSetup
setupChallenge domain registrationDomain = do
  challenge <- getDomainVerificationChallenge domain registrationDomain >>= getJSON 200
  dnsToken <- challenge %. "dns_verification_token" & asString
  challengeId <- challenge %. "id" & asString
  challengeToken <- challenge %. "token" & asString

  technitiumToken <- getTechnitiumApiKey
  registerTechnitiumZone technitiumToken registrationDomain

  pure $
    ChallengeSetup
      { dnsToken,
        challengeId,
        challengeToken,
        technitiumToken
      }

data DomainRegistrationSetup = DomainRegistrationSetup
  { dnsToken :: String,
    technitiumToken :: String,
    ownershipToken :: String
  }

setupOwnershipToken :: (MakesValue domain, HasCallStack) => domain -> String -> App DomainRegistrationSetup
setupOwnershipToken domain registrationDomain = do
  challenge <- setupChallenge domain registrationDomain

  -- register TXT DNS record
  registerTechnitiumRecord challenge.technitiumToken registrationDomain ("wire-domain." <> registrationDomain) "TXT" challenge.dnsToken

  -- verify domain
  ownershipToken <- bindResponse (verifyDomain OwnDomain registrationDomain challenge.challengeId challenge.challengeToken) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "domain_ownership_token" & asString

  pure $ DomainRegistrationSetup challenge.dnsToken challenge.technitiumToken ownershipToken
