{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module API.Galley where

import API.Common
import Control.Lens hiding ((.=))
import Control.Monad.Reader
import Control.Retry
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ProtoLens as Proto
import Data.ProtoLens.Labels ()
import qualified Data.UUID as UUID
import Numeric.Lens
import Proto.Otr as Proto
import Testlib.Prelude

data CreateConv = CreateConv
  { qualifiedUsers :: [Value],
    name :: Maybe String,
    access :: Maybe [String],
    accessRole :: Maybe [String],
    team :: Maybe String,
    messageTimer :: Maybe Int,
    receiptMode :: Maybe Int,
    newUsersRole :: String,
    protocol :: String
  }

defProteus :: CreateConv
defProteus =
  CreateConv
    { qualifiedUsers = [],
      name = Nothing,
      access = Nothing,
      accessRole = Nothing,
      team = Nothing,
      messageTimer = Nothing,
      receiptMode = Nothing,
      newUsersRole = "wire_admin",
      protocol = "proteus"
    }

defMLS :: CreateConv
defMLS = defProteus {protocol = "mls"}

allowGuests :: CreateConv -> CreateConv
allowGuests cc =
  cc
    { access = Just ["code"],
      accessRole = Just ["team_member", "guest"]
    }

instance MakesValue CreateConv where
  make cc = do
    quids <- for (cc.qualifiedUsers) objQidObject
    pure
      $ Aeson.object
      $ ( [ "qualified_users" .= quids,
            "conversation_role" .= cc.newUsersRole,
            "protocol" .= cc.protocol
          ]
            <> catMaybes
              [ "name" .=? cc.name,
                "access" .=? cc.access,
                "access_role_v2" .=? cc.access,
                "team" .=? (cc.team <&> \tid -> Aeson.object ["teamid" .= tid, "managed" .= False]),
                "message_timer" .=? cc.messageTimer,
                "receipt_mode" .=? cc.receiptMode
              ]
        )

postConversation ::
  ( HasCallStack,
    MakesValue user
  ) =>
  user ->
  CreateConv ->
  App Response
postConversation user cc = do
  req <- baseRequest user Galley Versioned "/conversations"
  ccv <- make cc
  submit "POST" $ req & addJSON ccv

deleteTeamConversation ::
  ( HasCallStack,
    MakesValue user,
    MakesValue conv
  ) =>
  String ->
  conv ->
  user ->
  App Response
deleteTeamConversation tid qcnv user = do
  cnv <- snd <$> objQid qcnv
  let path = joinHttpPath ["teams", tid, "conversations", cnv]
  req <- baseRequest user Galley Versioned path
  submit "DELETE" req

deleteTeamMember ::
  ( HasCallStack,
    MakesValue owner,
    MakesValue member
  ) =>
  String ->
  owner ->
  member ->
  App Response
deleteTeamMember tid owner mem = do
  memId <- objId mem
  let path = joinHttpPath ["teams", tid, "members", memId]
  req <- baseRequest owner Galley Versioned path
  submit "DELETE" (addJSONObject ["password" .= defPassword] req)

putConversationProtocol ::
  ( HasCallStack,
    MakesValue user,
    MakesValue qcnv,
    MakesValue protocol
  ) =>
  user ->
  qcnv ->
  protocol ->
  App Response
putConversationProtocol user qcnv protocol = do
  (domain, cnv) <- objQid qcnv
  p <- asString protocol
  req <- baseRequest user Galley Versioned (joinHttpPath ["conversations", domain, cnv, "protocol"])
  submit "PUT" (req & addJSONObject ["protocol" .= p])

getConversation ::
  ( HasCallStack,
    MakesValue user,
    MakesValue qcnv
  ) =>
  user ->
  qcnv ->
  App Response
getConversation user qcnv = do
  (domain, cnv) <- objQid qcnv
  req <- baseRequest user Galley Versioned (joinHttpPath ["conversations", domain, cnv])
  submit "GET" req

getSubConversation ::
  ( HasCallStack,
    MakesValue user,
    MakesValue conv
  ) =>
  user ->
  conv ->
  String ->
  App Response
getSubConversation user conv sub = do
  (cnvDomain, cnvId) <- objQid conv
  req <-
    baseRequest user Galley Versioned
      $ joinHttpPath
        [ "conversations",
          cnvDomain,
          cnvId,
          "subconversations",
          sub
        ]
  submit "GET" req

deleteSubConversation ::
  (HasCallStack, MakesValue user, MakesValue sub) =>
  user ->
  sub ->
  App Response
deleteSubConversation user sub = do
  (conv, Just subId) <- objSubConv sub
  (domain, convId) <- objQid conv
  groupId <- sub %. "group_id" & asString
  epoch :: Int <- sub %. "epoch" & asIntegral
  req <-
    baseRequest user Galley Versioned
      $ joinHttpPath ["conversations", domain, convId, "subconversations", subId]
  submit "DELETE" $ req & addJSONObject ["group_id" .= groupId, "epoch" .= epoch]

leaveSubConversation ::
  (HasCallStack, MakesValue user) =>
  user ->
  ConvId ->
  App Response
leaveSubConversation user convId = do
  (domain, mainConvId) <- objQid convId
  let Just subId = convId.subconvId
  req <-
    baseRequest user Galley Versioned
      $ joinHttpPath ["conversations", domain, mainConvId, "subconversations", subId, "self"]
  submit "DELETE" req

getSelfConversation :: (HasCallStack, MakesValue user) => user -> App Response
getSelfConversation user = do
  req <- baseRequest user Galley Versioned "/conversations/mls-self"
  submit "GET" $ req

data ListConversationIds = ListConversationIds {pagingState :: Maybe String, size :: Maybe Int}

instance Default ListConversationIds where
  def = ListConversationIds Nothing Nothing

listConversationIds :: (MakesValue user) => user -> ListConversationIds -> App Response
listConversationIds user args = do
  req <- baseRequest user Galley Versioned "/conversations/list-ids"
  submit "POST"
    $ req
    & addJSONObject
      ( ["paging_state" .= s | s <- toList args.pagingState]
          <> ["size" .= s | s <- toList args.size]
      )

listConversations :: (MakesValue user) => user -> [Value] -> App Response
listConversations user cnvs = do
  req <- baseRequest user Galley Versioned "/conversations/list"
  submit "POST"
    $ req
    & addJSONObject ["qualified_ids" .= cnvs]

getMLSPublicKeys :: (HasCallStack, MakesValue user) => user -> App Response
getMLSPublicKeys user = do
  req <- baseRequest user Galley Versioned "/mls/public-keys"
  submit "GET" req

getMLSPublicKeysJWK :: (HasCallStack, MakesValue user) => user -> App Response
getMLSPublicKeysJWK user = do
  req <- baseRequest user Galley Versioned "/mls/public-keys"
  submit "GET" $ addQueryParams [("format", "jwk")] req

postMLSMessage :: (HasCallStack) => ClientIdentity -> ByteString -> App Response
postMLSMessage cid msg = do
  req <- baseRequest cid Galley Versioned "/mls/messages"
  submit "POST" (addMLS msg req)

postMLSCommitBundle :: (HasCallStack) => ClientIdentity -> ByteString -> App Response
postMLSCommitBundle cid msg = do
  req <- baseRequest cid Galley Versioned "/mls/commit-bundles"
  submit "POST" (addMLS msg req)

postProteusMessage :: (HasCallStack, MakesValue user, MakesValue conv) => user -> conv -> QualifiedNewOtrMessage -> App Response
postProteusMessage user conv msgs = do
  convDomain <- objDomain conv
  convId <- objId conv
  let bytes = Proto.encodeMessage msgs
  req <- baseRequest user Galley Versioned (joinHttpPath ["conversations", convDomain, convId, "proteus", "messages"])
  submit "POST" (addProtobuf bytes req)

mkProteusRecipient :: (HasCallStack, MakesValue user, MakesValue client) => user -> client -> String -> App Proto.QualifiedUserEntry
mkProteusRecipient user client = mkProteusRecipients user [(user, [client])]

mkProteusRecipients :: (HasCallStack, MakesValue domain, MakesValue user, MakesValue client) => domain -> [(user, [client])] -> String -> App Proto.QualifiedUserEntry
mkProteusRecipients dom userClients msg = do
  userDomain <- asString =<< objDomain dom
  userEntries <- mapM mkUserEntry userClients
  pure
    $ Proto.defMessage
    & #domain .~ fromString userDomain
    & #entries .~ userEntries
  where
    mkUserEntry (user, clients) = do
      userId <- LBS.toStrict . UUID.toByteString . fromJust . UUID.fromString <$> objId user
      clientEntries <- mapM mkClientEntry clients
      pure
        $ Proto.defMessage
        & #user . #uuid .~ userId
        & #clients .~ clientEntries
    mkClientEntry client = do
      clientId <- (^?! hex) <$> objId client
      pure
        $ Proto.defMessage
        & #client . #client .~ clientId
        & #text .~ fromString msg

getGroupInfo ::
  (HasCallStack, MakesValue user) =>
  user ->
  ConvId ->
  App Response
getGroupInfo user conv = do
  let path = joinHttpPath $ case conv.subconvId of
        Nothing -> ["conversations", conv.domain, conv.id_, "groupinfo"]
        Just sub -> ["conversations", conv.domain, conv.id_, "subconversations", sub, "groupinfo"]
  req <- baseRequest user Galley Versioned path
  submit "GET" req

removeConversationMember ::
  (HasCallStack, MakesValue user, MakesValue conv) =>
  user ->
  conv ->
  App Response
removeConversationMember user conv = do
  (convDomain, convId) <- objQid conv
  (userDomain, userId) <- objQid user
  req <- baseRequest user Galley Versioned (joinHttpPath ["conversations", convDomain, convId, "members", userDomain, userId])
  submit "DELETE" req

updateConversationMember ::
  (HasCallStack, MakesValue user, MakesValue conv, MakesValue target) =>
  user ->
  conv ->
  target ->
  String ->
  App Response
updateConversationMember user conv target role = do
  (convDomain, convId) <- objQid conv
  (targetDomain, targetId) <- objQid target
  req <- baseRequest user Galley Versioned (joinHttpPath ["conversations", convDomain, convId, "members", targetDomain, targetId])
  submit "PUT" (req & addJSONObject ["conversation_role" .= role])

deleteTeamConv ::
  (HasCallStack, MakesValue team, MakesValue conv, MakesValue user) =>
  team ->
  conv ->
  user ->
  App Response
deleteTeamConv team conv user = do
  teamId <- objId team
  convId <- objId $ objQidObject conv
  req <- baseRequest user Galley Versioned (joinHttpPath ["teams", teamId, "conversations", convId])
  submit "DELETE" req

getMLSOne2OneConversationLegacy ::
  (HasCallStack, MakesValue self, MakesValue other) =>
  self ->
  other ->
  App Response
getMLSOne2OneConversationLegacy self other = do
  (domain, uid) <- objQid other
  req <-
    baseRequest self Galley Versioned
      $ joinHttpPath ["conversations", "one2one", domain, uid]
  submit "GET" req

getMLSOne2OneConversation ::
  (HasCallStack, MakesValue self, MakesValue other) =>
  self ->
  other ->
  App Response
getMLSOne2OneConversation self other = do
  (domain, uid) <- objQid other
  req <-
    baseRequest self Galley Versioned
      $ joinHttpPath ["one2one-conversations", domain, uid]
  submit "GET" req

getGroupClients ::
  (HasCallStack, MakesValue user) =>
  user ->
  String ->
  App Response
getGroupClients user groupId = do
  req <-
    baseRequest
      user
      Galley
      Unversioned
      (joinHttpPath ["i", "group", BS.unpack . B64U.encodeUnpadded . B64.decodeLenient $ BS.pack groupId])
  submit "GET" req

data AddMembers = AddMembers
  { users :: [Value],
    role :: Maybe String,
    version :: Maybe Int
  }

instance Default AddMembers where
  def = AddMembers {users = [], role = Nothing, version = Nothing}

addMembers ::
  (HasCallStack, MakesValue user, MakesValue conv) =>
  user ->
  conv ->
  AddMembers ->
  App Response
addMembers usr qcnv opts = do
  (convDomain, convId) <- objQid qcnv
  qUsers <- mapM objQidObject opts.users
  let path = case opts.version of
        Just v | v <= 1 -> ["conversations", convId, "members", "v2"]
        _ -> ["conversations", convDomain, convId, "members"]
  req <-
    baseRequest
      usr
      Galley
      (maybe Versioned ExplicitVersion opts.version)
      (joinHttpPath path)
  submit "POST"
    $ req
    & addJSONObject
      ( ["qualified_users" .= qUsers]
          <> ["conversation_role" .= r | r <- toList opts.role]
      )

removeMember :: (HasCallStack, MakesValue remover, MakesValue conv, MakesValue removed) => remover -> conv -> removed -> App Response
removeMember remover qcnv removed = do
  (convDomain, convId) <- objQid qcnv
  (removedDomain, removedId) <- objQid removed
  req <- baseRequest remover Galley Versioned (joinHttpPath ["conversations", convDomain, convId, "members", removedDomain, removedId])
  submit "DELETE" req

postConversationCode ::
  (HasCallStack, MakesValue user, MakesValue conv) =>
  user ->
  conv ->
  Maybe String ->
  Maybe String ->
  App Response
postConversationCode user conv mbpassword mbZHost = do
  convId <- objId conv
  req <- baseRequest user Galley Versioned (joinHttpPath ["conversations", convId, "code"])
  submit
    "POST"
    ( req
        & addJSONObject ["password" .= pw | pw <- maybeToList mbpassword]
        & maybe id zHost mbZHost
    )

getConversationCode ::
  (HasCallStack, MakesValue user, MakesValue conv) =>
  user ->
  conv ->
  Maybe String ->
  App Response
getConversationCode user conv mbZHost = do
  convId <- objId conv
  req <- baseRequest user Galley Versioned (joinHttpPath ["conversations", convId, "code"])
  submit
    "GET"
    ( req
        & addQueryParams [("cnv", convId)]
        & maybe id zHost mbZHost
    )

getJoinCodeConv :: (HasCallStack, MakesValue user) => user -> String -> String -> App Response
getJoinCodeConv u k v = do
  req <- baseRequest u Galley Versioned (joinHttpPath ["conversations", "join"])
  submit "GET" (req & addQueryParams [("key", k), ("code", v)])

-- https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/put_conversations__cnv_domain___cnv__name
changeConversationName ::
  (HasCallStack, MakesValue user, MakesValue conv, MakesValue name) =>
  user ->
  conv ->
  name ->
  App Response
changeConversationName user qcnv name = do
  (convDomain, convId) <- objQid qcnv
  let path = joinHttpPath ["conversations", convDomain, convId, "name"]
  nameReq <- make name
  req <- baseRequest user Galley Versioned path
  submit "PUT" (req & addJSONObject ["name" .= nameReq])

updateRole ::
  ( HasCallStack,
    MakesValue callerUser,
    MakesValue targetUser,
    MakesValue roleUpdate,
    MakesValue qcnv
  ) =>
  callerUser ->
  targetUser ->
  roleUpdate ->
  qcnv ->
  App Response
updateRole caller target role qcnv = do
  (cnvDomain, cnvId) <- objQid qcnv
  (tarDomain, tarId) <- objQid target
  roleReq <- make role
  req <-
    baseRequest
      caller
      Galley
      Versioned
      ( joinHttpPath ["conversations", cnvDomain, cnvId, "members", tarDomain, tarId]
      )
  submit "PUT" (req & addJSONObject ["conversation_role" .= roleReq])

updateReceiptMode ::
  ( HasCallStack,
    MakesValue user,
    MakesValue conv,
    MakesValue mode
  ) =>
  user ->
  conv ->
  mode ->
  App Response
updateReceiptMode user qcnv mode = do
  (cnvDomain, cnvId) <- objQid qcnv
  modeReq <- make mode
  let path = joinHttpPath ["conversations", cnvDomain, cnvId, "receipt-mode"]
  req <- baseRequest user Galley Versioned path
  submit "PUT" (req & addJSONObject ["receipt_mode" .= modeReq])

updateAccess ::
  ( HasCallStack,
    MakesValue user,
    MakesValue conv
  ) =>
  user ->
  conv ->
  [Aeson.Pair] ->
  App Response
updateAccess user qcnv update = do
  (cnvDomain, cnvId) <- objQid qcnv
  let path = joinHttpPath ["conversations", cnvDomain, cnvId, "access"]
  req <- baseRequest user Galley Versioned path
  submit "PUT" (req & addJSONObject update)

updateMessageTimer ::
  ( HasCallStack,
    MakesValue user,
    MakesValue conv
  ) =>
  user ->
  conv ->
  Word64 ->
  App Response
updateMessageTimer user qcnv update = do
  (cnvDomain, cnvId) <- objQid qcnv
  updateReq <- make update
  let path = joinHttpPath ["conversations", cnvDomain, cnvId, "message-timer"]
  req <- baseRequest user Galley Versioned path
  submit "PUT" (addJSONObject ["message_timer" .= updateReq] req)

getTeam :: (HasCallStack, MakesValue user, MakesValue tid) => user -> tid -> App Response
getTeam user tid = do
  tidStr <- asString tid
  req <- baseRequest user Galley Versioned (joinHttpPath ["teams", tidStr])
  submit "GET" req

getTeamMembers :: (HasCallStack, MakesValue user, MakesValue tid) => user -> tid -> App Response
getTeamMembers user tid = do
  tidStr <- asString tid
  req <- baseRequest user Galley Versioned (joinHttpPath ["teams", tidStr, "members"])
  submit "GET" req

data AppLockSettings = AppLockSettings
  { status :: String,
    enforce :: Bool,
    inactivityTimeoutSecs :: Int
  }

instance Default AppLockSettings where
  def = AppLockSettings "disabled" False 60

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/put_teams__tid__features_appLock
putAppLockSettings ::
  (HasCallStack, MakesValue tid, MakesValue caller) =>
  tid ->
  caller ->
  AppLockSettings ->
  App Response
putAppLockSettings tid caller settings = do
  tidStr <- asString tid
  req <- baseRequest caller Galley Versioned (joinHttpPath ["teams", tidStr, "features", "appLock"])
  submit
    "PUT"
    ( addJSONObject
        [ "status" .= settings.status,
          "ttl" .= "unlimited",
          "config"
            .= object
              [ "enforceAppLock" .= settings.enforce,
                "inactivityTimeoutSecs" .= settings.inactivityTimeoutSecs
              ]
        ]
        req
    )

data TeamProperties = TeamProperties
  { icon :: String,
    iconKey :: String,
    name :: String,
    spashScreen :: String
  }

instance Default TeamProperties where
  def = TeamProperties "default" "default" "test" "default"

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/put_teams__tid_
putTeamProperties ::
  (HasCallStack, MakesValue tid, MakesValue caller) =>
  tid ->
  caller ->
  TeamProperties ->
  App Response
putTeamProperties tid caller properties = do
  tidStr <- asString tid
  req <- baseRequest caller Galley Versioned (joinHttpPath ["teams", tidStr])
  submit
    "PUT"
    ( addJSONObject
        [ "icon" .= properties.icon,
          "icon_key" .= properties.iconKey,
          "name" .= properties.name,
          "splash_screen" .= properties.spashScreen
        ]
        req
    )

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/get_teams__tid__legalhold__uid_
legalholdUserStatus :: (HasCallStack, MakesValue tid, MakesValue user, MakesValue owner) => tid -> owner -> user -> App Response
legalholdUserStatus tid ownerid user = do
  tidS <- asString tid
  uid <- objId user
  req <- baseRequest ownerid Galley Versioned (joinHttpPath ["teams", tidS, "legalhold", uid])
  submit "GET" req

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/delete_teams__tid__legalhold__uid_
disableLegalHold ::
  (HasCallStack, MakesValue tid, MakesValue ownerid, MakesValue uid) =>
  tid ->
  ownerid ->
  uid ->
  -- | the password for user with $uid$
  String ->
  App Response
disableLegalHold tid ownerid uid pw = do
  tidStr <- asString tid
  uidStr <- objId uid
  req <- baseRequest ownerid Galley Versioned (joinHttpPath ["teams", tidStr, "legalhold", uidStr])
  submit "DELETE" (addJSONObject ["password" .= pw] req)

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/post_teams__tid__legalhold_consent
consentToLegalHold :: (HasCallStack, MakesValue tid, MakesValue zusr) => tid -> zusr -> String -> App Response
consentToLegalHold tid zusr pwd = do
  tidStr <- asString tid
  req <- baseRequest zusr Galley Versioned (joinHttpPath ["teams", tidStr, "legalhold", "consent"])
  submit "POST" (addJSONObject ["password" .= pwd] req)

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/get_teams__tid__legalhold__uid_
getLegalHoldStatus :: (HasCallStack, MakesValue tid, MakesValue zusr) => tid -> zusr -> App Response
getLegalHoldStatus tid zusr = do
  tidStr <- asString tid
  uidStr <- asString $ zusr %. "id"
  req <- baseRequest zusr Galley Versioned (joinHttpPath ["teams", tidStr, "legalhold", uidStr])
  submit "GET" req

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/post_teams__tid__legalhold_settings
postLegalHoldSettings :: (HasCallStack, MakesValue ownerid, MakesValue tid, MakesValue newService) => tid -> ownerid -> newService -> App Response
postLegalHoldSettings tid owner newSettings =
  asks ((* 1_000_000) . timeOutSeconds) >>= \tSecs -> retrying (policy tSecs) only412 $ \_ -> do
    tidStr <- asString tid
    req <- baseRequest owner Galley Versioned (joinHttpPath ["teams", tidStr, "legalhold", "settings"])
    newSettingsObj <- make newSettings
    submit "POST" (addJSON newSettingsObj req)
  where
    policy :: Int -> RetryPolicy
    policy tSecs = limitRetriesByCumulativeDelay tSecs $ exponentialBackoff 50

    only412 :: RetryStatus -> Response -> App Bool
    only412 _ resp = pure $ resp.status == 412

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/post_teams__tid__legalhold__uid_
requestLegalHoldDevice :: (HasCallStack, MakesValue tid, MakesValue ownerid, MakesValue uid) => tid -> ownerid -> uid -> App Response
requestLegalHoldDevice tid ownerid uid = do
  tidStr <- asString tid
  uidStr <- objId uid
  req <- baseRequest ownerid Galley Versioned (joinHttpPath ["teams", tidStr, "legalhold", uidStr])
  submit "POST" req

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/put_teams__tid__legalhold__uid__approve
--
--   like approveLegalHoldDevice' but approves for the requesting party
approveLegalHoldDevice :: (HasCallStack, MakesValue tid, MakesValue uid) => tid -> uid -> String -> App Response
approveLegalHoldDevice tid uid = approveLegalHoldDevice' tid uid uid

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/put_teams__tid__legalhold__uid__approve
--
--   useful for testing unauthorized requests
approveLegalHoldDevice' :: (HasCallStack, MakesValue tid, MakesValue uid, MakesValue forUid) => tid -> uid -> forUid -> String -> App Response
approveLegalHoldDevice' tid uid forUid pwd = do
  tidStr <- asString tid
  uidStr <- asString $ forUid %. "id"
  req <- baseRequest uid Galley Versioned (joinHttpPath ["teams", tidStr, "legalhold", uidStr, "approve"])
  submit "PUT" (addJSONObject ["password" .= pwd] req)

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/put_teams__tid__features_legalhold
putLegalholdStatus ::
  (HasCallStack, MakesValue tid, MakesValue usr) =>
  tid ->
  usr ->
  -- | the status to put to
  String ->
  App Response
putLegalholdStatus tid usr status = do
  tidStr <- asString tid

  baseRequest usr Galley Versioned (joinHttpPath ["teams", tidStr, "features", "legalhold"])
    >>= submit "PUT"
    . addJSONObject ["status" .= status, "ttl" .= "unlimited"]

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/get_feature_configs
getFeatureConfigs :: (HasCallStack, MakesValue user) => user -> App Response
getFeatureConfigs user = do
  req <- baseRequest user Galley Versioned "/feature-configs"
  submit "GET" req

-- | https://staging-nginz-https.zinfra.io/v5/api/swagger-ui/#/default/get_teams__tid__features
getTeamFeatures :: (HasCallStack, MakesValue user, MakesValue tid) => user -> tid -> App Response
getTeamFeatures user tid = do
  tidStr <- asString tid
  req <- baseRequest user Galley Versioned (joinHttpPath ["teams", tidStr, "features"])
  submit "GET" req

getTeamFeature :: (HasCallStack, MakesValue user, MakesValue tid) => user -> tid -> String -> App Response
getTeamFeature user tid featureName = do
  tidStr <- asString tid
  req <- baseRequest user Galley Versioned (joinHttpPath ["teams", tidStr, "features", featureName])
  submit "GET" req

setTeamFeatureConfig :: (HasCallStack, MakesValue user, MakesValue team, MakesValue featureName, MakesValue payload) => user -> team -> featureName -> payload -> App Response
setTeamFeatureConfig = setTeamFeatureConfigVersioned Versioned

setTeamFeatureConfigVersioned :: (HasCallStack, MakesValue user, MakesValue team, MakesValue featureName, MakesValue payload) => Versioned -> user -> team -> featureName -> payload -> App Response
setTeamFeatureConfigVersioned versioned user team featureName payload = do
  tid <- asString team
  fn <- asString featureName
  p <- make payload
  req <- baseRequest user Galley versioned $ joinHttpPath ["teams", tid, "features", fn]
  submit "PUT" $ req & addJSON p

-- | http://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/get_feature_configs
getFeaturesForUser :: (HasCallStack, MakesValue user) => user -> App Response
getFeaturesForUser user = baseRequest user Galley Versioned "feature-configs" >>= submit "GET"

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/get_teams_notifications
getTeamNotifications :: (HasCallStack, MakesValue user) => user -> Maybe String -> App Response
getTeamNotifications user mSince =
  baseRequest user Galley Versioned "teams/notifications" >>= \req ->
    submit "GET"
      $ addQueryParams [("since", since) | since <- maybeToList mSince] req

-- | https://staging-nginz-https.zinfra.io/v6/api/swagger-ui/#/default/get_teams__tid__members_csv
getTeamMembersCsv :: (HasCallStack, MakesValue user) => user -> String -> App Response
getTeamMembersCsv user tid = do
  req <- baseRequest user Galley Versioned (joinHttpPath ["teams", tid, "members", "csv"])
  submit "GET" req
