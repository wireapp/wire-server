-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.GalleyAPIAccess.Rpc where

import Bilge hiding (head, options, requestId)
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as BL
import Data.Coerce (coerce)
import Data.Currency qualified as Currency
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Qualified
import Imports
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types qualified as HTTP
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai.Utilities.Error qualified as Wai
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog
import Servant.API (toHeader)
import System.Logger.Message
import Util.Options
import Wire.API.Conversation hiding (Member)
import Wire.API.Routes.Internal.Brig.EJPD (EJPDConvInfo)
import Wire.API.Routes.Internal.Galley.TeamsIntra qualified as Team
import Wire.API.Routes.Version
import Wire.API.Team
import Wire.API.Team.Conversation qualified as Conv
import Wire.API.Team.Feature
import Wire.API.Team.LegalHold
import Wire.API.Team.Member as Member
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import Wire.GalleyAPIAccess (GalleyAPIAccess (..), MLSOneToOneEstablished (..), ShowOrHideInvitationUrl (..))
import Wire.ParseException
import Wire.Rpc

interpretGalleyAPIAccessToRpc ::
  ( Member (Error ParseException) r,
    Member Rpc r,
    Member TinyLog r
  ) =>
  Set Version ->
  Endpoint ->
  Sem (GalleyAPIAccess ': r) a ->
  Sem r a
interpretGalleyAPIAccessToRpc disabledVersions galleyEndpoint =
  let v = fromMaybe (error "service can't run with undefined API version") $ maxAvailableVersion disabledVersions
   in interpret $
        runInputConst galleyEndpoint . \case
          CreateSelfConv id' -> createSelfConv v id'
          GetConv id' id'' -> getConv v id' id''
          GetTeamConv id' id'' id'2 -> getTeamConv v id' id'' id'2
          NewClient id' ci -> newClient id' ci
          CheckUserCanJoinTeam id' -> checkUserCanJoinTeam id'
          AddTeamMember id' id'' a b -> addTeamMember id' id'' a b
          CreateTeam id' bnt id'' -> createTeam id' bnt id''
          GetTeamMember id' id'' -> getTeamMember id' id''
          GetTeamMembers id' -> getTeamMembers id'
          GetTeamId id' -> getTeamId id'
          GetTeam id' -> getTeam id'
          GetTeamName id' -> getTeamName id'
          GetTeamLegalHoldStatus id' -> getTeamLegalHoldStatus id'
          GetTeamSearchVisibility id' -> getTeamSearchVisibility id'
          GetFeatureConfigForTeam tid -> getFeatureConfigForTeam tid
          GetUserLegalholdStatus id' tid -> getUserLegalholdStatus id' tid
          ChangeTeamStatus id' ts m_al -> changeTeamStatus id' ts m_al
          MemberIsTeamOwner id' id'' -> memberIsTeamOwner id' id''
          GetAllTeamFeaturesForUser m_id' -> getAllTeamFeaturesForUser m_id'
          GetVerificationCodeEnabled id' -> getVerificationCodeEnabled id'
          GetExposeInvitationURLsToTeamAdmin id' -> getTeamExposeInvitationURLsToTeamAdmin id'
          IsMLSOne2OneEstablished lusr qother -> checkMLSOne2OneEstablished lusr qother
          UnblockConversation lusr mconn qcnv -> unblockConversation v lusr mconn qcnv
          GetEJPDConvInfo uid -> getEJPDConvInfo uid

getUserLegalholdStatus ::
  ( Member TinyLog r,
    Member (Error ParseException) r,
    Member Rpc r
  ) =>
  Local UserId ->
  TeamId ->
  Sem (Input Endpoint : r) UserLegalHoldStatusResponse
getUserLegalholdStatus luid tid = do
  debug $
    remote "galley"
      . msg (val "get legalhold user status")
  decodeBodyOrThrow "galley" =<< galleyRequest do
    method GET
      . paths ["teams", toByteString' tid, "legalhold", toByteString' (tUnqualified luid)]
      . zUser (tUnqualified luid)
      . expect2xx

galleyRequest :: (Member Rpc r, Member (Input Endpoint) r) => (Request -> Request) -> Sem r (Response (Maybe LByteString))
galleyRequest req = do
  ep <- input
  rpcWithRetries "galley" ep req

-- | Calls 'Galley.API.createSelfConversationH'.
createSelfConv ::
  ( Member Rpc r,
    Member TinyLog r,
    Member (Input Endpoint) r
  ) =>
  Version ->
  UserId ->
  Sem r ()
createSelfConv v u = do
  debug $
    remote "galley"
      . msg (val "Creating self conversation")
  void $
    galleyRequest $
      method POST
        . paths [toHeader v, "conversations", "self"]
        . zUser u
        . expect2xx

-- | Calls 'Galley.API.getConversationH'.
getConv ::
  ( Member (Error ParseException) r,
    Member Rpc r,
    Member (Input Endpoint) r,
    Member TinyLog r
  ) =>
  Version ->
  UserId ->
  Local ConvId ->
  Sem r (Maybe Conversation)
getConv v usr lcnv = do
  debug $
    remote "galley"
      . field "domain" (toByteString (tDomain lcnv))
      . field "conv" (toByteString (tUnqualified lcnv))
      . msg (val "Getting conversation")
  rs <- galleyRequest req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBodyOrThrow "galley" rs
    _ -> pure Nothing
  where
    req =
      method GET
        . paths
          [ toHeader v,
            "conversations",
            toByteString' (tDomain lcnv),
            toByteString' (tUnqualified lcnv)
          ]
        . zUser usr
        . expect [status200, status404]

-- | Calls 'Galley.API.getTeamConversationH'.
getTeamConv ::
  ( Member (Error ParseException) r,
    Member Rpc r,
    Member (Input Endpoint) r,
    Member TinyLog r
  ) =>
  Version ->
  UserId ->
  TeamId ->
  ConvId ->
  Sem r (Maybe Conv.TeamConversation)
getTeamConv v usr tid cnv = do
  debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . msg (val "Getting team conversation")
  rs <- galleyRequest req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBodyOrThrow "galley" rs
    _ -> pure Nothing
  where
    req =
      method GET
        . paths
          [ toHeader v,
            "teams",
            toByteString' tid,
            "conversations",
            toByteString' cnv
          ]
        . zUser usr
        . expect [status200, status404]

-- | Calls 'Galley.API.addClientH'.
newClient ::
  ( Member Rpc r,
    Member (Input Endpoint) r,
    Member TinyLog r
  ) =>
  UserId ->
  ClientId ->
  Sem r ()
newClient u c = do
  debug $
    remote "galley"
      . field "user" (toByteString u)
      . field "client" (toByteString c)
      . msg (val "new client")
  void . galleyRequest $
    method POST
      . paths ["i", "clients", toByteString' c]
      . zUser u
      . expect2xx

-- | Calls 'Galley.API.canUserJoinTeamH'.
checkUserCanJoinTeam ::
  ( Member Rpc r,
    Member (Input Endpoint) r,
    Member TinyLog r
  ) =>
  TeamId ->
  Sem r (Maybe Wai.Error)
checkUserCanJoinTeam tid = do
  debug $
    remote "galley"
      . msg (val "Check if can add member to team")
  rs <- galleyRequest req
  pure $ case Bilge.statusCode rs of
    200 -> Nothing
    _ -> case responseJsonMaybe rs of
      Just (e :: Wai.Error) -> pure e
      Nothing -> error ("Invalid response from galley: " <> show rs)
  where
    req =
      method GET
        . paths ["i", "teams", toByteString' tid, "members", "check"]
        . header "Content-Type" "application/json"

-- | Calls 'Galley.API.uncheckedAddTeamMemberH'.
addTeamMember ::
  ( Member Rpc r,
    Member (Input Endpoint) r,
    Member TinyLog r
  ) =>
  UserId ->
  TeamId ->
  Maybe (UserId, UTCTimeMillis) ->
  Role ->
  Sem r Bool
addTeamMember u tid minvmeta role = do
  debug $
    remote "galley"
      . msg (val "Adding member to team")
  rs <- galleyRequest req
  pure $ case Bilge.statusCode rs of
    200 -> True
    _ -> False
  where
    prm = Member.rolePermissions role
    bdy = Member.mkNewTeamMember u prm minvmeta
    req =
      method POST
        . paths ["i", "teams", toByteString' tid, "members"]
        . header "Content-Type" "application/json"
        . zUser u
        . expect [status200, status403]
        . lbytes (encode bdy)

-- | Calls 'Galley.API.createBindingTeamH'.
createTeam ::
  ( Member Rpc r,
    Member (Input Endpoint) r,
    Member TinyLog r
  ) =>
  UserId ->
  NewTeam ->
  TeamId ->
  Sem r ()
createTeam u t teamid = do
  debug $
    remote "galley"
      . msg (val "Creating Team")
  void $ galleyRequest $ req teamid
  where
    req tid =
      method PUT
        . paths ["i", "teams", toByteString' tid]
        . header "Content-Type" "application/json"
        . zUser u
        . expect2xx
        . lbytes (encode t)

-- | Calls 'Galley.API.uncheckedGetTeamMemberH'.
getTeamMember ::
  ( Member (Error ParseException) r,
    Member Rpc r,
    Member (Input Endpoint) r,
    Member TinyLog r
  ) =>
  UserId ->
  TeamId ->
  Sem r (Maybe TeamMember)
getTeamMember u tid = do
  debug $
    remote "galley"
      . msg (val "Get team member")
  rs <- galleyRequest req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBodyOrThrow "galley" rs
    _ -> pure Nothing
  where
    req =
      method GET
        . paths ["i", "teams", toByteString' tid, "members", toByteString' u]
        . zUser u
        . expect [status200, status404]

-- | Calls 'Galley.API.uncheckedGetTeamMembersH'.
--
-- | TODO: is now truncated.  this is (only) used for team suspension / unsuspension, which
-- means that only the first 2000 members of a team (according to some arbitrary order) will
-- be suspended, and the rest will remain active.
getTeamMembers ::
  ( Member (Error ParseException) r,
    Member Rpc r,
    Member (Input Endpoint) r,
    Member TinyLog r
  ) =>
  TeamId ->
  Sem r TeamMemberList
getTeamMembers tid = do
  debug $ remote "galley" . msg (val "Get team members")
  galleyRequest req >>= decodeBodyOrThrow "galley"
  where
    req =
      method GET
        . paths ["i", "teams", toByteString' tid, "members"]
        . expect2xx

memberIsTeamOwner ::
  (Member Rpc r, Member (Input Endpoint) r) =>
  TeamId ->
  UserId ->
  Sem r Bool
memberIsTeamOwner tid uid = do
  r <-
    galleyRequest $
      method GET
        . paths ["i", "teams", toByteString' tid, "is-team-owner", toByteString' uid]
  pure $ responseStatus r /= status403

-- | Calls 'Galley.API.getBindingTeamIdH'.
getTeamId ::
  ( Member (Error ParseException) r,
    Member Rpc r,
    Member (Input Endpoint) r,
    Member TinyLog r
  ) =>
  UserId ->
  Sem r (Maybe TeamId)
getTeamId u = do
  debug $ remote "galley" . msg (val "Get team from user")
  rs <- galleyRequest req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBodyOrThrow "galley" rs
    _ -> pure Nothing
  where
    req =
      method GET
        . paths ["i", "users", toByteString' u, "team"]
        . expect [status200, status404]

-- | Calls 'Galley.API.getTeamInternalH'.
getTeam ::
  ( Member (Error ParseException) r,
    Member Rpc r,
    Member (Input Endpoint) r,
    Member TinyLog r
  ) =>
  TeamId ->
  Sem r Team.TeamData
getTeam tid = do
  debug $ remote "galley" . msg (val "Get team info")
  galleyRequest req >>= decodeBodyOrThrow "galley"
  where
    req =
      method GET
        . paths ["i", "teams", toByteString' tid]
        . expect2xx

-- | Calls 'Galley.API.getTeamInternalH'.
getTeamName ::
  ( Member (Error ParseException) r,
    Member Rpc r,
    Member (Input Endpoint) r,
    Member TinyLog r
  ) =>
  TeamId ->
  Sem r Team.TeamName
getTeamName tid = do
  debug $ remote "galley" . msg (val "Get team info")
  galleyRequest req >>= decodeBodyOrThrow "galley"
  where
    req =
      method GET
        . paths ["i", "teams", toByteString' tid, "name"]
        . expect2xx

-- | Calls 'Galley.API.getTeamFeatureStatusH'.
getTeamLegalHoldStatus ::
  ( Member (Error ParseException) r,
    Member Rpc r,
    Member (Input Endpoint) r,
    Member TinyLog r
  ) =>
  TeamId ->
  Sem r (LockableFeature LegalholdConfig)
getTeamLegalHoldStatus tid = do
  debug $ remote "galley" . msg (val "Get legalhold settings")
  galleyRequest req >>= decodeBodyOrThrow "galley"
  where
    req =
      method GET
        . paths ["i", "teams", toByteString' tid, "features", featureNameBS @LegalholdConfig]
        . expect2xx

-- | Calls 'Galley.API.getSearchVisibilityInternalH'.
getTeamSearchVisibility ::
  ( Member (Error ParseException) r,
    Member Rpc r,
    Member (Input Endpoint) r,
    Member TinyLog r
  ) =>
  TeamId ->
  Sem r TeamSearchVisibility
getTeamSearchVisibility tid =
  coerce @TeamSearchVisibilityView @TeamSearchVisibility <$> do
    debug $ remote "galley" . msg (val "Get search visibility settings")
    galleyRequest req >>= decodeBodyOrThrow "galley"
  where
    req =
      method GET
        . paths ["i", "teams", toByteString' tid, "search-visibility"]
        . expect2xx

getFeatureConfigForTeam ::
  forall feature r.
  ( IsFeatureConfig feature,
    Typeable feature,
    Member TinyLog r,
    Member Rpc r,
    Member (Error ParseException) r
  ) =>
  TeamId ->
  Sem (Input Endpoint : r) (LockableFeature feature)
getFeatureConfigForTeam tid = do
  debug $ remote "galley" . msg (val "Get feature config for team")
  galleyRequest req >>= decodeBodyOrThrow "galley"
  where
    req =
      method GET
        . paths ["i", "teams", toByteString' tid, "features", featureNameBS @feature]
        . expect2xx

getVerificationCodeEnabled ::
  ( Member (Error ParseException) r,
    Member Rpc r,
    Member (Input Endpoint) r,
    Member TinyLog r
  ) =>
  TeamId ->
  Sem r Bool
getVerificationCodeEnabled tid = do
  debug $ remote "galley" . msg (val "Get snd factor password challenge settings")
  response <- galleyRequest req
  status <- (.status) <$> decodeBodyOrThrow @(LockableFeature SndFactorPasswordChallengeConfig) "galley" response
  case status of
    FeatureStatusEnabled -> pure True
    FeatureStatusDisabled -> pure False
  where
    req =
      method GET
        . paths ["i", "teams", toByteString' tid, "features", featureNameBS @SndFactorPasswordChallengeConfig]
        . expect2xx

decodeBodyOrThrow :: forall a r. (Typeable a, FromJSON a, Member (Error ParseException) r) => Text -> Response (Maybe BL.ByteString) -> Sem r a
decodeBodyOrThrow ctx r = either (throw . ParseException ctx) pure (responseJsonEither r)

getAllTeamFeaturesForUser ::
  (Member Rpc r, Member (Input Endpoint) r) =>
  Maybe UserId ->
  Sem r AllTeamFeatures
getAllTeamFeaturesForUser mbUserId =
  responseJsonUnsafe
    <$> galleyRequest
      ( method GET
          . paths ["i", "feature-configs"]
          . maybe id (queryItem "user_id" . toByteString') mbUserId
      )

-- | Calls 'Galley.API.updateTeamStatusH'.
changeTeamStatus ::
  ( Member Rpc r,
    Member (Input Endpoint) r,
    Member TinyLog r
  ) =>
  TeamId ->
  Team.TeamStatus ->
  Maybe Currency.Alpha ->
  Sem r ()
changeTeamStatus tid s cur = do
  debug $ remote "galley" . msg (val "Change Team status")
  void $ galleyRequest req
  where
    req =
      method PUT
        . paths ["i", "teams", toByteString' tid, "status"]
        . header "Content-Type" "application/json"
        . expect2xx
        . lbytes (encode $ Team.TeamStatusUpdate s cur)

getTeamExposeInvitationURLsToTeamAdmin ::
  ( Member Rpc r,
    Member (Input Endpoint) r,
    Member (Error ParseException) r,
    Member TinyLog r
  ) =>
  TeamId ->
  Sem r ShowOrHideInvitationUrl
getTeamExposeInvitationURLsToTeamAdmin tid = do
  debug $ remote "galley" . msg (val "Get expose invitation URLs to team admin settings")
  response <- galleyRequest req
  status <- (.status) <$> decodeBodyOrThrow @(LockableFeature ExposeInvitationURLsToTeamAdminConfig) "galley" response
  case status of
    FeatureStatusEnabled -> pure ShowInvitationUrl
    FeatureStatusDisabled -> pure HideInvitationUrl
  where
    req =
      method GET
        . paths ["i", "teams", toByteString' tid, "features", featureNameBS @ExposeInvitationURLsToTeamAdminConfig]
        . expect2xx

checkMLSOne2OneEstablished ::
  ( Member (Error ParseException) r,
    Member (Input Endpoint) r,
    Member Rpc r,
    Member TinyLog r
  ) =>
  Local UserId ->
  Qualified UserId ->
  Sem r MLSOneToOneEstablished
checkMLSOne2OneEstablished self (Qualified other otherDomain) = do
  debug $ remote "galley" . msg (val "Get the MLS one-to-one conversation")
  responseSelf <- galleyRequest req
  case HTTP.statusCode (HTTP.responseStatus responseSelf) of
    200 -> do
      established <- decodeBodyOrThrow @Bool "galley" responseSelf
      pure $ if established then Established else NotEstablished
    403 -> pure NotAMember
    400 -> pure NotEstablished
    _ -> pure NotEstablished
  where
    req =
      method GET
        . paths
          [ "i",
            "conversations",
            "mls-one2one",
            toByteString' otherDomain,
            toByteString' other,
            "established"
          ]
        . zUser (tUnqualified self)

unblockConversation ::
  ( Member (Error ParseException) r,
    Member (Input Endpoint) r,
    Member Rpc r,
    Member TinyLog r
  ) =>
  Version ->
  Local UserId ->
  Maybe ConnId ->
  Qualified ConvId ->
  Sem r Conversation
unblockConversation v lusr mconn (Qualified cnv cdom) = do
  debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . field "domain" (toByteString cdom)
      . msg (val "Unblocking conversation")
  void $ galleyRequest putReq
  galleyRequest getReq >>= decodeBodyOrThrow @Conversation "galley"
  where
    putReq =
      method PUT
        . paths ["i", "conversations", toByteString' cdom, toByteString' cnv, "unblock"]
        . zUser (tUnqualified lusr)
        . maybe id (header "Z-Connection" . fromConnId) mconn
        . expect2xx
    getReq =
      method GET
        . paths [toHeader v, "conversations", toByteString' cdom, toByteString' cnv]
        . zUser (tUnqualified lusr)
        . expect2xx

remote :: ByteString -> Msg -> Msg
remote = field "remote"

getEJPDConvInfo ::
  forall r.
  ( Member TinyLog r,
    Member (Error ParseException) r,
    Member (Input Endpoint) r,
    Member Rpc r
  ) =>
  UserId ->
  Sem r [EJPDConvInfo]
getEJPDConvInfo uid = do
  debug $
    remote "galley"
      . msg (val "get conversation info for ejpd")
  decodeBodyOrThrow "galley" =<< galleyRequest getReq
  where
    getReq =
      method GET
        . paths ["i", "user", toByteString' uid, "all-conversations"]
