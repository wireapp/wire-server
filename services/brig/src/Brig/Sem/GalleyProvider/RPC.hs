{-# OPTIONS_GHC -Wno-unused-matches #-}

module Brig.Sem.GalleyProvider.RPC where


import Bilge hiding (head, options, requestId)
import Bilge.RPC
import Brig.API.Types
import Brig.App
import Brig.RPC
import Control.Lens ((^.))
import Control.Monad.Catch
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.Coerce (coerce)
import qualified Data.Currency as Currency
import Data.Id
import Data.Json.Util (UTCTimeMillis)
import Data.Range
import qualified Galley.Types.Teams as Team
import qualified Galley.Types.Teams.Intra as Team
import Imports
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import qualified Network.Wai.Utilities.Error as Wai
import System.Logger.Class as Log hiding (name, (.=))
import Wire.API.Conversation hiding (Member)
import Wire.API.Team
import qualified Wire.API.Team.Conversation as Conv
import Wire.API.Team.Feature
import qualified Wire.API.Team.Member as Member
import qualified Wire.API.Team.Member as Team
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import Brig.Sem.GalleyProvider (GalleyProvider(..))
import Brig.Sem.RPC
import Polysemy

interpretGalleyProviderToRPC :: Member RPC r => Request -> Sem (GalleyProvider ': r) a -> Sem r a
interpretGalleyProviderToRPC req = interpret $ \case
   CreateSelfConv id'                         -> runIt $ createSelfConv id'
   CreateLocalConnectConv qwt qwt' m_txt m_ci -> undefined -- createLocalConnectConv qwt qwt' m_txt m_ci
   AcceptLocalConnectConv qwt m_ci id'        -> undefined -- acceptLocalConnectConv qwt m_ci id'
   BlockLocalConv qwt m_ci id'                -> undefined -- blockLocalConv qwt m_ci id'
   UnblockLocalConv qwt m_ci id'              -> undefined -- unblockLocalConv qwt m_ci id'
   GetConv id' id''                           -> runIt $ getConv id' id''
   GetTeamConv id' id'' id'2                  -> runIt $ getTeamConv id' id'' id'2
   NewClient id' ci                           -> runIt $ newClient id' ci
   CheckUserCanJoinTeam id'                   -> runIt $ checkUserCanJoinTeam id'
   AddTeamMember id' id'' x0                  -> runIt $ addTeamMember id' id'' x0
   CreateTeam id' bnt id''                    -> runIt $ createTeam id' bnt id''
   GetTeamMember id' id''                     -> runIt $ getTeamMember id' id''
   GetTeamMembers id'                         -> runIt $ getTeamMembers id'
   GetTeamContacts id'                        -> undefined -- getTeamContacts id'
   GetTeamId id'                              -> runIt $ getTeamId id'
   GetTeam id'                                -> runIt $ getTeam id'
   GetTeamName id'                            -> runIt $ getTeamName id'
   GetTeamLegalHoldStatus id'                 -> runIt $ getTeamLegalHoldStatus id'
   GetTeamSearchVisibility id'                -> runIt $ getTeamSearchVisibility id'
   ChangeTeamStatus id' ts m_al               -> runIt $ changeTeamStatus id' ts m_al
   MemberIsTeamOwner id' id''                 -> runIt $ memberIsTeamOwner id' id''
   GetAllFeatureConfigsForUser m_id'          -> runIt $ getAllFeatureConfigsForUser m_id'
   GetVerificationCodeEnabled id'             -> runIt $ getVerificationCodeEnabled id'

runIt :: HttpClientIO a -> Sem r a
runIt = undefined

-- runIt = undefined

-- | Calls 'Galley.API.createSelfConversationH'.
createSelfConv ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  UserId ->
  m ()
createSelfConv u = do
  debug $
    remote "galley"
      . msg (val "Creating self conversation")
  void $ galleyRequest POST req
  where
    req =
      path "/conversations/self"
        . zUser u
        . expect2xx

-- | Calls 'Galley.API.getConversationH'.
getConv ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  UserId ->
  ConvId ->
  m (Maybe Conversation)
getConv usr cnv = do
  debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . msg (val "Getting conversation")
  rs <- galleyRequest GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBody "galley" rs
    _ -> pure Nothing
  where
    req =
      paths ["conversations", toByteString' cnv]
        . zUser usr
        . expect [status200, status404]

-- | Calls 'Galley.API.getTeamConversationH'.
getTeamConv ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  UserId ->
  TeamId ->
  ConvId ->
  m (Maybe Conv.TeamConversation)
getTeamConv usr tid cnv = do
  debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . msg (val "Getting team conversation")
  rs <- galleyRequest GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBody "galley" rs
    _ -> pure Nothing
  where
    req =
      paths ["teams", toByteString' tid, "conversations", toByteString' cnv]
        . zUser usr
        . expect [status200, status404]

-- | Calls 'Galley.API.addClientH'.
newClient ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  UserId ->
  ClientId ->
  m ()
newClient u c = do
  debug $
    remote "galley"
      . field "user" (toByteString u)
      . field "client" (toByteString c)
      . msg (val "new client")
  let p = paths ["i", "clients", toByteString' c]
  void $ galleyRequest POST (p . zUser u . expect2xx)

-- | Calls 'Galley.API.canUserJoinTeamH'.
checkUserCanJoinTeam ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  TeamId ->
  m (Maybe Wai.Error)
checkUserCanJoinTeam tid = do
  debug $
    remote "galley"
      . msg (val "Check if can add member to team")
  rs <- galleyRequest GET req
  pure $ case Bilge.statusCode rs of
    200 -> Nothing
    _ -> case decodeBody "galley" rs of
      Just (e :: Wai.Error) -> pure e
      Nothing -> error ("Invalid response from galley: " <> show rs)
  where
    req =
      paths ["i", "teams", toByteString' tid, "members", "check"]
        . header "Content-Type" "application/json"

-- | Calls 'Galley.API.uncheckedAddTeamMemberH'.
addTeamMember ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  UserId ->
  TeamId ->
  (Maybe (UserId, UTCTimeMillis), Role) ->
  m Bool
addTeamMember u tid (minvmeta, role) = do
  debug $
    remote "galley"
      . msg (val "Adding member to team")
  rs <- galleyRequest POST req
  pure $ case Bilge.statusCode rs of
    200 -> True
    _ -> False
  where
    prm = Team.rolePermissions role
    bdy = Member.mkNewTeamMember u prm minvmeta
    req =
      paths ["i", "teams", toByteString' tid, "members"]
        . header "Content-Type" "application/json"
        . zUser u
        . expect [status200, status403]
        . lbytes (encode bdy)

-- | Calls 'Galley.API.createBindingTeamH'.
createTeam ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  UserId ->
  BindingNewTeam ->
  TeamId ->
  m CreateUserTeam
createTeam u t@(BindingNewTeam bt) teamid = do
  debug $
    remote "galley"
      . msg (val "Creating Team")
  r <- galleyRequest PUT $ req teamid
  tid <-
    maybe (error "invalid team id") pure $
      fromByteString $
        getHeader' "Location" r
  pure (CreateUserTeam tid $ fromRange (bt ^. newTeamName))
  where
    req tid =
      paths ["i", "teams", toByteString' tid]
        . header "Content-Type" "application/json"
        . zUser u
        . expect2xx
        . lbytes (encode t)

-- | Calls 'Galley.API.uncheckedGetTeamMemberH'.
getTeamMember ::
  ( MonadLogger m,
    MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m
  ) =>
  UserId ->
  TeamId ->
  m (Maybe Team.TeamMember)
getTeamMember u tid = do
  debug $
    remote "galley"
      . msg (val "Get team member")
  rs <- galleyRequest GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBody "galley" rs
    _ -> pure Nothing
  where
    req =
      paths ["i", "teams", toByteString' tid, "members", toByteString' u]
        . zUser u
        . expect [status200, status404]

-- | Calls 'Galley.API.uncheckedGetTeamMembersH'.
--
-- | TODO: is now truncated.  this is (only) used for team suspension / unsuspension, which
-- means that only the first 2000 members of a team (according to some arbitrary order) will
-- be suspended, and the rest will remain active.
getTeamMembers ::
  ( MonadLogger m,
    MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m
  ) =>
  TeamId ->
  m Team.TeamMemberList
getTeamMembers tid = do
  debug $ remote "galley" . msg (val "Get team members")
  galleyRequest GET req >>= decodeBody "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid, "members"]
        . expect2xx

memberIsTeamOwner ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m
  ) =>
  TeamId ->
  UserId ->
  m Bool
memberIsTeamOwner tid uid = do
  r <-
    galleyRequest GET $
      paths ["i", "teams", toByteString' tid, "is-team-owner", toByteString' uid]
  pure $ responseStatus r /= status403

-- | Calls 'Galley.API.getBindingTeamIdH'.
getTeamId ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  UserId ->
  m (Maybe TeamId)
getTeamId u = do
  debug $ remote "galley" . msg (val "Get team from user")
  rs <- galleyRequest GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBody "galley" rs
    _ -> pure Nothing
  where
    req =
      paths ["i", "users", toByteString' u, "team"]
        . expect [status200, status404]

-- | Calls 'Galley.API.getTeamInternalH'.
getTeam ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  TeamId ->
  m Team.TeamData
getTeam tid = do
  debug $ remote "galley" . msg (val "Get team info")
  galleyRequest GET req >>= decodeBody "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid]
        . expect2xx

-- | Calls 'Galley.API.getTeamInternalH'.
getTeamName ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  TeamId ->
  m Team.TeamName
getTeamName tid = do
  debug $ remote "galley" . msg (val "Get team info")
  galleyRequest GET req >>= decodeBody "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid, "name"]
        . expect2xx

-- | Calls 'Galley.API.getTeamFeatureStatusH'.
getTeamLegalHoldStatus ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  TeamId ->
  m (WithStatus LegalholdConfig)
getTeamLegalHoldStatus tid = do
  debug $ remote "galley" . msg (val "Get legalhold settings")
  galleyRequest GET req >>= decodeBody "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid, "features", featureNameBS @LegalholdConfig]
        . expect2xx

-- | Calls 'Galley.API.getSearchVisibilityInternalH'.
getTeamSearchVisibility ::
  ( MonadLogger m,
    MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m
  ) =>
  TeamId ->
  m TeamSearchVisibility
getTeamSearchVisibility tid =
  coerce @TeamSearchVisibilityView @TeamSearchVisibility <$> do
    debug $ remote "galley" . msg (val "Get search visibility settings")
    galleyRequest GET req >>= decodeBody "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid, "search-visibility"]
        . expect2xx

getVerificationCodeEnabled ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  TeamId ->
  m Bool
getVerificationCodeEnabled tid = do
  debug $ remote "galley" . msg (val "Get snd factor password challenge settings")
  response <- galleyRequest GET req
  status <- wsStatus <$> decodeBody @(WithStatus SndFactorPasswordChallengeConfig) "galley" response
  case status of
    FeatureStatusEnabled -> pure True
    FeatureStatusDisabled -> pure False
  where
    req =
      paths ["i", "teams", toByteString' tid, "features", featureNameBS @SndFactorPasswordChallengeConfig]
        . expect2xx

getAllFeatureConfigsForUser ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m
  ) =>
  Maybe UserId ->
  m AllFeatureConfigs
getAllFeatureConfigsForUser mbUserId =
  responseJsonUnsafe
    <$> galleyRequest
      GET
      ( paths ["i", "feature-configs"]
          . maybe id (queryItem "user_id" . toByteString') mbUserId
      )

-- | Calls 'Galley.API.updateTeamStatusH'.
changeTeamStatus ::
  ( MonadReader Env m,
    MonadIO m,
    MonadMask m,
    MonadHttp m,
    HasRequestId m,
    MonadLogger m
  ) =>
  TeamId ->
  Team.TeamStatus ->
  Maybe Currency.Alpha ->
  m ()
changeTeamStatus tid s cur = do
  debug $ remote "galley" . msg (val "Change Team status")
  void $ galleyRequest PUT req
  where
    req =
      paths ["i", "teams", toByteString' tid, "status"]
        . header "Content-Type" "application/json"
        . expect2xx
        . lbytes (encode $ Team.TeamStatusUpdate s cur)
