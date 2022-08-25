{-# OPTIONS_GHC -Wno-unused-matches #-}

module Brig.Sem.GalleyProvider.RPC where

import Bilge hiding (head, options, requestId)
import Brig.API.Types
import Brig.App
import Brig.RPC
import Brig.Sem.GalleyProvider (GalleyProvider (..))
import Brig.Sem.ServiceRPC (Service (Galley), ServiceRPC)
import qualified Brig.Sem.ServiceRPC as ServiceRPC
import Control.Error (hush)
import Control.Lens ((^.))
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as BL
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
import Polysemy
import Polysemy.Error
import System.Logger (Msg, field, msg, val)
import Wire.API.Conversation hiding (Member)
import Wire.API.Team
import qualified Wire.API.Team.Conversation as Conv
import Wire.API.Team.Feature
import qualified Wire.API.Team.Member as Member
import qualified Wire.API.Team.Member as Team
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import Wire.Sem.Logger

interpretGalleyProviderToRPC ::
  Members
    '[ Error ParseException,
       ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  Sem (GalleyProvider ': r) a ->
  Sem r a
interpretGalleyProviderToRPC = interpret $ \case
  CreateSelfConv id' -> createSelfConv id'
  GetConv id' id'' -> getConv id' id''
  GetTeamConv id' id'' id'2 -> getTeamConv id' id'' id'2
  NewClient id' ci -> newClient id' ci
  CheckUserCanJoinTeam id' -> checkUserCanJoinTeam id'
  AddTeamMember id' id'' x0 -> addTeamMember id' id'' x0
  CreateTeam id' bnt id'' -> createTeam id' bnt id''
  GetTeamMember id' id'' -> getTeamMember id' id''
  GetTeamMembers id' -> getTeamMembers id'
  GetTeamId id' -> getTeamId id'
  GetTeam id' -> getTeam id'
  GetTeamName id' -> getTeamName id'
  GetTeamLegalHoldStatus id' -> getTeamLegalHoldStatus id'
  GetTeamSearchVisibility id' -> getTeamSearchVisibility id'
  ChangeTeamStatus id' ts m_al -> changeTeamStatus id' ts m_al
  MemberIsTeamOwner id' id'' -> memberIsTeamOwner id' id''
  GetAllFeatureConfigsForUser m_id' -> getAllFeatureConfigsForUser m_id'
  GetVerificationCodeEnabled id' -> getVerificationCodeEnabled id'

runIt :: HttpClientIO a -> Sem r a
runIt = undefined

-- runIt = undefined

-- | Calls 'Galley.API.createSelfConversationH'.
createSelfConv ::
  Members
    '[ ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  UserId ->
  Sem r ()
createSelfConv u = do
  debug $
    remote "galley"
      . msg (val "Creating self conversation")
  void $ ServiceRPC.request @'Galley POST req
  where
    req =
      path "/conversations/self"
        . zUser u
        . expect2xx

-- | Calls 'Galley.API.getConversationH'.
getConv ::
  Members
    '[ Error ParseException,
       ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  UserId ->
  ConvId ->
  Sem r (Maybe Conversation)
getConv usr cnv = do
  debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . msg (val "Getting conversation")
  rs <- ServiceRPC.request @'Galley GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBodyOrThrow "galley" rs
    _ -> pure Nothing
  where
    req =
      paths ["conversations", toByteString' cnv]
        . zUser usr
        . expect [status200, status404]

-- | Calls 'Galley.API.getTeamConversationH'.
getTeamConv ::
  Members
    '[ Error ParseException,
       ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  UserId ->
  TeamId ->
  ConvId ->
  Sem r (Maybe Conv.TeamConversation)
getTeamConv usr tid cnv = do
  debug $
    remote "galley"
      . field "conv" (toByteString cnv)
      . msg (val "Getting team conversation")
  rs <- ServiceRPC.request @'Galley GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBodyOrThrow "galley" rs
    _ -> pure Nothing
  where
    req =
      paths ["teams", toByteString' tid, "conversations", toByteString' cnv]
        . zUser usr
        . expect [status200, status404]

-- | Calls 'Galley.API.addClientH'.
newClient ::
  Members
    '[ ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  UserId ->
  ClientId ->
  Sem r ()
newClient u c = do
  debug $
    remote "galley"
      . field "user" (toByteString u)
      . field "client" (toByteString c)
      . msg (val "new client")
  let p = paths ["i", "clients", toByteString' c]
  void $ ServiceRPC.request @'Galley POST (p . zUser u . expect2xx)

-- | Calls 'Galley.API.canUserJoinTeamH'.
checkUserCanJoinTeam ::
  Members
    '[ ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  TeamId ->
  Sem r (Maybe Wai.Error)
checkUserCanJoinTeam tid = do
  debug $
    remote "galley"
      . msg (val "Check if can add member to team")
  rs <- ServiceRPC.request @'Galley GET req
  pure $ case Bilge.statusCode rs of
    200 -> Nothing
    _ -> case decodeBodyMaybe "galley" rs of
      Just (e :: Wai.Error) -> pure e
      Nothing -> error ("Invalid response from galley: " <> show rs)
  where
    req =
      paths ["i", "teams", toByteString' tid, "members", "check"]
        . header "Content-Type" "application/json"

-- | Calls 'Galley.API.uncheckedAddTeamMemberH'.
addTeamMember ::
  Members
    '[ ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  UserId ->
  TeamId ->
  (Maybe (UserId, UTCTimeMillis), Role) ->
  Sem r Bool
addTeamMember u tid (minvmeta, role) = do
  debug $
    remote "galley"
      . msg (val "Adding member to team")
  rs <- ServiceRPC.request @'Galley POST req
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
  Members
    '[ ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  UserId ->
  BindingNewTeam ->
  TeamId ->
  Sem r CreateUserTeam
createTeam u t@(BindingNewTeam bt) teamid = do
  debug $
    remote "galley"
      . msg (val "Creating Team")
  r <- ServiceRPC.request @'Galley PUT $ req teamid
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
  Members
    '[ Error ParseException,
       ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  UserId ->
  TeamId ->
  Sem r (Maybe Team.TeamMember)
getTeamMember u tid = do
  debug $
    remote "galley"
      . msg (val "Get team member")
  rs <- ServiceRPC.request @'Galley GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBodyOrThrow "galley" rs
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
  Members
    '[ Error ParseException,
       ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  TeamId ->
  Sem r Team.TeamMemberList
getTeamMembers tid = do
  debug $ remote "galley" . msg (val "Get team members")
  ServiceRPC.request @'Galley GET req >>= decodeBodyOrThrow "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid, "members"]
        . expect2xx

memberIsTeamOwner ::
  Members
    '[ ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  TeamId ->
  UserId ->
  Sem r Bool
memberIsTeamOwner tid uid = do
  r <-
    ServiceRPC.request @'Galley GET $
      paths ["i", "teams", toByteString' tid, "is-team-owner", toByteString' uid]
  pure $ responseStatus r /= status403

-- | Calls 'Galley.API.getBindingTeamIdH'.
getTeamId ::
  Members
    '[ Error ParseException,
       ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  UserId ->
  Sem r (Maybe TeamId)
getTeamId u = do
  debug $ remote "galley" . msg (val "Get team from user")
  rs <- ServiceRPC.request @'Galley GET req
  case Bilge.statusCode rs of
    200 -> Just <$> decodeBodyOrThrow "galley" rs
    _ -> pure Nothing
  where
    req =
      paths ["i", "users", toByteString' u, "team"]
        . expect [status200, status404]

-- | Calls 'Galley.API.getTeamInternalH'.
getTeam ::
  Members
    '[ Error ParseException,
       ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  TeamId ->
  Sem r Team.TeamData
getTeam tid = do
  debug $ remote "galley" . msg (val "Get team info")
  ServiceRPC.request @'Galley GET req >>= decodeBodyOrThrow "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid]
        . expect2xx

-- | Calls 'Galley.API.getTeamInternalH'.
getTeamName ::
  Members
    '[ Error ParseException,
       ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  TeamId ->
  Sem r Team.TeamName
getTeamName tid = do
  debug $ remote "galley" . msg (val "Get team info")
  ServiceRPC.request @'Galley GET req >>= decodeBodyOrThrow "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid, "name"]
        . expect2xx

-- | Calls 'Galley.API.getTeamFeatureStatusH'.
getTeamLegalHoldStatus ::
  Members
    '[ Error ParseException,
       ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  TeamId ->
  Sem r (WithStatus LegalholdConfig)
getTeamLegalHoldStatus tid = do
  debug $ remote "galley" . msg (val "Get legalhold settings")
  ServiceRPC.request @'Galley GET req >>= decodeBodyOrThrow "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid, "features", featureNameBS @LegalholdConfig]
        . expect2xx

-- | Calls 'Galley.API.getSearchVisibilityInternalH'.
getTeamSearchVisibility ::
  Members
    '[ Error ParseException,
       ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  TeamId ->
  Sem r TeamSearchVisibility
getTeamSearchVisibility tid =
  coerce @TeamSearchVisibilityView @TeamSearchVisibility <$> do
    debug $ remote "galley" . msg (val "Get search visibility settings")
    ServiceRPC.request @'Galley GET req >>= decodeBodyOrThrow "galley"
  where
    req =
      paths ["i", "teams", toByteString' tid, "search-visibility"]
        . expect2xx

getVerificationCodeEnabled ::
  Members
    '[ Error ParseException,
       ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  TeamId ->
  Sem r Bool
getVerificationCodeEnabled tid = do
  debug $ remote "galley" . msg (val "Get snd factor password challenge settings")
  response <- ServiceRPC.request @'Galley GET req
  status <- wsStatus <$> decodeBodyOrThrow @(WithStatus SndFactorPasswordChallengeConfig) "galley" response
  case status of
    FeatureStatusEnabled -> pure True
    FeatureStatusDisabled -> pure False
  where
    req =
      paths ["i", "teams", toByteString' tid, "features", featureNameBS @SndFactorPasswordChallengeConfig]
        . expect2xx

decodeBodyOrThrow :: forall a r. (Typeable a, FromJSON a, Member (Error ParseException) r) => Text -> Response (Maybe BL.ByteString) -> Sem r a
decodeBodyOrThrow t r =
  case decodeBody @a t r of
    Left a ->
      case Imports.fromException a of
        Just pe -> throw @ParseException pe
        Nothing -> error "impossible: something other than ParseExceptionNothing was thrown by decodeBody"
    Right b -> pure b

decodeBodyMaybe :: (Typeable a, FromJSON a) => Text -> Response (Maybe BL.ByteString) -> Maybe a
decodeBodyMaybe t r = hush $ decodeBody t r

getAllFeatureConfigsForUser ::
  Members
    '[ ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  Maybe UserId ->
  Sem r AllFeatureConfigs
getAllFeatureConfigsForUser mbUserId =
  responseJsonUnsafe
    <$> ServiceRPC.request @'Galley
      GET
      ( paths ["i", "feature-configs"]
          . maybe id (queryItem "user_id" . toByteString') mbUserId
      )

-- | Calls 'Galley.API.updateTeamStatusH'.
changeTeamStatus ::
  Members
    '[ ServiceRPC 'Galley,
       Logger (Msg -> Msg)
     ]
    r =>
  TeamId ->
  Team.TeamStatus ->
  Maybe Currency.Alpha ->
  Sem r ()
changeTeamStatus tid s cur = do
  debug $ remote "galley" . msg (val "Change Team status")
  void $ ServiceRPC.request @'Galley PUT req
  where
    req =
      paths ["i", "teams", toByteString' tid, "status"]
        . header "Content-Type" "application/json"
        . expect2xx
        . lbytes (encode $ Team.TeamStatusUpdate s cur)
