module Wire.TeamInvitationSubsystem.Interpreter where

import Control.Arrow ((&&&))
import Control.Error (MaybeT (..))
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.Qualified
import Data.Set qualified as Set
import Data.Text.Ascii qualified as AsciiText
import Data.Text.Encoding qualified as Text
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input, input, runInputConst)
import Polysemy.TinyLog
import System.Logger.Message as Log
import URI.ByteString
import Util.Logging
import Util.Timeout (Timeout (..))
import Wire.API.Team.Invitation
import Wire.API.Team.Member
import Wire.API.Team.Member qualified as Teams
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.User
import Wire.Arbitrary
import Wire.EmailSubsystem
import Wire.GalleyAPIAccess hiding (AddTeamMember)
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.InvitationStore (InvitationStore, StoredInvitation)
import Wire.InvitationStore qualified as Store
import Wire.Sem.Logger qualified as Log
import Wire.Sem.Now (Now)
import Wire.Sem.Now qualified as Now
import Wire.Sem.Random (Random)
import Wire.Sem.Random qualified as Random
import Wire.TeamInvitationSubsystem
import Wire.TeamInvitationSubsystem.Error
import Wire.UserKeyStore
import Wire.UserSubsystem (UserSubsystem, getLocalUserAccountByUserKey, getSelfProfile, isBlocked)

data TeamInvitationSubsystemConfig = TeamInvitationSubsystemConfig
  { maxTeamSize :: Word32,
    teamInvitationTimeout :: Timeout
  }
  deriving (Show, Generic)
  deriving (Arbitrary) via GenericUniform TeamInvitationSubsystemConfig

runTeamInvitationSubsystem ::
  ( Member (Error TeamInvitationSubsystemError) r,
    Member TinyLog r,
    Member GalleyAPIAccess r,
    Member UserSubsystem r,
    Member Random r,
    Member InvitationStore r,
    Member Now r,
    Member EmailSubsystem r
  ) =>
  TeamInvitationSubsystemConfig ->
  InterpreterFor TeamInvitationSubsystem r
runTeamInvitationSubsystem cfg = interpret $ \case
  InviteUser luid tid request -> runInputConst cfg $ inviteUserImpl luid tid request
  InternalCreateInvitation tid mExpectedInvId role mbInviterUid inviterEmail invRequest ->
    runInputConst cfg $ createInvitation' tid mExpectedInvId role mbInviterUid inviterEmail invRequest

inviteUserImpl ::
  ( Member (Error TeamInvitationSubsystemError) r,
    Member GalleyAPIAccess r,
    Member UserSubsystem r,
    Member TinyLog r,
    Member Random r,
    Member InvitationStore r,
    Member (Input TeamInvitationSubsystemConfig) r,
    Member Now r,
    Member EmailSubsystem r
  ) =>
  Local UserId ->
  TeamId ->
  InvitationRequest ->
  Sem r (Invitation, InvitationLocation)
inviteUserImpl luid tid request = do
  let inviteeRole = fromMaybe defaultRole request.role

  let inviteePerms = Teams.rolePermissions inviteeRole
  ensurePermissionToAddUser (tUnqualified luid) tid inviteePerms

  inviterEmail <-
    note TeamInvitationNoEmail =<< runMaybeT do
      self <- MaybeT $ getSelfProfile luid
      MaybeT . pure . userEmail $ selfUser self

  let context =
        logFunction "Brig.Team.API.createInvitation"
          . logTeam tid
          . logEmail request.inviteeEmail

  (id &&& loc) . fst
    <$> logInvitationRequest
      context
      (createInvitation' tid Nothing inviteeRole (Just <$> luid) inviterEmail request)
  where
    loc :: Invitation -> InvitationLocation
    loc inv =
      InvitationLocation $ "/teams/" <> toByteString' tid <> "/invitations/" <> toByteString' inv.invitationId

createInvitation' ::
  ( Member GalleyAPIAccess r,
    Member UserSubsystem r,
    Member InvitationStore r,
    Member TinyLog r,
    Member (Error TeamInvitationSubsystemError) r,
    Member Random r,
    Member (Input TeamInvitationSubsystemConfig) r,
    Member Now r,
    Member EmailSubsystem r
  ) =>
  TeamId ->
  Maybe InvitationId ->
  Role ->
  Local (Maybe UserId) ->
  EmailAddress ->
  InvitationRequest ->
  Sem r (Invitation, InvitationCode)
createInvitation' tid mExpectedInvId inviteeRole mbInviterUid inviterEmail invRequest = do
  let email = invRequest.inviteeEmail
  let uke = qualifyAs mbInviterUid $ mkEmailKey email
  blacklistedEm <- isBlocked email
  when blacklistedEm $
    throw TeamInvitationBlacklistedEmail

  mEmailOwner <- getLocalUserAccountByUserKey uke
  isPersonalUserMigration <- case mEmailOwner of
    Nothing -> pure False
    Just user ->
      if (user.userStatus == Active && isNothing user.userTeam)
        then pure True
        else throw TeamInvitationEmailTaken

  maxSize <- maxTeamSize <$> input
  pending <- Store.countInvitations tid
  when (fromIntegral pending >= maxSize) $
    throw TooManyTeamInvitations

  showInvitationUrl <- GalleyAPIAccess.getExposeInvitationURLsToTeamAdmin tid

  do
    iid <- maybe (Id <$> Random.uuid) pure mExpectedInvId
    now <- Now.get
    timeout <- teamInvitationTimeout <$> input
    code <- mkInvitationCode
    newInv <-
      let insertInv =
            Store.MkInsertInvitation
              { invitationId = iid,
                teamId = tid,
                role = inviteeRole,
                createdAt = now,
                createdBy = tUnqualified mbInviterUid,
                inviteeEmail = email,
                inviteeName = invRequest.inviteeName,
                code = code
                -- mUrl = mUrl
              }
       in Store.insertInvitation insertInv timeout

    let sendOp =
          if isPersonalUserMigration
            then sendTeamInvitationMailPersonalUser
            else sendTeamInvitationMail

    invitationUrl <- sendOp email tid inviterEmail code invRequest.locale
    inv <- toInvitation invitationUrl showInvitationUrl newInv
    pure (inv, code)

mkInvitationCode :: (Member Random r) => Sem r InvitationCode
mkInvitationCode = InvitationCode . AsciiText.encodeBase64Url <$> Random.bytes 24

-- | brig used to not store the role, so for migration we allow this to be empty and fill in the
-- default here.
toInvitation ::
  forall r.
  (Member TinyLog r) =>
  Text ->
  ShowOrHideInvitationUrl ->
  StoredInvitation ->
  Sem r Invitation
toInvitation urlText showUrl storedInv = do
  url <-
    case showUrl of
      HideInvitationUrl -> pure Nothing
      ShowInvitationUrl -> parseHttpsUrl urlText
  pure $
    Invitation
      { team = storedInv.teamId,
        role = fromMaybe defaultRole storedInv.role,
        invitationId = storedInv.invitationId,
        createdAt = storedInv.createdAt,
        createdBy = storedInv.createdBy,
        inviteeEmail = storedInv.email,
        inviterEmail = Nothing,
        inviteeName = storedInv.name,
        inviteeUrl = url
      }
  where
    parseHttpsUrl :: Text -> Sem r (Maybe (URIRef Absolute))
    parseHttpsUrl url =
      either (\e -> Nothing <$ logError url e) (pure . Just) $
        parseURI laxURIParserOptions (Text.encodeUtf8 url)

    logError url e =
      Log.err $
        Log.msg @Text "Unable to create invitation url. Please check configuration."
          . Log.field "url" url
          . Log.field "error" (show e)

logInvitationRequest ::
  (Member TinyLog r, Member (Error TeamInvitationSubsystemError) r) =>
  (Msg -> Msg) ->
  Sem (Error TeamInvitationSubsystemError : r) (Invitation, InvitationCode) ->
  Sem r (Invitation, InvitationCode)
logInvitationRequest context action =
  runError action >>= \case
    Left e -> do
      Log.warn $
        msg @String ("Failed to create invitation: " <> show e)
          . context
      throw e
    Right res@(_, code) -> do
      Log.info $
        msg @ByteString "Successfully created invitation"
          . context
          . logInvitationCode code
      pure res

-- | Privilege escalation detection (make sure no `RoleMember` user creates a `RoleOwner`).
--
-- There is some code duplication with 'Galley.API.Teams.ensureNotElevated'.
ensurePermissionToAddUser ::
  ( Member GalleyAPIAccess r,
    Member (Error TeamInvitationSubsystemError) r
  ) =>
  UserId ->
  TeamId ->
  Permissions ->
  Sem r ()
ensurePermissionToAddUser u t inviteePerms = do
  minviter <- GalleyAPIAccess.getTeamMember u t
  unless (check minviter) $
    throw TeamInvitationInsufficientTeamPermissions
  where
    check :: Maybe TeamMember -> Bool
    check (Just inviter) =
      hasPermission inviter AddTeamMember
        && all (mayGrantPermission inviter) (Set.toList (inviteePerms.self))
    check Nothing = False

logInvitationCode :: InvitationCode -> (Msg -> Msg)
logInvitationCode code = field "invitation_code" (AsciiText.toText $ fromInvitationCode code)
