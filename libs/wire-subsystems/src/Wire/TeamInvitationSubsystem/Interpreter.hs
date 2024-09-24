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
import Polysemy.Input
import Polysemy.TinyLog
import System.Logger.Message as Log
import URI.ByteString
import Util.Logging
import Wire.API.Team.Invitation
import Wire.API.Team.Member
import Wire.API.Team.Member qualified as Teams
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.User
import Wire.EmailSending (EmailSending)
import Wire.EmailSubsystem
import Wire.GalleyAPIAccess hiding (AddTeamMember)
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.InvitationCodeStore (InvitationCodeStore, StoredInvitation)
import Wire.InvitationCodeStore qualified as Store
import Wire.Sem.Logger qualified as Log
import Wire.Sem.Random (Random)
import Wire.Sem.Random qualified as Random
import Wire.TeamInvitationSubsystem
import Wire.UserKeyStore
import Wire.UserSubsystem (UserSubsystem, getLocalUserAccountByUserKey, getSelfProfile, isBlocked)

data TeamInvitationError
  = TeamInvitationNoEmail
  | TeamInvitationInsufficientTeamPermissions
  | TooManyTeamInvitations
  | TeamInvitationBlacklistedEmail
  | TeamInvitationEmailTaken

runTeamInvitationSubsystem :: (Member (Error TeamInvitationError) r) => InterpreterFor TeamInvitationSubsystem r
runTeamInvitationSubsystem = interpret $ \case
  InviteUser luid tid request -> inviteUserImpl luid tid request
  AcceptInvitation uid invitationId invitationCode -> acceptInvitationImpl uid invitationId invitationCode
  RevokeInvitation tid invitationId -> revokeInvitationImpl tid invitationId
  GetInvitationByCode invitationCode -> getInvitationByCodeImpl invitationCode
  GetInvitationByEmail email -> getInvitationByEmailImpl email
  CheckInvitationsByEmail email -> checkInvitationsByEmailImpl email
  DeleteAllInvitationsFor tid -> deleteAllInvitationsForImpl tid

inviteUserImpl :: (Member (Error TeamInvitationError) r, Member GalleyAPIAccess r, Member UserSubsystem r, Member TinyLog r) => Local UserId -> TeamId -> InvitationRequest -> Sem r (Invitation, InvitationLocation)
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
    Member UserKeyStore r,
    Member InvitationCodeStore r,
    Member EmailSending r,
    Member TinyLog r,
    Member (Error TeamInvitationError) r,
    Member Random r
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
    Just account ->
      if (account.accountStatus == Active && isNothing account.accountUser.userTeam)
        then pure True
        else throw TeamInvitationEmailTaken

  maxSize <- asks (.settings.maxTeamSize)
  pending <- Store.countInvitations tid
  when (fromIntegral pending >= maxSize) $
    throw TooManyTeamInvitations

  showInvitationUrl <- GalleyAPIAccess.getExposeInvitationURLsToTeamAdmin tid

  do
    iid <- maybe (Id <$> Random.uuid) pure mExpectedInvId
    now <- liftIO =<< asks (.currentTime)
    timeout <- asks (.settings.teamInvitationTimeout)
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

isPersonalUser :: (Member UserSubsystem r) => Local EmailKey -> Sem r Bool
isPersonalUser uke = do
  mAccount <- getLocalUserAccountByUserKey uke
  pure $ case mAccount of
    -- this can e.g. happen if the key is claimed but the account is not yet created
    Nothing -> False
    Just account ->
      account.accountStatus == Active
        && isNothing account.accountUser.userTeam

-- | brig used to not store the role, so for migration we allow this to be empty and fill in the
-- default here.
toInvitation ::
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

logInvitationRequest :: (Member TinyLog r) => (Msg -> Msg) -> Sem (Error TeamInvitationError : r) (Invitation, InvitationCode) -> Sem r (Invitation, InvitationCode)
logInvitationRequest context action =
  runError action >>= \case
    Left e -> do
      Log.warn $
        msg @String ("Failed to create invitation: " <> show err)
          . context
      throw e
    Right res@(_, code) -> do
      Log.info $
        msg @ByteString "Successfully created invitation"
          . context
          . logInvitationCode code
      pure res

-- flip mapExceptT action \action' -> do
--   eith <- action'
--   case eith of
--     Left err' -> do
--       liftSem $
--         Log.warn $
--           context
--             . Log.msg @Text
--               ( "Failed to create invitation, label: "
--                   <> (LT.toStrict . errorLabel) err'
--               )
--       pure (Left err')
--     Right result@(_, code) -> liftSem do
--       Log.info $ (context . logInvitationCode code) . Log.msg @Text "Successfully created invitation"
--       pure (Right result)

acceptInvitationImpl :: UserId -> InvitationId -> InvitationCode -> Sem r ()
acceptInvitationImpl = undefined

revokeInvitationImpl :: TeamId -> InvitationId -> Sem r ()
revokeInvitationImpl = undefined

getInvitationByCodeImpl :: InvitationCode -> Sem r Invitation
getInvitationByCodeImpl = undefined

getInvitationByEmailImpl :: EmailAddress -> Sem r Invitation
getInvitationByEmailImpl = undefined

checkInvitationsByEmailImpl :: EmailAddress -> Sem r HeadInvitationByEmailResult
checkInvitationsByEmailImpl = undefined

deleteAllInvitationsForImpl :: TeamId -> Sem r ()
deleteAllInvitationsForImpl = undefined

-- | Privilege escalation detection (make sure no `RoleMember` user creates a `RoleOwner`).
--
-- There is some code duplication with 'Galley.API.Teams.ensureNotElevated'.
ensurePermissionToAddUser ::
  ( Member GalleyAPIAccess r,
    Member (Error TeamInvitationError) r
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
