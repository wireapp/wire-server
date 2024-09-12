-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Brig.Team.API
  ( servantAPI,
    getInvitationByEmail,
    getInvitationCode,
    suspendTeam,
    unsuspendTeam,
    teamSize,
    createInvitationViaScim,
  )
where

import Brig.API.Error
import Brig.API.Handler
import Brig.API.User (createUserInviteViaScim, fetchUserIdentity)
import Brig.API.User qualified as API
import Brig.API.Util (logEmail, logInvitationCode)
import Brig.App
import Brig.Data.User as User
import Brig.Effects.ConnectionStore (ConnectionStore)
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import Brig.IO.Intra qualified as Intra
import Brig.Options (setMaxTeamSize, setTeamInvitationTimeout)
import Brig.Team.DB qualified as DB
import Brig.Team.Email
import Brig.Team.Util (ensurePermissionToAddUser, ensurePermissions)
import Brig.Types.Team (TeamSize)
import Brig.User.Search.TeamSize qualified as TeamSize
import Control.Lens (view, (^.))
import Control.Monad.Trans.Except (mapExceptT)
import Data.ByteString.Conversion (toByteString, toByteString')
import Data.Id
import Data.List1 qualified as List1
import Data.Qualified (Local, tUnqualified)
import Data.Range
import Data.Text.Lazy qualified as LT
import Data.Time.Clock (UTCTime)
import Data.Tuple.Extra
import Imports hiding (head)
import Network.Wai.Utilities hiding (code, message)
import Polysemy
import Polysemy.Input (Input)
import Polysemy.TinyLog (TinyLog)
import Servant hiding (Handler, JSON, addHeader)
import System.Logger.Class qualified as Log
import System.Logger.Message as Log
import Util.Logging (logFunction, logTeam)
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
import Wire.API.Password
import Wire.API.Routes.Internal.Brig (FoundInvitationCode (FoundInvitationCode))
import Wire.API.Routes.Internal.Galley.TeamsIntra qualified as Team
import Wire.API.Routes.Named
import Wire.API.Routes.Public.Brig (TeamsAPI)
import Wire.API.Team
import Wire.API.Team.Invitation
import Wire.API.Team.Invitation qualified as Public
import Wire.API.Team.Member (teamMembers)
import Wire.API.Team.Member qualified as Teams
import Wire.API.Team.Permission (Perm (AddTeamMember))
import Wire.API.Team.Role
import Wire.API.Team.Role qualified as Public
import Wire.API.User hiding (fromEmail)
import Wire.API.User qualified as Public
import Wire.API.UserEvent
import Wire.BlockListStore
import Wire.EmailSending (EmailSending)
import Wire.Error
import Wire.GalleyAPIAccess (GalleyAPIAccess, ShowOrHideInvitationUrl (..))
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.NotificationSubsystem
import Wire.PasswordStore
import Wire.Sem.Concurrency
import Wire.Sem.Paging.Cassandra (InternalPaging)
import Wire.UserKeyStore
import Wire.UserSubsystem

servantAPI ::
  ( Member GalleyAPIAccess r,
    Member UserKeyStore r,
    Member UserSubsystem r,
    Member EmailSending r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member PasswordStore r
  ) =>
  ServerT TeamsAPI (Handler r)
servantAPI =
  Named @"send-team-invitation" createInvitation
    :<|> Named @"get-team-invitations" listInvitations
    :<|> Named @"get-team-invitation" getInvitation
    :<|> Named @"delete-team-invitation" deleteInvitation
    :<|> Named @"get-team-invitation-info" getInvitationByCode
    :<|> Named @"head-team-invitations" headInvitationByEmail
    :<|> Named @"get-team-size" teamSizePublic
    :<|> Named @"accept-team-invitation" acceptTeamInvitationByPersonalUser

teamSizePublic :: (Member GalleyAPIAccess r) => UserId -> TeamId -> (Handler r) TeamSize
teamSizePublic uid tid = do
  ensurePermissions uid tid [AddTeamMember] -- limit this to team admins to reduce risk of involuntary DOS attacks
  teamSize tid

teamSize :: TeamId -> (Handler r) TeamSize
teamSize t = lift $ TeamSize.teamSize t

getInvitationCode :: TeamId -> InvitationId -> (Handler r) FoundInvitationCode
getInvitationCode t r = do
  code <- lift . wrapClient $ DB.lookupInvitationCode t r
  maybe (throwStd $ errorToWai @'E.InvalidInvitationCode) (pure . FoundInvitationCode) code

data CreateInvitationInviter = CreateInvitationInviter
  { inviterUid :: UserId,
    inviterEmail :: EmailAddress
  }
  deriving (Eq, Show)

createInvitation ::
  ( Member GalleyAPIAccess r,
    Member UserKeyStore r,
    Member UserSubsystem r,
    Member EmailSending r,
    Member (Input (Local ())) r
  ) =>
  UserId ->
  TeamId ->
  Public.InvitationRequest ->
  Handler r (Public.Invitation, Public.InvitationLocation)
createInvitation uid tid body = do
  let inviteeRole = fromMaybe defaultRole body.role
  inviter <- do
    let inviteePerms = Teams.rolePermissions inviteeRole
    idt <- maybe (throwStd (errorToWai @'E.NoIdentity)) pure =<< lift (fetchUserIdentity uid)
    from <- maybe (throwStd (errorToWai @'E.NoEmail)) pure (emailIdentity idt)
    ensurePermissionToAddUser uid tid inviteePerms
    pure $ CreateInvitationInviter uid from

  let context =
        logFunction "Brig.Team.API.createInvitation"
          . logTeam tid
          . logEmail body.inviteeEmail

  (id &&& loc) . fst
    <$> logInvitationRequest
      context
      (createInvitation' tid Nothing inviteeRole (Just (inviterUid inviter)) (inviterEmail inviter) body)
  where
    loc :: Invitation -> InvitationLocation
    loc inv =
      InvitationLocation $ "/teams/" <> toByteString' tid <> "/invitations/" <> toByteString' (inInvitation inv)

createInvitationViaScim ::
  ( Member BlockListStore r,
    Member GalleyAPIAccess r,
    Member UserKeyStore r,
    Member (UserPendingActivationStore p) r,
    Member TinyLog r,
    Member EmailSending r,
    Member UserSubsystem r,
    Member (Input (Local ())) r
  ) =>
  TeamId ->
  NewUserScimInvitation ->
  (Handler r) UserAccount
createInvitationViaScim tid newUser@(NewUserScimInvitation _tid uid _eid loc name email role) = do
  env <- ask
  let inviteeRole = role
      fromEmail = env ^. emailSender
      invreq =
        InvitationRequest
          { locale = loc,
            role = Nothing, -- (unused, it's in the type for 'createInvitationV5')
            inviteeName = Just name,
            inviteeEmail = email
          }

      context =
        logFunction "Brig.Team.API.createInvitationViaScim"
          . logTeam tid
          . logEmail email

  void $
    logInvitationRequest context $
      createInvitation' tid (Just uid) inviteeRole Nothing fromEmail invreq

  createUserInviteViaScim newUser

logInvitationRequest :: (Msg -> Msg) -> (Handler r) (Invitation, InvitationCode) -> (Handler r) (Invitation, InvitationCode)
logInvitationRequest context action =
  flip mapExceptT action $ \action' -> do
    eith <- action'
    case eith of
      Left err' -> do
        Log.warn $
          context
            . Log.msg @Text
              ( "Failed to create invitation, label: "
                  <> (LT.toStrict . errorLabel) err'
              )
        pure (Left err')
      Right result@(_, code) -> do
        Log.info $ (context . logInvitationCode code) . Log.msg @Text "Successfully created invitation"
        pure (Right result)

createInvitation' ::
  ( Member UserSubsystem r,
    Member GalleyAPIAccess r,
    Member UserKeyStore r,
    Member EmailSending r,
    Member (Input (Local ())) r
  ) =>
  TeamId ->
  Maybe UserId ->
  Public.Role ->
  Maybe UserId ->
  EmailAddress ->
  Public.InvitationRequest ->
  Handler r (Public.Invitation, Public.InvitationCode)
createInvitation' tid mUid inviteeRole mbInviterUid fromEmail invRequest = do
  let email = inviteeEmail invRequest
  let uke = mkEmailKey email
  blacklistedEm <- lift $ liftSem $ isBlocked email
  when blacklistedEm $
    throwStd blacklistedEmail
  emailTaken <- lift $ liftSem $ isJust <$> lookupKey uke
  isPersonalUserMigration <-
    if emailTaken
      then do
        mAccount <- lift $ liftSem $ getLocalUserAccountByUserKey =<< qualifyLocal' uke
        pure $ case mAccount of
          -- this can e.g. happen if the key is claimed but the account is not yet created
          Nothing -> False
          Just account ->
            account.accountStatus == Active
              && isNothing account.accountUser.userTeam
      else pure False

  when emailTaken $
    unless isPersonalUserMigration $
      throwStd emailExists

  maxSize <- setMaxTeamSize <$> view settings
  pending <- lift $ wrapClient $ DB.countInvitations tid
  when (fromIntegral pending >= maxSize) $
    throwStd (errorToWai @'E.TooManyTeamInvitations)

  showInvitationUrl <- lift $ liftSem $ GalleyAPIAccess.getExposeInvitationURLsToTeamAdmin tid

  lift $ do
    iid <- maybe (liftIO DB.mkInvitationId) (pure . Id . toUUID) mUid
    now <- liftIO =<< view currentTime
    timeout <- setTeamInvitationTimeout <$> view settings
    code <- liftIO $ DB.mkInvitationCode
    mUrl <-
      wrapClient $
        if isPersonalUserMigration
          then DB.mkInviteUrlPersonalUser showInvitationUrl tid code
          else DB.mkInviteUrl showInvitationUrl tid code
    newInv <-
      wrapClient $
        DB.insertInvitation
          code
          iid
          tid
          inviteeRole
          now
          mbInviterUid
          email
          invRequest.inviteeName
          timeout
          mUrl
    if isPersonalUserMigration
      then sendInvitationMailPersonalUser email tid fromEmail code invRequest.locale
      else sendInvitationMail email tid fromEmail code invRequest.locale
    pure (newInv, code)

deleteInvitation :: (Member GalleyAPIAccess r) => UserId -> TeamId -> InvitationId -> (Handler r) ()
deleteInvitation uid tid iid = do
  ensurePermissions uid tid [AddTeamMember]
  lift $ wrapClient $ DB.deleteInvitation tid iid

listInvitations :: (Member GalleyAPIAccess r) => UserId -> TeamId -> Maybe InvitationId -> Maybe (Range 1 500 Int32) -> (Handler r) Public.InvitationList
listInvitations uid tid start mSize = do
  ensurePermissions uid tid [AddTeamMember]
  showInvitationUrl <- lift $ liftSem $ GalleyAPIAccess.getExposeInvitationURLsToTeamAdmin tid
  rs <- lift $ wrapClient $ DB.lookupInvitations showInvitationUrl tid start (fromMaybe (unsafeRange 100) mSize)
  pure $! Public.InvitationList (DB.resultList rs) (DB.resultHasMore rs)

getInvitation :: (Member GalleyAPIAccess r) => UserId -> TeamId -> InvitationId -> (Handler r) (Maybe Public.Invitation)
getInvitation uid tid iid = do
  ensurePermissions uid tid [AddTeamMember]
  showInvitationUrl <- lift $ liftSem $ GalleyAPIAccess.getExposeInvitationURLsToTeamAdmin tid
  lift $ wrapClient $ DB.lookupInvitation showInvitationUrl tid iid

getInvitationByCode :: Public.InvitationCode -> (Handler r) Public.Invitation
getInvitationByCode c = do
  inv <- lift . wrapClient $ DB.lookupInvitationByCode HideInvitationUrl c
  maybe (throwStd $ errorToWai @'E.InvalidInvitationCode) pure inv

headInvitationByEmail :: EmailAddress -> (Handler r) Public.HeadInvitationByEmailResult
headInvitationByEmail e = do
  lift $
    wrapClient $
      DB.lookupInvitationInfoByEmail e <&> \case
        DB.InvitationByEmail _ -> Public.InvitationByEmail
        DB.InvitationByEmailNotFound -> Public.InvitationByEmailNotFound
        DB.InvitationByEmailMoreThanOne -> Public.InvitationByEmailMoreThanOne

-- | FUTUREWORK: This should also respond with status 409 in case of
-- @DB.InvitationByEmailMoreThanOne@.  Refactor so that 'headInvitationByEmailH' and
-- 'getInvitationByEmailH' are almost the same thing.
getInvitationByEmail :: EmailAddress -> (Handler r) Public.Invitation
getInvitationByEmail email = do
  inv <- lift $ wrapClient $ DB.lookupInvitationByEmail HideInvitationUrl email
  maybe (throwStd (notFound "Invitation not found")) pure inv

suspendTeam ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Concurrency 'Unsafe) r,
    Member GalleyAPIAccess r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  TeamId ->
  (Handler r) NoContent
suspendTeam tid = do
  Log.info $ Log.msg (Log.val "Team suspended") ~~ Log.field "team" (toByteString tid)
  changeTeamAccountStatuses tid Suspended
  lift $ wrapClient $ DB.deleteInvitations tid
  lift $ liftSem $ GalleyAPIAccess.changeTeamStatus tid Team.Suspended Nothing
  pure NoContent

unsuspendTeam ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Concurrency 'Unsafe) r,
    Member GalleyAPIAccess r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  TeamId ->
  (Handler r) NoContent
unsuspendTeam tid = do
  changeTeamAccountStatuses tid Active
  lift $ liftSem $ GalleyAPIAccess.changeTeamStatus tid Team.Active Nothing
  pure NoContent

-------------------------------------------------------------------------------
-- Internal

changeTeamAccountStatuses ::
  ( Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member (Concurrency 'Unsafe) r,
    Member GalleyAPIAccess r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r
  ) =>
  TeamId ->
  AccountStatus ->
  (Handler r) ()
changeTeamAccountStatuses tid s = do
  team <- Team.tdTeam <$> lift (liftSem $ GalleyAPIAccess.getTeam tid)
  unless (team ^. teamBinding == Binding) $
    throwStd noBindingTeam
  uids <- toList1 =<< lift (fmap (view Teams.userId) . view teamMembers <$> liftSem (GalleyAPIAccess.getTeamMembers tid))
  API.changeAccountStatus uids s !>> accountStatusError
  where
    toList1 (x : xs) = pure $ List1.list1 x xs
    toList1 [] = throwStd (notFound "Team not found or no members")

acceptTeamInvitationByPersonalUser ::
  ( Member UserSubsystem r,
    Member GalleyAPIAccess r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r,
    Member TinyLog r,
    Member (Embed HttpClientIO) r,
    Member NotificationSubsystem r,
    Member PasswordStore r
  ) =>
  Local UserId ->
  AcceptTeamInvitation ->
  (Handler r) ()
acceptTeamInvitationByPersonalUser luid req = do
  mek <- do
    mSelfProfile <- lift $ liftSem $ getSelfProfile luid
    pure $ mkEmailKey <$> (userEmail . selfUser =<< mSelfProfile)
  checkPassword
  mInv <- API.findTeamInvitation mek req.code !>> toInvitationError
  case mInv of
    Nothing -> throwStd (errorToWai @'E.PendingInvitationNotFound)
    Just (inv, _, tid) -> do
      let minvmeta = ((,inCreatedAt inv) <$> inCreatedBy inv, inRole inv)
          uid = tUnqualified luid
      added <- lift $ liftSem $ GalleyAPIAccess.addTeamMember uid tid minvmeta
      unless added $ throwStd (errorToWai @'E.TooManyTeamMembers)
      lift $ do
        wrapClient $ User.updateUserTeam uid tid
        wrapClient $ DB.deleteInvitation inv.inTeam inv.inInvitation
        liftSem $ Intra.onUserEvent uid Nothing (teamUpdated uid tid)
  where
    checkPassword = do
      p <-
        lift (liftSem . lookupHashedPassword . tUnqualified $ luid)
          >>= maybe (throwStd (errorToWai @'E.MissingAuth)) pure
      unless (verifyPassword req.password p) $
        throwStd (errorToWai @'E.BadCredentials)
    toInvitationError :: RegisterError -> HttpError
    toInvitationError = \case
      RegisterErrorMissingIdentity -> StdError (errorToWai @'E.MissingIdentity)
      RegisterErrorInvalidActivationCodeWrongUser -> StdError (errorToWai @'E.InvalidActivationCodeWrongUser)
      RegisterErrorInvalidActivationCodeWrongCode -> StdError (errorToWai @'E.InvalidActivationCodeWrongCode)
      _ -> StdError (notFound "Something went wrong, while looking up the invitation")
