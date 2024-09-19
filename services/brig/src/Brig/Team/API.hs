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
import Brig.App as App
import Brig.Data.User as User
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import Brig.Options
import Brig.Team.Email
import Brig.Team.Template
import Brig.Types.Team (TeamSize)
import Brig.User.Search.TeamSize qualified as TeamSize
import Control.Lens (view, (^.))
import Control.Monad.Trans.Except (mapExceptT)
import Data.ByteString.Conversion (toByteString, toByteString')
import Data.Id
import Data.List1 qualified as List1
import Data.Qualified (Local, tUnqualified)
import Data.Range
import Data.Text.Ascii
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy qualified as Text
import Data.Tuple.Extra
import Imports hiding (head)
import Network.Wai.Utilities hiding (Error, code, message)
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input, input)
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import Servant hiding (Handler, JSON, addHeader)
import System.Logger.Message as Log
import URI.ByteString (Absolute, URIRef, laxURIParserOptions, parseURI)
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
import Wire.EmailSubsystem.Template
import Wire.Error
import Wire.Events (Events)
import Wire.Events qualified as Events
import Wire.GalleyAPIAccess (GalleyAPIAccess, ShowOrHideInvitationUrl (..))
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.InvitationCodeStore (InvitationCodeStore (..), PaginatedResult (..), StoredInvitation (..))
import Wire.InvitationCodeStore qualified as Store
import Wire.InvitationCodeStore.Cassandra qualified as Store (mkInvitationCode)
import Wire.PasswordStore
import Wire.Sem.Concurrency
import Wire.UserKeyStore
import Wire.UserSubsystem
import Wire.UserSubsystem qualified as User
import Wire.UserSubsystem.Error

servantAPI ::
  ( Member GalleyAPIAccess r,
    Member UserKeyStore r,
    Member UserSubsystem r,
    Member Store.InvitationCodeStore r,
    Member EmailSending r,
    Member (Input (Local ())) r,
    Member TinyLog r,
    Member PasswordStore r,
    Member (Input TeamTemplates) r,
    Member Events r,
    Member (Error UserSubsystemError) r
  ) =>
  ServerT TeamsAPI (Handler r)
servantAPI =
  Named @"send-team-invitation" createInvitation
    :<|> Named @"get-team-invitations"
      (\u t inv s -> lift . liftSem $ listInvitations u t inv s)
    :<|> Named @"get-team-invitation"
      (\u t inv -> lift . liftSem $ getInvitation u t inv)
    :<|> Named @"delete-team-invitation"
      (\u t inv -> lift . liftSem $ deleteInvitation u t inv)
    :<|> Named @"get-team-invitation-info" getInvitationByCode
    :<|> Named @"head-team-invitations" (lift . liftSem . headInvitationByEmail)
    :<|> Named @"get-team-size" teamSizePublic
    :<|> Named @"accept-team-invitation" acceptTeamInvitationByPersonalUser

teamSizePublic ::
  ( Member GalleyAPIAccess r,
    Member (Error UserSubsystemError) r
  ) =>
  UserId ->
  TeamId ->
  (Handler r) TeamSize
teamSizePublic uid tid = do
  -- limit this to team admins to reduce risk of involuntary DOS attacks
  lift . liftSem $ ensurePermissions uid tid [AddTeamMember]
  teamSize tid

teamSize :: TeamId -> (Handler r) TeamSize
teamSize t = lift $ TeamSize.teamSize t

getInvitationCode ::
  (Member Store.InvitationCodeStore r) =>
  TeamId ->
  InvitationId ->
  (Handler r) FoundInvitationCode
getInvitationCode t r = do
  inv <- lift . liftSem $ Store.lookupInvitation t r
  maybe (throwStd $ errorToWai @'E.InvalidInvitationCode) (pure . FoundInvitationCode . (.code)) inv

data CreateInvitationInviter = CreateInvitationInviter
  { inviterUid :: UserId,
    inviterEmail :: EmailAddress
  }
  deriving (Eq, Show)

createInvitation ::
  ( Member GalleyAPIAccess r,
    Member UserKeyStore r,
    Member InvitationCodeStore r,
    Member UserSubsystem r,
    Member EmailSending r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input TeamTemplates) r,
    Member (Error UserSubsystemError) r
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
    lift . liftSem $ ensurePermissionToAddUser uid tid inviteePerms
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
      InvitationLocation $ "/teams/" <> toByteString' tid <> "/invitations/" <> toByteString' inv.invitationId

createInvitationViaScim ::
  ( Member GalleyAPIAccess r,
    Member BlockListStore r,
    Member UserKeyStore r,
    Member InvitationCodeStore r,
    Member (UserPendingActivationStore p) r,
    Member TinyLog r,
    Member UserSubsystem r,
    Member EmailSending r,
    Member (Input (Local ())) r,
    Member (Input TeamTemplates) r
  ) =>
  TeamId ->
  NewUserScimInvitation ->
  (Handler r) UserAccount
createInvitationViaScim tid newUser@(NewUserScimInvitation _tid uid _eid loc name email role) = do
  env <- ask
  let inviteeRole = role
      fromEmail = env ^. App.emailSender
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

logInvitationRequest :: (Member TinyLog r) => (Msg -> Msg) -> (Handler r) (Invitation, InvitationCode) -> Handler r (Invitation, InvitationCode)
logInvitationRequest context action =
  flip mapExceptT action \action' -> do
    eith <- action'
    case eith of
      Left err' -> do
        liftSem $
          Log.warn $
            context
              . Log.msg @Text
                ( "Failed to create invitation, label: "
                    <> (LT.toStrict . errorLabel) err'
                )
        pure (Left err')
      Right result@(_, code) -> liftSem do
        Log.info $ (context . logInvitationCode code) . Log.msg @Text "Successfully created invitation"
        pure (Right result)

createInvitation' ::
  ( Member GalleyAPIAccess r,
    Member UserSubsystem r,
    Member UserKeyStore r,
    Member InvitationCodeStore r,
    Member EmailSending r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input TeamTemplates) r
  ) =>
  TeamId ->
  Maybe UserId ->
  Public.Role ->
  Maybe UserId ->
  EmailAddress ->
  Public.InvitationRequest ->
  Handler r (Public.Invitation, Public.InvitationCode)
createInvitation' tid mUid inviteeRole mbInviterUid fromEmail invRequest = do
  let email = invRequest.inviteeEmail
  let uke = mkEmailKey email
  blacklistedEm <- lift $ liftSem $ isBlocked email
  when blacklistedEm $
    throwStd blacklistedEmail
  emailTaken <- lift $ liftSem $ isJust <$> lookupKey uke
  isPersonalUserMigration <-
    if emailTaken
      then lift $ liftSem $ isPersonalUser uke
      else pure False
  when emailTaken $
    unless isPersonalUserMigration $
      throwStd emailExists

  maxSize <- setMaxTeamSize <$> view settings
  pending <- lift $ liftSem $ Store.countInvitations tid
  when (fromIntegral pending >= maxSize) $
    throwStd (errorToWai @'E.TooManyTeamInvitations)

  showInvitationUrl <- lift $ liftSem $ GalleyAPIAccess.getExposeInvitationURLsToTeamAdmin tid

  lift $ do
    iid <- maybe randomId (pure . Id . toUUID) mUid
    now <- liftIO =<< view currentTime
    timeout <- setTeamInvitationTimeout <$> view settings
    code <- liftIO $ Store.mkInvitationCode
    newInv <-
      let insertInv =
            Store.MkInsertInvitation
              { invitationId = iid,
                teamId = tid,
                role = inviteeRole,
                createdAt = now,
                createdBy = mbInviterUid,
                inviteeEmail = email,
                inviteeName = invRequest.inviteeName,
                code = code
                -- mUrl = mUrl
              }
       in liftSem $ Store.insertInvitation insertInv timeout

    let sendOp =
          if isPersonalUserMigration
            then sendInvitationMailPersonalUser
            else sendInvitationMail

    sendOp email tid fromEmail code invRequest.locale
    inv <- liftSem $ toInvitation isPersonalUserMigration showInvitationUrl newInv
    pure (inv, code)

isPersonalUser :: (Member UserSubsystem r, Member (Input (Local ())) r) => EmailKey -> Sem r Bool
isPersonalUser uke = do
  mAccount <- getLocalUserAccountByUserKey =<< qualifyLocal' uke
  pure $ case mAccount of
    -- this can e.g. happen if the key is claimed but the account is not yet created
    Nothing -> False
    Just account ->
      account.accountStatus == Active
        && isNothing account.accountUser.userTeam

deleteInvitation ::
  ( Member GalleyAPIAccess r,
    Member InvitationCodeStore r,
    Member (Error UserSubsystemError) r
  ) =>
  UserId ->
  TeamId ->
  InvitationId ->
  Sem r ()
deleteInvitation uid tid iid = do
  ensurePermissions uid tid [AddTeamMember]
  Store.deleteInvitation tid iid

listInvitations ::
  forall r.
  ( Member GalleyAPIAccess r,
    Member TinyLog r,
    Member InvitationCodeStore r,
    Member (Input TeamTemplates) r,
    Member (Input (Local ())) r,
    Member UserSubsystem r,
    Member (Error UserSubsystemError) r
  ) =>
  UserId ->
  TeamId ->
  Maybe InvitationId ->
  Maybe (Range 1 500 Int32) ->
  Sem r Public.InvitationList
listInvitations uid tid startingId mSize = do
  ensurePermissions uid tid [AddTeamMember]
  showInvitationUrl <- GalleyAPIAccess.getExposeInvitationURLsToTeamAdmin tid
  let toInvitations is = mapM (toInvitationHack showInvitationUrl) is
  Store.lookupInvitationsPaginated mSize tid startingId >>= \case
    PaginatedResultHasMore storedInvs -> do
      invs <- toInvitations storedInvs
      pure $ InvitationList invs True
    PaginatedResult storedInvs -> do
      invs <- toInvitations storedInvs
      pure $ InvitationList invs False
  where
    -- To create the correct team invitation URL, we need to detect whether the invited account already exists.
    -- Optimization: if url is not to be shown, do not check for existing personal user.
    toInvitationHack :: ShowOrHideInvitationUrl -> StoredInvitation -> Sem r Invitation
    toInvitationHack HideInvitationUrl si = toInvitation False HideInvitationUrl si -- isPersonalUserMigration is always is ignored here
    toInvitationHack ShowInvitationUrl si = do
      isPersonalUserMigration <- isPersonalUser (mkEmailKey si.email)
      toInvitation isPersonalUserMigration ShowInvitationUrl si

-- | brig used to not store the role, so for migration we allow this to be empty and fill in the
-- default here.
toInvitation ::
  ( Member TinyLog r,
    Member (Input TeamTemplates) r
  ) =>
  Bool ->
  ShowOrHideInvitationUrl ->
  StoredInvitation ->
  Sem r Invitation
toInvitation isPersonalUserMigration showUrl storedInv = do
  url <-
    if isPersonalUserMigration
      then mkInviteUrlPersonalUser showUrl storedInv.teamId storedInv.code
      else mkInviteUrl showUrl storedInv.teamId storedInv.code
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

getInviteUrl ::
  forall r.
  (Member TinyLog r) =>
  InvitationEmailTemplate ->
  TeamId ->
  AsciiText Base64Url ->
  Sem r (Maybe (URIRef Absolute))
getInviteUrl (invitationEmailUrl -> template) team code = do
  let branding = id -- url is not branded
  let url = Text.toStrict $ renderTextWithBranding template replace branding
  parseHttpsUrl url
  where
    replace "team" = idToText team
    replace "code" = toText code
    replace x = x

    parseHttpsUrl :: Text -> Sem r (Maybe (URIRef Absolute))
    parseHttpsUrl url =
      either (\e -> Nothing <$ logError url e) (pure . Just) $
        parseURI laxURIParserOptions (encodeUtf8 url)

    logError url e =
      Log.err $
        Log.msg @Text "Unable to create invitation url. Please check configuration."
          . Log.field "url" url
          . Log.field "error" (show e)

mkInviteUrl ::
  ( Member TinyLog r,
    Member (Input TeamTemplates) r
  ) =>
  ShowOrHideInvitationUrl ->
  TeamId ->
  InvitationCode ->
  Sem r (Maybe (URIRef Absolute))
mkInviteUrl HideInvitationUrl _ _ = pure Nothing
mkInviteUrl ShowInvitationUrl team (InvitationCode c) = do
  template <- invitationEmail <$> input
  getInviteUrl template team c

mkInviteUrlPersonalUser ::
  ( Member TinyLog r,
    Member (Input TeamTemplates) r
  ) =>
  ShowOrHideInvitationUrl ->
  TeamId ->
  InvitationCode ->
  Sem r (Maybe (URIRef Absolute))
mkInviteUrlPersonalUser HideInvitationUrl _ _ = pure Nothing
mkInviteUrlPersonalUser ShowInvitationUrl team (InvitationCode c) = do
  template <- existingUserInvitationEmail <$> input
  getInviteUrl template team c

getInvitation ::
  ( Member GalleyAPIAccess r,
    Member InvitationCodeStore r,
    Member TinyLog r,
    Member (Input TeamTemplates) r,
    Member (Error UserSubsystemError) r
  ) =>
  UserId ->
  TeamId ->
  InvitationId ->
  Sem r (Maybe Public.Invitation)
getInvitation uid tid iid = do
  ensurePermissions uid tid [AddTeamMember]

  invitationM <- Store.lookupInvitation tid iid
  case invitationM of
    Nothing -> pure Nothing
    Just invitation -> do
      showInvitationUrl <- GalleyAPIAccess.getExposeInvitationURLsToTeamAdmin tid
      maybeUrl <- mkInviteUrl showInvitationUrl tid invitation.code
      pure $ Just (Store.invitationFromStored maybeUrl invitation)

getInvitationByCode ::
  (Member Store.InvitationCodeStore r) =>
  Public.InvitationCode ->
  (Handler r) Public.Invitation
getInvitationByCode c = do
  inv <- lift . liftSem $ Store.lookupInvitationByCode c
  maybe (throwStd $ errorToWai @'E.InvalidInvitationCode) (pure . Store.invitationFromStored Nothing) inv

headInvitationByEmail ::
  (Member InvitationCodeStore r, Member TinyLog r) =>
  EmailAddress ->
  Sem r Public.HeadInvitationByEmailResult
headInvitationByEmail email =
  Store.lookupInvitationCodesByEmail email >>= \case
    [] -> pure Public.InvitationByEmailNotFound
    [_code] -> pure Public.InvitationByEmail
    (_ : _ : _) -> do
      Log.info $
        Log.msg (Log.val "team_invidation_email: multiple pending invites from different teams for the same email")
          . Log.field "email" (show email)
      pure Public.InvitationByEmailMoreThanOne

-- | FUTUREWORK: This should also respond with status 409 in case of
-- @DB.InvitationByEmailMoreThanOne@.  Refactor so that 'headInvitationByEmailH' and
-- 'getInvitationByEmailH' are almost the same thing.
getInvitationByEmail ::
  (Member Store.InvitationCodeStore r, Member TinyLog r) =>
  EmailAddress ->
  (Handler r) Public.Invitation
getInvitationByEmail email = do
  inv <- lift . liftSem $ Store.lookupInvitationByEmail email
  maybe (throwStd (notFound "Invitation not found")) (pure . Store.invitationFromStored Nothing) inv

suspendTeam ::
  ( Member (Embed HttpClientIO) r,
    Member (Concurrency 'Unsafe) r,
    Member GalleyAPIAccess r,
    Member UserSubsystem r,
    Member Events r,
    Member TinyLog r,
    Member InvitationCodeStore r
  ) =>
  TeamId ->
  (Handler r) NoContent
suspendTeam tid = do
  lift $ liftSem $ Log.info $ Log.msg (Log.val "Team suspended") ~~ Log.field "team" (toByteString tid)
  -- Update the status of all users from the given team
  changeTeamAccountStatuses tid Suspended
  lift . liftSem $ do
    Store.deleteAllTeamInvitations tid
    -- RPC to galley to change team status there
    GalleyAPIAccess.changeTeamStatus tid Team.Suspended Nothing
  pure NoContent

unsuspendTeam ::
  ( Member (Embed HttpClientIO) r,
    Member (Concurrency 'Unsafe) r,
    Member GalleyAPIAccess r,
    Member UserSubsystem r,
    Member Events r
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
    Member (Concurrency 'Unsafe) r,
    Member GalleyAPIAccess r,
    Member UserSubsystem r,
    Member Events r
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
  forall r.
  ( Member UserSubsystem r,
    Member GalleyAPIAccess r,
    Member InvitationCodeStore r,
    Member PasswordStore r,
    Member Events r
  ) =>
  Local UserId ->
  AcceptTeamInvitation ->
  (Handler r) ()
acceptTeamInvitationByPersonalUser luid req = do
  (mek, mTid) <- do
    mSelfProfile <- lift $ liftSem $ getSelfProfile luid
    let mek = mkEmailKey <$> (userEmail . selfUser =<< mSelfProfile)
        mTid = mSelfProfile >>= userTeam . selfUser
    pure (mek, mTid)
  checkPassword
  (inv, (.teamId) -> tid) <- API.findTeamInvitation mek req.code !>> toInvitationError
  let minvmeta = (,inv.createdAt) <$> inv.createdBy
      uid = tUnqualified luid
  for_ mTid $ \userTid ->
    unless (tid == userTid) $
      throwStd (errorToWai @'E.CannotJoinMultipleTeams)
  added <- lift $ liftSem $ GalleyAPIAccess.addTeamMember uid tid minvmeta (fromMaybe defaultRole inv.role)
  unless added $ throwStd (errorToWai @'E.TooManyTeamMembers)
  lift $ do
    wrapClient $ User.updateUserTeam uid tid
    liftSem $ Store.deleteInvitation inv.teamId inv.invitationId
    liftSem $ User.internalUpdateSearchIndex uid
    liftSem $ Events.generateUserEvent uid Nothing (teamUpdated uid tid)
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
      RegisterErrorInvalidInvitationCode -> StdError (errorToWai @'E.InvalidInvitationCode)
      _ -> StdError (notFound "Something went wrong, while looking up the invitation")
