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
import Brig.App qualified as App
import Brig.Effects.ConnectionStore (ConnectionStore)
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import Brig.Options (setMaxTeamSize, setTeamInvitationTimeout)
import Brig.Team.Email
import Brig.Team.Template
import Brig.Team.Util (ensurePermissionToAddUser, ensurePermissions)
import Brig.Types.Team (TeamSize)
import Brig.User.Search.TeamSize qualified as TeamSize
import Control.Lens (view, (^.))
import Control.Monad.Trans.Except (mapExceptT)
import Data.ByteString.Conversion (toByteString, toByteString')
import Data.Id
import Data.List1 qualified as List1
import Data.Qualified (Local)
import Data.Range
import Data.Text.Ascii
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy qualified as Text
import Data.Time.Clock (UTCTime)
import Data.Tuple.Extra
import Imports hiding (head)
import Network.Wai.Utilities hiding (code, message)
import Polysemy
import Polysemy.Input (Input)
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import Servant hiding (Handler, JSON, addHeader)
import System.Logger.Message as Log
import URI.ByteString (Absolute, URIRef, laxURIParserOptions, parseURI)
import Util.Logging (logFunction, logTeam)
import Wire.API.Error
import Wire.API.Error.Brig qualified as E
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
import Wire.BlockListStore
import Wire.EmailSending (EmailSending)
import Wire.EmailSubsystem.Template
import Wire.Error
import Wire.GalleyAPIAccess (GalleyAPIAccess, ShowOrHideInvitationUrl (..))
import Wire.GalleyAPIAccess qualified as GalleyAPIAccess
import Wire.InvitationCodeStore (InsertInvitation (..), InvitationCodeStore (..), PaginatedResult (..), StoredInvitation (..))
import Wire.InvitationCodeStore qualified as Store
import Wire.NotificationSubsystem
import Wire.Sem.Concurrency
import Wire.Sem.Paging.Cassandra (InternalPaging)
import Wire.UserKeyStore
import Wire.UserSubsystem

servantAPI ::
  ( Member GalleyAPIAccess r,
    Member UserKeyStore r,
    Member UserSubsystem r,
    Member EmailSending r,
    Member TinyLog r,
    Member Store.InvitationCodeStore r
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

teamSizePublic :: (Member GalleyAPIAccess r) => UserId -> TeamId -> (Handler r) TeamSize
teamSizePublic uid tid = do
  ensurePermissions uid tid [AddTeamMember] -- limit this to team admins to reduce risk of involuntary DOS attacks
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
    Member UserSubsystem r,
    Member EmailSending r,
    Member TinyLog r,
    Member InvitationCodeStore r
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
      InvitationLocation $ "/teams/" <> toByteString' tid <> "/invitations/" <> toByteString' inv.invitationId

createInvitationViaScim ::
  ( Member BlockListStore r,
    Member GalleyAPIAccess r,
    Member UserKeyStore r,
    Member (UserPendingActivationStore p) r,
    Member TinyLog r,
    Member EmailSending r,
    Member UserSubsystem r,
    Member InvitationCodeStore r
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
  ( Member UserSubsystem r,
    Member GalleyAPIAccess r,
    Member UserKeyStore r,
    Member EmailSending r,
    Member TinyLog r,
    Member InvitationCodeStore r
  ) =>
  TeamId ->
  Maybe UserId ->
  Public.Role ->
  Maybe UserId ->
  EmailAddress ->
  Public.InvitationRequest ->
  Handler r (Public.Invitation, Public.InvitationCode)
createInvitation' tid mUid inviteeRole mbInviterUid fromEmail body = do
  let email = body.inviteeEmail
  let uke = mkEmailKey email
  blacklistedEm <- lift $ liftSem $ isBlocked email
  when blacklistedEm $
    throwStd blacklistedEmail
  emailTaken <- lift $ liftSem $ isJust <$> lookupKey uke
  when emailTaken $
    throwStd emailExists

  maxSize <- setMaxTeamSize <$> view settings
  pending <- lift $ liftSem $ Store.countInvitations tid
  when (fromIntegral pending >= maxSize) $
    throwStd (errorToWai @'E.TooManyTeamInvitations)

  showInvitationUrl <- lift $ liftSem $ GalleyAPIAccess.getExposeInvitationURLsToTeamAdmin tid

  iid <- maybe (liftIO randomId) (pure . Id . toUUID) mUid
  now <- liftIO =<< view currentTime
  timeout <- setTeamInvitationTimeout <$> view settings
  let insertInv =
        MkInsertInvitation
          { invitationId = iid,
            teamId = tid,
            role = inviteeRole,
            createdAt = now,
            createdBy = mbInviterUid,
            inviteeEmail = email,
            inviteeName = body.inviteeName
          }
  newInv <-
    lift . liftSem $
      Store.insertInvitation
        insertInv
        timeout
  lift $ sendInvitationMail email tid fromEmail newInv.code body.locale
  inv <- toInvitation showInvitationUrl newInv
  pure (inv, newInv.code)

deleteInvitation ::
  (Member GalleyAPIAccess r, Member InvitationCodeStore r) =>
  UserId ->
  TeamId ->
  InvitationId ->
  (Handler r) ()
deleteInvitation uid tid iid = do
  ensurePermissions uid tid [AddTeamMember]
  lift . liftSem $ Store.deleteInvitation tid iid

listInvitations ::
  ( Member GalleyAPIAccess r,
    Member TinyLog r,
    Member InvitationCodeStore r
  ) =>
  UserId ->
  TeamId ->
  Maybe InvitationId ->
  Maybe (Range 1 500 Int32) ->
  (Handler r) Public.InvitationList
listInvitations uid tid startingId mSize = do
  ensurePermissions uid tid [AddTeamMember]
  showInvitationUrl <- lift $ liftSem $ GalleyAPIAccess.getExposeInvitationURLsToTeamAdmin tid
  let toInvitations is = mapM (toInvitation showInvitationUrl) is
  lift (liftSem $ Store.lookupInvitationsPaginated mSize tid startingId) >>= \case
    PaginatedResultHasMore storedInvs -> do
      invs <- toInvitations storedInvs
      pure $ InvitationList invs True
    PaginatedResult storedInvs -> do
      invs <- toInvitations storedInvs
      pure $ InvitationList invs False

-- | brig used to not store the role, so for migration we allow this to be empty and fill in the
-- default here.
toInvitation ::
  ( Member TinyLog r
  ) =>
  ShowOrHideInvitationUrl ->
  StoredInvitation ->
  (Handler r) Invitation
toInvitation showUrl storedInv = do
  url <- mkInviteUrl showUrl storedInv.teamId storedInv.code
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

mkInviteUrl ::
  (Member TinyLog r) =>
  ShowOrHideInvitationUrl ->
  TeamId ->
  InvitationCode ->
  (Handler r) (Maybe (URIRef Absolute))
mkInviteUrl HideInvitationUrl _ _ = pure Nothing
mkInviteUrl ShowInvitationUrl team (InvitationCode c) = do
  template <- invitationEmailUrl . invitationEmail . snd <$> teamTemplates Nothing
  branding <- view App.templateBranding
  let url = Text.toStrict $ renderTextWithBranding template replace branding
  parseHttpsUrl url
  where
    replace "team" = idToText team
    replace "code" = toText c
    replace x = x
    parseHttpsUrl :: (Member TinyLog r) => Text -> (Handler r) (Maybe (URIRef Absolute))
    parseHttpsUrl url =
      either (\e -> lift . liftSem $ logError url e >> pure Nothing) (pure . Just) $
        parseURI laxURIParserOptions (encodeUtf8 url)
    logError url e =
      Log.err $
        Log.msg @Text "Unable to create invitation url. Please check configuration."
          . Log.field "url" url
          . Log.field "error" (show e)

getInvitation ::
  ( Member GalleyAPIAccess r,
    Member InvitationCodeStore r,
    Member TinyLog r
  ) =>
  UserId ->
  TeamId ->
  InvitationId ->
  (Handler r) (Maybe Public.Invitation)
getInvitation uid tid iid = do
  ensurePermissions uid tid [AddTeamMember]

  invitationM <- lift . liftSem $ Store.lookupInvitation tid iid
  case invitationM of
    Nothing -> pure Nothing
    Just invitation -> do
      showInvitationUrl <- lift . liftSem $ GalleyAPIAccess.getExposeInvitationURLsToTeamAdmin tid
      maybeUrl <- mkInviteUrl showInvitationUrl tid invitation.code
      pure $ Just (Store.invitationFromStored maybeUrl invitation)

getInvitationByCode ::
  (Member Store.InvitationCodeStore r) =>
  Public.InvitationCode ->
  (Handler r) Public.Invitation
getInvitationByCode c = do
  inv <- lift . liftSem $ Store.lookupInvitationByCode c
  maybe (throwStd $ errorToWai @'E.InvalidInvitationCode) (pure . Store.invitationFromStored Nothing) inv

headInvitationByEmail :: (Member InvitationCodeStore r, Member TinyLog r) => EmailAddress -> (Handler r) Public.HeadInvitationByEmailResult
headInvitationByEmail email =
  lift $
    liftSem $
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
    Member NotificationSubsystem r,
    Member (Concurrency 'Unsafe) r,
    Member GalleyAPIAccess r,
    Member TinyLog r,
    Member (Input (Local ())) r,
    Member (Input UTCTime) r,
    Member (ConnectionStore InternalPaging) r,
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
