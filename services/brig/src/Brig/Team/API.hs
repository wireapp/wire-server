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
  ( routesPublic,
    routesInternal,
  )
where

import Brig.API.Error
import Brig.API.Handler
import Brig.API.User (createUserInviteViaScim, fetchUserIdentity)
import qualified Brig.API.User as API
import Brig.API.Util (logEmail, logInvitationCode)
import Brig.App
import Brig.Data.UserKey
import qualified Brig.Data.UserKey as Data
import Brig.Effects.BlacklistStore (BlacklistStore)
import qualified Brig.Effects.BlacklistStore as BlacklistStore
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import qualified Brig.Email as Email
import Brig.Options (setMaxTeamSize, setTeamInvitationTimeout)
import qualified Brig.Phone as Phone
import Brig.Effects.GalleyProvider (GalleyProvider)
import qualified Brig.Effects.GalleyProvider as GalleyProvider
import Brig.Effects.UserPendingActivationStore (UserPendingActivationStore)
import qualified Brig.Team.DB as DB
import Brig.Team.Email
import Brig.Team.Types (ShowOrHideInvitationUrl (..))
import Brig.Team.Util (ensurePermissionToAddUser, ensurePermissions)
import Brig.Types.Intra (AccountStatus (..), NewUserScimInvitation (..), UserAccount (..))
import Brig.Types.Team (TeamSize)
import qualified Brig.User.Search.TeamSize as TeamSize
import Control.Lens (view, (^.))
import Control.Monad.Trans.Except (mapExceptT)
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.Id
import qualified Data.List1 as List1
import Data.Range
import Data.String.Conversions (cs)
import qualified Data.Swagger.Build.Api as Doc
import qualified Galley.Types.Teams as Team
import qualified Galley.Types.Teams.Intra as Team
import Imports hiding (head)
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Predicate hiding (and, result, setStatus)
import Network.Wai.Routing
import Network.Wai.Utilities hiding (code, message)
import Network.Wai.Utilities.Swagger (document)
import qualified Network.Wai.Utilities.Swagger as Doc
import Polysemy (Member, Members)
import System.Logger (Msg)
import qualified System.Logger.Class as Log
import Util.Logging (logFunction, logTeam)
import Wire.API.Error
import qualified Wire.API.Error.Brig as E
import Wire.API.Team
import Wire.API.Team.Invitation
import qualified Wire.API.Team.Invitation as Public
import Wire.API.Team.Member (teamMembers)
import qualified Wire.API.Team.Member as Teams
import Wire.API.Team.Permission (Perm (AddTeamMember))
import Wire.API.Team.Role
import qualified Wire.API.Team.Role as Public
import qualified Wire.API.Team.Size as Public
import Wire.API.User hiding (fromEmail)
import qualified Wire.API.User as Public

routesPublic ::
  Members
    '[ BlacklistStore,
       GalleyProvider
     ]
    r =>
  Routes Doc.ApiBuilder (Handler r) ()
routesPublic = do
  post "/teams/:tid/invitations" (continue createInvitationPublicH) $
    accept "application" "json"
      .&. header "Z-User"
      .&. capture "tid"
      .&. jsonRequest @Public.InvitationRequest
  document "POST" "sendTeamInvitation" $ do
    Doc.summary "Create and send a new team invitation."
    Doc.notes
      "Invitations are sent by email. The maximum allowed number of \
      \pending team invitations is equal to the team size."
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      Doc.description "Team ID"
    Doc.body (Doc.ref Public.modelTeamInvitationRequest) $
      Doc.description "JSON body"
    Doc.returns (Doc.ref Public.modelTeamInvitation)
    Doc.response 201 "Invitation was created and sent." Doc.end
    Doc.errorResponse noEmail
    Doc.errorResponse (errorToWai @'E.NoIdentity)
    Doc.errorResponse (errorToWai @'E.InvalidEmail)
    Doc.errorResponse (errorToWai @'E.BlacklistedEmail)
    Doc.errorResponse (errorToWai @'E.TooManyTeamInvitations)

  get "/teams/:tid/invitations" (continue listInvitationsH) $
    accept "application" "json"
      .&. header "Z-User"
      .&. capture "tid"
      .&. opt (query "start")
      .&. def (unsafeRange 100) (query "size")
  document "GET" "listTeamInvitations" $ do
    Doc.summary "List the sent team invitations"
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      Doc.description "Team ID"
    Doc.parameter Doc.Query "start" Doc.string' $ do
      Doc.description "Invitation id to start from (ascending)."
      Doc.optional
    Doc.parameter Doc.Query "size" Doc.int32' $ do
      Doc.description "Number of results to return (default 100, max 500)."
      Doc.optional
    Doc.returns (Doc.ref Public.modelTeamInvitationList)
    Doc.response 200 "List of sent invitations" Doc.end

  get "/teams/:tid/invitations/:iid" (continue getInvitationH) $
    accept "application" "json"
      .&. header "Z-User"
      .&. capture "tid"
      .&. capture "iid"
  document "GET" "getInvitation" $ do
    Doc.summary "Get a pending team invitation by ID."
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      Doc.description "Team ID"
    Doc.parameter Doc.Path "id" Doc.bytes' $
      Doc.description "Team Invitation ID"
    Doc.returns (Doc.ref Public.modelTeamInvitation)
    Doc.response 200 "Invitation" Doc.end

  delete "/teams/:tid/invitations/:iid" (continue deleteInvitationH) $
    accept "application" "json"
      .&. header "Z-User"
      .&. capture "tid"
      .&. capture "iid"
  document "DELETE" "deleteInvitation" $ do
    Doc.summary "Delete a pending invitation by ID."
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      Doc.description "Team ID"
    Doc.parameter Doc.Path "iid" Doc.bytes' $
      Doc.description "Team Invitation ID"
    Doc.response 200 "Invitation deleted." Doc.end

  get "/teams/invitations/info" (continue getInvitationByCodeH) $
    accept "application" "json"
      .&. query "code"
  document "GET" "getInvitationInfo" $ do
    Doc.summary "Get invitation info given a code."
    Doc.parameter Doc.Query "code" Doc.bytes' $
      Doc.description "Invitation code"
    Doc.returns (Doc.ref Public.modelTeamInvitation)
    Doc.response 200 "Invitation successful." Doc.end
    Doc.errorResponse (errorToWai @'E.InvalidInvitationCode)

  -- FUTUREWORK: Add another endpoint to allow resending of invitation codes
  head "/teams/invitations/by-email" (continue headInvitationByEmailH) $
    accept "application" "json"
      .&. query "email"

  document "HEAD" "headInvitationPending" $ do
    Doc.summary "Check if there is an invitation pending given an email address."
    Doc.parameter Doc.Query "email" Doc.bytes' $
      Doc.description "Email address"
    Doc.response 200 "Pending invitation exists." Doc.end
    Doc.response 404 "No pending invitations exists." Doc.end
    Doc.response 409 "Multiple conflicting invitations to different teams exists." Doc.end

  get "/teams/:tid/size" (continue teamSizePublicH) $
    accept "application" "json"
      .&. header "Z-User"
      .&. capture "tid"

  document "GET" "teamSize" $ do
    Doc.summary
      "Returns the number of team members as an integer.  \
      \Can be out of sync by roughly the `refresh_interval` \
      \of the ES index."
    Doc.parameter Doc.Path "tid" Doc.bytes' $
      Doc.description "Team ID"
    Doc.returns (Doc.ref Public.modelTeamSize)
    Doc.response 200 "Invitation successful." Doc.end
    Doc.response 403 "No permission (not admin or owner of this team)." Doc.end

routesInternal ::
  Members
    '[ BlacklistStore,
       GalleyProvider,
       UserPendingActivationStore p
     ]
    r =>
  Routes a (Handler r) ()
routesInternal = do
  get "/i/teams/invitations/by-email" (continue getInvitationByEmailH) $
    accept "application" "json"
      .&. query "email"

  get "/i/teams/invitation-code" (continue getInvitationCodeH) $
    accept "application" "json"
      .&. param "team"
      .&. param "invitation_id"

  post "/i/teams/:tid/suspend" (continue suspendTeamH) $
    accept "application" "json"
      .&. capture "tid"

  post "/i/teams/:tid/unsuspend" (continue unsuspendTeamH) $
    accept "application" "json"
      .&. capture "tid"

  get "/i/teams/:tid/size" (continue teamSizeH) $
    accept "application" "json"
      .&. capture "tid"

  post "/i/teams/:tid/invitations" (continue createInvitationViaScimH) $
    accept "application" "json"
      .&. jsonRequest @NewUserScimInvitation

teamSizePublicH :: Members '[GalleyProvider] r => JSON ::: UserId ::: TeamId -> (Handler r) Response
teamSizePublicH (_ ::: uid ::: tid) = json <$> teamSizePublic uid tid

teamSizePublic :: Members '[GalleyProvider] r => UserId -> TeamId -> (Handler r) TeamSize
teamSizePublic uid tid = do
  ensurePermissions uid tid [AddTeamMember] -- limit this to team admins to reduce risk of involuntary DOS attacks
  teamSize tid

teamSizeH :: JSON ::: TeamId -> (Handler r) Response
teamSizeH (_ ::: t) = json <$> teamSize t

teamSize :: TeamId -> (Handler r) TeamSize
teamSize t = lift $ TeamSize.teamSize t

getInvitationCodeH :: JSON ::: TeamId ::: InvitationId -> (Handler r) Response
getInvitationCodeH (_ ::: t ::: r) = do
  json <$> getInvitationCode t r

getInvitationCode :: TeamId -> InvitationId -> (Handler r) FoundInvitationCode
getInvitationCode t r = do
  code <- lift . wrapClient $ DB.lookupInvitationCode t r
  maybe (throwStd $ errorToWai @'E.InvalidInvitationCode) (pure . FoundInvitationCode) code

newtype FoundInvitationCode = FoundInvitationCode InvitationCode
  deriving (Eq, Show, Generic)

instance ToJSON FoundInvitationCode where
  toJSON (FoundInvitationCode c) = object ["code" .= c]

createInvitationPublicH ::
  Members
    '[ BlacklistStore,
       GalleyProvider
     ]
    r =>
  JSON ::: UserId ::: TeamId ::: JsonRequest Public.InvitationRequest ->
  (Handler r) Response
createInvitationPublicH (_ ::: uid ::: tid ::: req) = do
  body <- parseJsonBody req
  newInv <- createInvitationPublic uid tid body
  pure . setStatus status201 . loc (inInvitation newInv) . json $ newInv
  where
    loc iid =
      addHeader "Location" $
        "/teams/" <> toByteString' tid <> "/invitations/" <> toByteString' iid

data CreateInvitationInviter = CreateInvitationInviter
  { inviterUid :: UserId,
    inviterEmail :: Email
  }
  deriving (Eq, Show)

createInvitationPublic ::
  Members
    '[ BlacklistStore,
       GalleyProvider
     ]
    r =>
  UserId ->
  TeamId ->
  Public.InvitationRequest ->
  Handler r Public.Invitation
createInvitationPublic uid tid body = do
  let inviteeRole = fromMaybe defaultRole . irRole $ body
  inviter <- do
    let inviteePerms = Team.rolePermissions inviteeRole
    idt <- maybe (throwStd (errorToWai @'E.NoIdentity)) pure =<< lift (fetchUserIdentity uid)
    from <- maybe (throwStd noEmail) pure (emailIdentity idt)
    ensurePermissionToAddUser uid tid inviteePerms
    pure $ CreateInvitationInviter uid from

  let context =
        logFunction "Brig.Team.API.createInvitationPublic"
          . logTeam tid
          . logEmail (irInviteeEmail body)

  fst
    <$> logInvitationRequest
      context
      (createInvitation' tid inviteeRole (Just (inviterUid inviter)) (inviterEmail inviter) body)

createInvitationViaScimH ::
  Members
    '[ BlacklistStore,
       UserPendingActivationStore p
     ]
    r =>
  JSON ::: JsonRequest NewUserScimInvitation ->
  (Handler r) Response
createInvitationViaScimH (_ ::: req) = do
  body <- parseJsonBody req
  setStatus status201 . json <$> createInvitationViaScim body

createInvitationViaScim ::
  Members
    '[ BlacklistStore,
       UserPendingActivationStore p
     ]
    r =>
  NewUserScimInvitation ->
  (Handler r) UserAccount
createInvitationViaScim newUser@(NewUserScimInvitation tid loc name email) = do
  env <- ask
  let inviteeRole = defaultRole
      fromEmail = env ^. emailSender
      invreq =
        InvitationRequest
          { irLocale = loc,
            irRole = Nothing, -- (unused, it's in the type for 'createInvitationPublicH')
            irInviteeName = Just name,
            irInviteeEmail = email,
            irInviteePhone = Nothing
          }

      context =
        logFunction "Brig.Team.API.createInvitationViaScim"
          . logTeam tid
          . logEmail email

  (inv, _) <-
    logInvitationRequest context $
      createInvitation' tid inviteeRole Nothing fromEmail invreq
  let uid = Id (toUUID (inInvitation inv))

  createUserInviteViaScim uid newUser

logInvitationRequest :: (Msg -> Msg) -> (Handler r) (Invitation, InvitationCode) -> (Handler r) (Invitation, InvitationCode)
logInvitationRequest context action =
  flip mapExceptT action $ \action' -> do
    eith <- action'
    case eith of
      Left err' -> do
        Log.warn $ context . Log.msg @Text ("Failed to create invitation, label: " <> (cs . errorLabel) err')
        pure (Left err')
      Right result@(_, code) -> do
        Log.info $ (context . logInvitationCode code) . Log.msg @Text "Successfully created invitation"
        pure (Right result)

createInvitation' :: Member BlacklistStore r => TeamId -> Public.Role -> Maybe UserId -> Email -> Public.InvitationRequest -> Handler r (Public.Invitation, Public.InvitationCode)
createInvitation' tid inviteeRole mbInviterUid fromEmail body = do
  -- FUTUREWORK: These validations are nearly copy+paste from accountCreation and
  --             sendActivationCode. Refactor this to a single place

  -- Validate e-mail
  inviteeEmail <- either (const $ throwStd (errorToWai @'E.InvalidEmail)) pure (Email.validateEmail (irInviteeEmail body))
  let uke = userEmailKey inviteeEmail
  blacklistedEm <- lift $ liftSem $ BlacklistStore.exists uke
  when blacklistedEm $
    throwStd blacklistedEmail
  emailTaken <- lift $ isJust <$> wrapClient (Data.lookupKey uke)
  when emailTaken $
    throwStd emailExists

  -- Validate phone
  inviteePhone <- for (irInviteePhone body) $ \p -> do
    validatedPhone <- maybe (throwStd (errorToWai @'E.InvalidPhone)) pure =<< lift (wrapClient $ Phone.validatePhone p)
    let ukp = userPhoneKey validatedPhone
    blacklistedPh <- lift $ liftSem $ BlacklistStore.exists ukp
    when blacklistedPh $
      throwStd (errorToWai @'E.BlacklistedPhone)
    phoneTaken <- lift $ isJust <$> wrapClient (Data.lookupKey ukp)
    when phoneTaken $
      throwStd phoneExists
    pure validatedPhone
  maxSize <- setMaxTeamSize <$> view settings
  pending <- lift $ wrapClient $ DB.countInvitations tid
  when (fromIntegral pending >= maxSize) $
    throwStd (errorToWai @'E.TooManyTeamInvitations)

  let locale = irLocale body
  let inviteeName = irInviteeName body
  showInvitationUrl <- lift $ wrapHttp $ Intra.getTeamExposeInvitationURLsToTeamAdmin tid

  lift $ do
    iid <- liftIO DB.mkInvitationId
    now <- liftIO =<< view currentTime
    timeout <- setTeamInvitationTimeout <$> view settings
    (newInv, code) <-
      wrapClient $
        DB.insertInvitation
          showInvitationUrl
          iid
          tid
          inviteeRole
          now
          mbInviterUid
          inviteeEmail
          inviteeName
          inviteePhone
          timeout
    (newInv, code) <$ sendInvitationMail inviteeEmail tid fromEmail code locale

deleteInvitationH :: Members '[GalleyProvider] r => JSON ::: UserId ::: TeamId ::: InvitationId -> (Handler r) Response
deleteInvitationH (_ ::: uid ::: tid ::: iid) = do
  empty <$ deleteInvitation uid tid iid

deleteInvitation :: Members '[GalleyProvider] r => UserId -> TeamId -> InvitationId -> (Handler r) ()
deleteInvitation uid tid iid = do
  ensurePermissions uid tid [AddTeamMember]
  lift $ wrapClient $ DB.deleteInvitation tid iid

listInvitationsH :: Members '[GalleyProvider] r => JSON ::: UserId ::: TeamId ::: Maybe InvitationId ::: Range 1 500 Int32 -> (Handler r) Response
listInvitationsH (_ ::: uid ::: tid ::: start ::: size) = do
  json <$> listInvitations uid tid start size

listInvitations :: Members '[GalleyProvider] r => UserId -> TeamId -> Maybe InvitationId -> Range 1 500 Int32 -> (Handler r) Public.InvitationList
listInvitations uid tid start size = do
  ensurePermissions uid tid [AddTeamMember]
  showInvitationUrl <- lift $ wrapHttp $ Intra.getTeamExposeInvitationURLsToTeamAdmin tid
  rs <- lift $ wrapClient $ DB.lookupInvitations showInvitationUrl tid start size
  pure $! Public.InvitationList (DB.resultList rs) (DB.resultHasMore rs)

getInvitationH :: Members '[GalleyProvider] r => JSON ::: UserId ::: TeamId ::: InvitationId -> (Handler r) Response
getInvitationH (_ ::: uid ::: tid ::: iid) = do
  inv <- getInvitation uid tid iid
  pure $ case inv of
    Just i -> json i
    Nothing -> setStatus status404 empty

getInvitation :: Members '[GalleyProvider] r => UserId -> TeamId -> InvitationId -> (Handler r) (Maybe Public.Invitation)
getInvitation uid tid iid = do
  ensurePermissions uid tid [AddTeamMember]
  showInvitationUrl <- lift $ wrapHttp $ Intra.getTeamExposeInvitationURLsToTeamAdmin tid
  lift $ wrapClient $ DB.lookupInvitation showInvitationUrl tid iid

getInvitationByCodeH :: JSON ::: Public.InvitationCode -> (Handler r) Response
getInvitationByCodeH (_ ::: c) = do
  json <$> getInvitationByCode c

getInvitationByCode :: Public.InvitationCode -> (Handler r) Public.Invitation
getInvitationByCode c = do
  inv <- lift . wrapClient $ DB.lookupInvitationByCode HideInvitationUrl c
  maybe (throwStd $ errorToWai @'E.InvalidInvitationCode) pure inv

headInvitationByEmailH :: JSON ::: Email -> (Handler r) Response
headInvitationByEmailH (_ ::: e) = do
  inv <- lift $ wrapClient $ DB.lookupInvitationInfoByEmail e
  pure $ case inv of
    DB.InvitationByEmail _ -> setStatus status200 empty
    DB.InvitationByEmailNotFound -> setStatus status404 empty
    DB.InvitationByEmailMoreThanOne -> setStatus status409 empty

-- | FUTUREWORK: This should also respond with status 409 in case of
-- @DB.InvitationByEmailMoreThanOne@.  Refactor so that 'headInvitationByEmailH' and
-- 'getInvitationByEmailH' are almost the same thing.
getInvitationByEmailH :: JSON ::: Email -> (Handler r) Response
getInvitationByEmailH (_ ::: email) =
  json <$> getInvitationByEmail email

getInvitationByEmail :: Email -> (Handler r) Public.Invitation
getInvitationByEmail email = do
  inv <- lift $ wrapClient $ DB.lookupInvitationByEmail HideInvitationUrl email
  maybe (throwStd (notFound "Invitation not found")) pure inv

suspendTeamH :: Members '[GalleyProvider] r => JSON ::: TeamId -> (Handler r) Response
suspendTeamH (_ ::: tid) = do
  empty <$ suspendTeam tid

suspendTeam :: Members '[GalleyProvider] r => TeamId -> (Handler r) ()
suspendTeam tid = do
  changeTeamAccountStatuses tid Suspended
  lift $ wrapClient $ DB.deleteInvitations tid
  lift $ liftSem $ GalleyProvider.changeTeamStatus tid Team.Suspended Nothing

unsuspendTeamH ::
  Members '[GalleyProvider] r =>
  JSON ::: TeamId ->
  (Handler r) Response
unsuspendTeamH (_ ::: tid) = do
  empty <$ unsuspendTeam tid

unsuspendTeam ::
  Members '[GalleyProvider] r =>
  TeamId ->
  (Handler r) ()
unsuspendTeam tid = do
  changeTeamAccountStatuses tid Active
  lift $ liftSem $ GalleyProvider.changeTeamStatus tid Team.Active Nothing

-------------------------------------------------------------------------------
-- Internal

changeTeamAccountStatuses ::
  Members '[GalleyProvider] r =>
  TeamId ->
  AccountStatus ->
  (Handler r) ()
changeTeamAccountStatuses tid s = do
  team <- Team.tdTeam <$> lift (liftSem $ GalleyProvider.getTeam tid)
  unless (team ^. teamBinding == Binding) $
    throwStd noBindingTeam
  uids <- toList1 =<< lift (fmap (view Teams.userId) . view teamMembers <$> liftSem (GalleyProvider.getTeamMembers tid))
  wrapHttpClientE (API.changeAccountStatus uids s) !>> accountStatusError
  where
    toList1 (x : xs) = pure $ List1.list1 x xs
    toList1 [] = throwStd (notFound "Team not found or no members")
