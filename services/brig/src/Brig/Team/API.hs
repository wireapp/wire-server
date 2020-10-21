-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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
import Brig.App (currentTime, emailSender, settings)
import qualified Brig.Data.Blacklist as Blacklist
import Brig.Data.UserKey
import qualified Brig.Data.UserKey as Data
import qualified Brig.Email as Email
import qualified Brig.IO.Intra as Intra
import Brig.Options (setMaxTeamSize, setTeamInvitationTimeout)
import qualified Brig.Phone as Phone
import qualified Brig.Team.DB as DB
import Brig.Team.Email
import Brig.Team.Util (ensurePermissionToAddUser, ensurePermissions)
import Brig.Types.Intra (AccountStatus (..), NewUserScimInvitation (..), UserAccount (..))
import Brig.Types.Team (TeamSize)
import Brig.Types.Team.Invitation
import Brig.Types.User (Email, InvitationCode, emailIdentity)
import qualified Brig.User.Search.Index as ESIndex
import Control.Lens (view, (^.))
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.Id
import qualified Data.List1 as List1
import Data.Range
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
import qualified Wire.API.Team.Invitation as Public
import qualified Wire.API.Team.Role as Public
import qualified Wire.API.User as Public

routesPublic :: Routes Doc.ApiBuilder Handler ()
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
    Doc.errorResponse noIdentity
    Doc.errorResponse invalidEmail
    Doc.errorResponse blacklistedEmail
    Doc.errorResponse tooManyTeamInvitations

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
    Doc.errorResponse invalidInvitationCode

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

  get "/teams/invitations/by-email" (continue getInvitationByEmailH) $
    accept "application" "json"
      .&. query "email"

  -- TODO: check if this correct
  document "GET" "getInvitationByEmail" $ do
    Doc.summary "Get a pending invitation for a given an email address."
    Doc.parameter Doc.Query "email" Doc.bytes' $
      Doc.description "Email address"
    Doc.returns (Doc.ref Public.modelTeamInvitation)
    Doc.response 404 "No pending invitations exists." Doc.end
    Doc.response 409 "Multiple conflicting invitations to different teams exists." Doc.end

routesInternal :: Routes a Handler ()
routesInternal = do
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

teamSizeH :: JSON ::: TeamId -> Handler Response
teamSizeH (_ ::: t) = json <$> teamSize t

teamSize :: TeamId -> Handler TeamSize
teamSize t = lift $ ESIndex.teamSize t

getInvitationCodeH :: JSON ::: TeamId ::: InvitationId -> Handler Response
getInvitationCodeH (_ ::: t ::: r) = do
  json <$> getInvitationCode t r

getInvitationCode :: TeamId -> InvitationId -> Handler FoundInvitationCode
getInvitationCode t r = do
  code <- lift $ DB.lookupInvitationCode t r
  maybe (throwStd invalidInvitationCode) (return . FoundInvitationCode) code

data FoundInvitationCode = FoundInvitationCode InvitationCode
  deriving (Eq, Show, Generic)

instance ToJSON FoundInvitationCode where
  toJSON (FoundInvitationCode c) = object ["code" .= c]

createInvitationPublicH :: JSON ::: UserId ::: TeamId ::: JsonRequest Public.InvitationRequest -> Handler Response
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

createInvitationPublic :: UserId -> TeamId -> Public.InvitationRequest -> Handler Public.Invitation
createInvitationPublic uid tid body = do
  let inviteeRole = fromMaybe Team.defaultRole . irRole $ body
  inviter <- do
    let inviteePerms = Team.rolePermissions inviteeRole
    idt <- maybe (throwStd noIdentity) return =<< lift (fetchUserIdentity uid)
    from <- maybe (throwStd noEmail) return (emailIdentity idt)
    ensurePermissionToAddUser uid tid inviteePerms
    pure $ CreateInvitationInviter uid from

  createInvitation' tid inviteeRole (Just (inviterUid inviter)) (inviterEmail inviter) body

createInvitationViaScimH :: JSON ::: JsonRequest NewUserScimInvitation -> Handler Response
createInvitationViaScimH (_ ::: req) = do
  body <- parseJsonBody req
  setStatus status201 . json <$> createInvitationViaScim body

createInvitationViaScim :: NewUserScimInvitation -> Handler UserAccount
createInvitationViaScim newUser@(NewUserScimInvitation _ tid loc name email) = do
  env <- ask
  let inviteeRole = Team.defaultRole
      fromEmail = env ^. emailSender
      invreq =
        InvitationRequest
          { irLocale = loc,
            irRole = Nothing, -- (unused, it's in the type for 'createInvitationPublicH')
            irInviteeName = Just name,
            irInviteeEmail = email,
            irInviteePhone = Nothing
          }
  _ <- createInvitation' tid inviteeRole Nothing fromEmail invreq
  createUserInviteViaScim newUser

createInvitation' :: TeamId -> Public.Role -> Maybe UserId -> Email -> Public.InvitationRequest -> Handler Public.Invitation
createInvitation' tid inviteeRole mbInviterUid fromEmail body = do
  -- FUTUREWORK: These validations are nearly copy+paste from accountCreation and
  --             sendActivationCode. Refactor this to a single place

  -- Validate e-mail
  inviteeEmail <- either (const $ throwStd invalidEmail) return (Email.validateEmail (irInviteeEmail body))
  let uke = userEmailKey inviteeEmail
  blacklistedEm <- lift $ Blacklist.exists uke
  when blacklistedEm $
    throwStd blacklistedEmail
  emailTaken <- lift $ isJust <$> Data.lookupKey uke
  when emailTaken $
    throwStd emailExists

  -- Validate phone
  inviteePhone <- for (irInviteePhone body) $ \p -> do
    validatedPhone <- maybe (throwStd invalidPhone) return =<< lift (Phone.validatePhone p)
    let ukp = userPhoneKey validatedPhone
    blacklistedPh <- lift $ Blacklist.exists ukp
    when blacklistedPh $
      throwStd blacklistedPhone
    phoneTaken <- lift $ isJust <$> Data.lookupKey ukp
    when phoneTaken $
      throwStd phoneExists
    return validatedPhone
  maxSize <- setMaxTeamSize <$> view settings
  pending <- lift $ DB.countInvitations tid
  when (fromIntegral pending >= maxSize) $
    throwStd tooManyTeamInvitations

  let locale = irLocale body
  let inviteeName = irInviteeName body

  lift $ do
    iid <- liftIO DB.mkInvitationId
    now <- liftIO =<< view currentTime
    timeout <- setTeamInvitationTimeout <$> view settings
    (newInv, code) <-
      DB.insertInvitation
        iid
        tid
        inviteeRole
        now
        mbInviterUid
        inviteeEmail
        inviteeName
        inviteePhone
        timeout
    newInv <$ sendInvitationMail inviteeEmail tid fromEmail code locale

deleteInvitationH :: JSON ::: UserId ::: TeamId ::: InvitationId -> Handler Response
deleteInvitationH (_ ::: uid ::: tid ::: iid) = do
  empty <$ deleteInvitation uid tid iid

deleteInvitation :: UserId -> TeamId -> InvitationId -> Handler ()
deleteInvitation uid tid iid = do
  ensurePermissions uid tid [Team.AddTeamMember]
  DB.deleteInvitation tid iid

listInvitationsH :: JSON ::: UserId ::: TeamId ::: Maybe InvitationId ::: Range 1 500 Int32 -> Handler Response
listInvitationsH (_ ::: uid ::: tid ::: start ::: size) = do
  json <$> listInvitations uid tid start size

listInvitations :: UserId -> TeamId -> Maybe InvitationId -> Range 1 500 Int32 -> Handler Public.InvitationList
listInvitations uid tid start size = do
  ensurePermissions uid tid [Team.AddTeamMember]
  rs <- lift $ DB.lookupInvitations tid start size
  return $! Public.InvitationList (DB.resultList rs) (DB.resultHasMore rs)

getInvitationH :: JSON ::: UserId ::: TeamId ::: InvitationId -> Handler Response
getInvitationH (_ ::: uid ::: tid ::: iid) = do
  inv <- getInvitation uid tid iid
  return $ case inv of
    Just i -> json i
    Nothing -> setStatus status404 empty

getInvitation :: UserId -> TeamId -> InvitationId -> Handler (Maybe Public.Invitation)
getInvitation uid tid iid = do
  ensurePermissions uid tid [Team.AddTeamMember]
  lift $ DB.lookupInvitation tid iid

getInvitationByCodeH :: JSON ::: Public.InvitationCode -> Handler Response
getInvitationByCodeH (_ ::: c) = do
  json <$> getInvitationByCode c

getInvitationByCode :: Public.InvitationCode -> Handler Public.Invitation
getInvitationByCode c = do
  inv <- lift $ DB.lookupInvitationByCode c
  maybe (throwStd invalidInvitationCode) return inv

headInvitationByEmailH :: JSON ::: Email -> Handler Response
headInvitationByEmailH (_ ::: e) = do
  inv <- lift $ DB.lookupInvitationInfoByEmail e
  return $ case inv of
    DB.InvitationByEmail _ -> setStatus status200 empty
    DB.InvitationByEmailNotFound -> setStatus status404 empty
    DB.InvitationByEmailMoreThanOne -> setStatus status409 empty

getInvitationByEmailH :: JSON ::: Email -> Handler Response
getInvitationByEmailH (_ ::: email) =
  json <$> getInvitationByEmail email

getInvitationByEmail :: Email -> Handler Public.Invitation
getInvitationByEmail email = do
  inv <- lift $ DB.lookupInvitationByEmail email
  maybe (throwStd (notFound "Invitation not found")) return inv

suspendTeamH :: JSON ::: TeamId -> Handler Response
suspendTeamH (_ ::: tid) = do
  empty <$ suspendTeam tid

suspendTeam :: TeamId -> Handler ()
suspendTeam tid = do
  changeTeamAccountStatuses tid Suspended
  lift $ DB.deleteInvitations tid
  lift $ Intra.changeTeamStatus tid Team.Suspended Nothing

unsuspendTeamH :: JSON ::: TeamId -> Handler Response
unsuspendTeamH (_ ::: tid) = do
  empty <$ unsuspendTeam tid

unsuspendTeam :: TeamId -> Handler ()
unsuspendTeam tid = do
  changeTeamAccountStatuses tid Active
  lift $ Intra.changeTeamStatus tid Team.Active Nothing

-------------------------------------------------------------------------------
-- Internal

changeTeamAccountStatuses :: TeamId -> AccountStatus -> Handler ()
changeTeamAccountStatuses tid s = do
  team <- Team.tdTeam <$> (lift $ Intra.getTeam tid)
  unless (team ^. Team.teamBinding == Team.Binding) $
    throwStd noBindingTeam
  uids <- toList1 =<< lift (fmap (view Team.userId) . view Team.teamMembers <$> Intra.getTeamMembers tid)
  API.changeAccountStatus uids s !>> accountStatusError
  where
    toList1 (x : xs) = return $ List1.list1 x xs
    toList1 [] = throwStd (notFound "Team not found or no members")
