module Brig.Team.API where

import Imports
import Brig.App (currentTime, settings)
import Brig.API.Error
import Brig.API.Handler
import Brig.API.User (fetchUserIdentity)
import Brig.Data.UserKey (userEmailKey)
import Brig.Email
import Brig.Options (setMaxTeamSize, setTeamInvitationTimeout)
import Brig.Team.Email
import Brig.Team.Util (ensurePermissions, ensurePermissionToAddUser)
import Brig.Types.Team.Invitation
import Brig.Types.User (InvitationCode, emailIdentity)
import Brig.Types.Intra (AccountStatus (..))
import Control.Lens (view, (^.))
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.Id
import Data.Range
import Network.HTTP.Types.Status
import Network.Wai (Response)
import Network.Wai.Predicate hiding (setStatus, result, and)
import Network.Wai.Routing hiding (head)
import Network.Wai.Utilities hiding (message, code)
import Network.Wai.Utilities.Swagger (document)

import qualified Brig.API.User                 as API
import qualified Brig.Data.Blacklist           as Blacklist
import qualified Brig.Data.UserKey             as Data
import qualified Brig.Team.DB                  as DB
import qualified Brig.Types.Swagger            as Doc
import qualified Network.Wai.Utilities.Swagger as Doc
import qualified Data.List1                    as List1
import qualified Data.Swagger.Build.Api        as Doc
import qualified Brig.IO.Intra                 as Intra
import qualified Galley.Types.Teams            as Team
import qualified Galley.Types.Teams.Intra      as Team

routes :: Routes Doc.ApiBuilder Handler ()
routes = do

    post "/teams/:tid/invitations" (continue createInvitationH) $
        accept "application" "json"
        .&. header "Z-User"
        .&. capture "tid"
        .&. jsonRequest @InvitationRequest

    document "POST" "sendTeamInvitation" $ do
        Doc.summary "Create and send a new team invitation."
        Doc.notes "Invitations are sent by email. The maximum allowed number of \
                  \pending team invitations is equal to the team size."
        Doc.parameter Doc.Path "tid" Doc.bytes' $
            Doc.description "Team ID"
        Doc.body (Doc.ref Doc.teamInvitationRequest) $
            Doc.description "JSON body"
        Doc.returns (Doc.ref Doc.teamInvitation)
        Doc.response 201 "Invitation was created and sent." Doc.end
        Doc.errorResponse noEmail
        Doc.errorResponse noIdentity
        Doc.errorResponse invalidEmail
        Doc.errorResponse blacklistedEmail
        Doc.errorResponse tooManyTeamInvitations

    ---

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
        Doc.returns (Doc.ref Doc.teamInvitationList)
        Doc.response 200 "List of sent invitations" Doc.end

    ---

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
        Doc.returns (Doc.ref Doc.teamInvitation)
        Doc.response 200 "Invitation" Doc.end

    ---

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

    ---

    get "/teams/invitations/info" (continue getInvitationByCodeH) $
        accept "application" "json"
        .&. query "code"

    document "GET" "getInvitationInfo" $ do
        Doc.summary "Get invitation info given a code."
        Doc.parameter Doc.Query "code" Doc.bytes' $
            Doc.description "Invitation code"
        Doc.returns (Doc.ref Doc.teamInvitation)
        Doc.response 200 "Invitation successful." Doc.end
        Doc.errorResponse invalidInvitationCode

    --- Internal

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
    toJSON (FoundInvitationCode c) = object [ "code" .= c ]

createInvitationH :: JSON ::: UserId ::: TeamId ::: JsonRequest InvitationRequest -> Handler Response
createInvitationH (_ ::: uid ::: tid ::: req) = do
    body   :: InvitationRequest <- parseJsonBody req
    newInv :: Invitation        <- createInvitation uid tid body
    pure . setStatus status201 . loc (inInvitation newInv) . json $ newInv
  where
    loc iid = addHeader "Location"
            $ "/teams/" <> toByteString' tid <> "/invitations/" <> toByteString' iid

createInvitation :: UserId -> TeamId -> InvitationRequest -> Handler Invitation
createInvitation uid tid body = do
    idt  <- maybe (throwStd noIdentity) return =<< lift (fetchUserIdentity uid)
    from <- maybe (throwStd noEmail)    return (emailIdentity idt)
    let inviteePerms = Team.rolePermissions inviteeRole
        inviteeRole  = fromMaybe Team.defaultRole . irRole $ body
    ensurePermissionToAddUser uid tid inviteePerms
    email <- either (const $ throwStd invalidEmail) return (validateEmail (irEmail body))
    let uk = userEmailKey email
    blacklisted <- lift $ Blacklist.exists uk
    when blacklisted $
        throwStd blacklistedEmail
    maxSize <- setMaxTeamSize <$> view settings
    pending <- lift $ DB.countInvitations tid
    when (fromIntegral pending >= maxSize) $
        throwStd tooManyTeamInvitations
    user <- lift $ Data.lookupKey uk
    case user of
        Just _  -> throwStd emailExists
        Nothing -> doInvite inviteeRole email from (irLocale body)
  where
    doInvite role to from lc = lift $ do
        now     <- liftIO =<< view currentTime
        timeout <- setTeamInvitationTimeout <$> view settings
        (newInv, code) <- DB.insertInvitation tid role to now (Just uid) timeout
        void $ sendInvitationMail to tid from code lc
        return newInv

deleteInvitationH :: JSON ::: UserId ::: TeamId ::: InvitationId -> Handler Response
deleteInvitationH (_ ::: uid ::: tid ::: iid) = do
    empty <$ deleteInvitation uid tid iid

deleteInvitation :: UserId -> TeamId -> InvitationId -> Handler ()
deleteInvitation uid tid iid = do
    ensurePermissions uid tid [Team.AddTeamMember]
    lift $ DB.deleteInvitation tid iid

listInvitationsH :: JSON ::: UserId ::: TeamId ::: Maybe InvitationId ::: Range 1 500 Int32 -> Handler Response
listInvitationsH (_ ::: uid ::: tid ::: start ::: size) = do
    json <$> listInvitations uid tid start size

listInvitations :: UserId -> TeamId -> Maybe InvitationId -> Range 1 500 Int32 -> Handler InvitationList
listInvitations uid tid start size = do
    ensurePermissions uid tid [Team.AddTeamMember]
    rs <- lift $ DB.lookupInvitations tid start size
    return $! InvitationList (DB.resultList rs) (DB.resultHasMore rs)

getInvitationH :: JSON ::: UserId ::: TeamId ::: InvitationId -> Handler Response
getInvitationH (_ ::: uid ::: tid ::: iid) = do
    inv <- getInvitation uid tid iid
    return $ case inv of
        Just i  -> json i
        Nothing -> setStatus status404 empty

getInvitation :: UserId -> TeamId -> InvitationId -> Handler (Maybe Invitation)
getInvitation uid tid iid = do
    ensurePermissions uid tid [Team.AddTeamMember]
    lift $ DB.lookupInvitation tid iid

getInvitationByCodeH :: JSON ::: InvitationCode -> Handler Response
getInvitationByCodeH (_ ::: c) = do
    json <$> getInvitationByCode c

getInvitationByCode :: InvitationCode -> Handler Invitation
getInvitationByCode c = do
    inv <- lift $ DB.lookupInvitationByCode c
    maybe (throwStd invalidInvitationCode) return inv

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
    unless (team^.Team.teamBinding == Team.Binding) $
        throwStd noBindingTeam
    uids <- toList1 =<< lift (fmap (view Team.userId) . view Team.teamMembers <$> Intra.getTeamMembers tid)
    API.changeAccountStatus uids s !>> accountStatusError
  where
    toList1 (x:xs) = return $ List1.list1 x xs
    toList1 []     = throwStd (notFound "Team not found or no members")
