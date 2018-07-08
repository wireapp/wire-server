{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Brig.Team.API where

import Brig.App (currentTime, settings)
import Brig.API.Error
import Brig.API.Handler
import Brig.API.User (fetchUserIdentity)
import Brig.Data.UserKey (userEmailKey)
import Brig.Email
import Brig.Options (setMaxConvAndTeamSize, setTeamInvitationTimeout)
import Brig.Team.Email
import Brig.Types.Team.Invitation
import Brig.Types.User (InvitationCode, emailIdentity)
import Brig.Types.Intra (AccountStatus (..))
import Control.Error
import Control.Lens (view, (^.))
import Control.Monad (when, void, unless)
import Control.Monad.Trans
import Control.Monad.Reader
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.Id
import Data.Int
import Data.Monoid ((<>))
import Data.Range
import Network.HTTP.Types.Status
import Network.Wai (Request, Response)
import Network.Wai.Predicate hiding (setStatus, result, and)
import Network.Wai.Routing hiding (head)
import Network.Wai.Utilities hiding (message, code, parseJsonBody)
import Network.Wai.Utilities.Swagger (document)
import Prelude

import qualified Brig.API.User                 as API
import qualified Brig.Blacklist                as Blacklist
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

    post "/teams/:tid/invitations" (continue createInvitation) $
        accept "application" "json"
        .&. header "Z-User"
        .&. header "Z-Connection"
        .&. capture "tid"
        .&. request

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

    get "/teams/:tid/invitations" (continue listInvitations) $
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

    get "/teams/:tid/invitations/:iid" (continue getInvitation) $
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

    delete "/teams/:tid/invitations/:iid" (continue deleteInvitation) $
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

    get "/teams/invitations/info" (continue getInvitationByCode) $
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

    get "/i/teams/invitation-code" (continue getInvitationCode) $
        accept "application" "json"
        .&. param "team"
        .&. param "invitation_id"

    post "/i/teams/:tid/suspend" (continue suspendTeam) $
        accept "application" "json"
        .&. capture "tid"

    post "/i/teams/:tid/unsuspend" (continue unsuspendTeam) $
        accept "application" "json"
        .&. capture "tid"

getInvitationCode :: JSON ::: TeamId ::: InvitationId -> Handler Response
getInvitationCode (_ ::: t ::: r) = do
    code <- lift $ DB.lookupInvitationCode t r
    maybe (throwStd invalidInvitationCode) (return . found) code
  where
    found c = json $ object [ "code" .= c ]

createInvitation :: JSON ::: UserId ::: ConnId ::: TeamId ::: Request -> Handler Response
createInvitation (_ ::: uid ::: _ ::: tid ::: req) = do
    body <- parseJsonBody req
    idt  <- maybe (throwStd noIdentity) return =<< lift (fetchUserIdentity uid)
    from <- maybe (throwStd noEmail)    return (emailIdentity idt)
    ensurePermissions uid tid [Team.AddTeamMember]
    email <- maybe (throwStd invalidEmail) return (validateEmail (irEmail body))
    let uk = userEmailKey email
    blacklisted <- lift $ Blacklist.exists uk
    when blacklisted $
        throwStd blacklistedEmail
    maxSize <- setMaxConvAndTeamSize <$> view settings
    pending <- lift $ DB.countInvitations tid
    when (fromIntegral pending >= maxSize) $
        throwStd tooManyTeamInvitations
    user <- lift $ Data.lookupKey uk
    case user of
        Just _  -> throwStd emailExists
        Nothing -> doInvite email from (irLocale body)
  where
    doInvite to from lc = lift $ do
        now     <- liftIO =<< view currentTime
        timeout <- setTeamInvitationTimeout <$> view settings
        (newInv, code) <- DB.insertInvitation tid to now timeout
        void $ sendInvitationMail to tid from code lc
        return . setStatus status201 . loc (inInvitation newInv) $ json newInv

    loc iid = addHeader "Location"
            $ "/teams/" <> toByteString' tid <> "/invitations/" <> toByteString' iid

deleteInvitation :: JSON ::: UserId ::: TeamId ::: InvitationId -> Handler Response
deleteInvitation (_ ::: uid ::: tid ::: iid) = do
    ensurePermissions uid tid [Team.AddTeamMember]
    lift $ DB.deleteInvitation tid iid
    return empty

listInvitations :: JSON ::: UserId ::: TeamId ::: Maybe InvitationId ::: Range 1 500 Int32 -> Handler Response
listInvitations (_ ::: uid ::: tid ::: start ::: size) = do
    ensurePermissions uid tid [Team.AddTeamMember]
    rs <- lift $ DB.lookupInvitations tid start size
    return . json $! InvitationList (DB.resultList rs) (DB.resultHasMore rs)

getInvitation :: JSON ::: UserId ::: TeamId ::: InvitationId -> Handler Response
getInvitation (_ ::: uid ::: tid ::: iid) = do
    ensurePermissions uid tid [Team.AddTeamMember]
    inv <- lift $ DB.lookupInvitation tid iid
    return $ case inv of
        Just i  -> json i
        Nothing -> setStatus status404 empty

getInvitationByCode :: JSON ::: InvitationCode -> Handler Response
getInvitationByCode (_ ::: c) = do
    inv <- lift $ DB.lookupInvitationByCode c
    maybe (throwStd invalidInvitationCode) (return . json) inv

suspendTeam :: JSON ::: TeamId -> Handler Response
suspendTeam (_ ::: tid) = do
    changeTeamAccountStatuses tid Suspended
    DB.deleteInvitations tid
    lift $ Intra.changeTeamStatus tid Team.Suspended Nothing
    return empty

unsuspendTeam :: JSON ::: TeamId -> Handler Response
unsuspendTeam (_ ::: tid) = do
    changeTeamAccountStatuses tid Active
    lift $ Intra.changeTeamStatus tid Team.Active Nothing
    return empty

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

ensurePermissions :: UserId -> TeamId -> [Team.Perm] -> Handler ()
ensurePermissions u t perms = do
    m <- lift $ Intra.getTeamMember u t
    unless (check m) $
        throwStd insufficientTeamPermissions
  where
    check :: Maybe Team.TeamMember -> Bool
    check (Just m) = and $ Team.hasPermission m <$> perms
    check Nothing  = False
