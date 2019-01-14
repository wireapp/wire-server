{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module API.Team.Util where

import Imports
import Bilge hiding (accept, timeout, head)
import Bilge.Assert
import Brig.Types.User
import Brig.Types.Team.Invitation
import Brig.Types.Activation
import Brig.Types.Connection
import Control.Lens (view, (^?))
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Id hiding (client)
import Data.Misc (Milliseconds)
import Data.Range
import Galley.Types (ConvTeamInfo (..), NewConv (..), NewConvUnmanaged (..), NewConvManaged (..))
import Test.Tasty.HUnit
import Util

import qualified Data.Set                    as Set
import qualified Galley.Types.Teams          as Team
import qualified Galley.Types.Teams.Intra    as Team
import qualified Data.Text.Encoding          as T
import qualified Network.Wai.Utilities.Error as Error

createTeam :: UserId -> Galley -> Http TeamId
createTeam u galley = do
    tid <- randomId
    r <- put ( galley
              . paths ["i", "teams", toByteString' tid]
              . contentJson
              . zAuthAccess u "conn"
              . expect2xx
              . lbytes (encode newTeam)
              )
    maybe (error "invalid team id") return $
        fromByteString $ getHeader' "Location" r

-- | NB: the created user is the team owner.
createUserWithTeam :: Brig -> Galley -> Http (UserId, TeamId)
createUserWithTeam brig galley = do
    e <- randomEmail
    n <- randomName
    let p = RequestBodyLBS . encode $ object
            [ "name"            .= n
            , "email"           .= fromEmail e
            , "password"        .= defPassword
            , "team"            .= newTeam
            ]
    user <- decodeBody =<< post (brig . path "/i/users" . contentJson . body p)
    let Just tid = userTeam user
    (team:_) <- view Team.teamListTeams <$> getTeams (userId user) galley
    liftIO $ assertBool "Team ID in registration and team table do not match" (tid == view Team.teamId team)
    selfTeam <- userTeam . selfUser <$> getSelfProfile brig (userId user)
    liftIO $ assertBool "Team ID in self profile and team table do not match" (selfTeam == Just tid)
    return (userId user, tid)

-- | Create a team member with given permissions.
createTeamMember
    :: Brig
    -> Galley
    -> UserId            -- ^ Team owner
    -> TeamId            -- ^ Team where the new user will be created
    -> Team.Permissions  -- ^ Permissions that the new user will have
    -> Http User
createTeamMember brig galley owner tid perm = do
    user <- inviteAndRegisterUser owner tid brig
    updatePermissions owner tid (userId user, perm) galley
    return user

inviteAndRegisterUser :: UserId -> TeamId -> Brig -> Http User
inviteAndRegisterUser u tid brig = do
    inviteeEmail <- randomEmail
    let invite = InvitationRequest inviteeEmail (Name "Bob") Nothing
    inv <- decodeBody =<< postInvitation brig tid u invite
    Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
    rspInvitee <- post (brig . path "/register"
                             . contentJson
                             . body (accept inviteeEmail inviteeCode)) <!! const 201 === statusCode

    let Just invitee = decodeBody rspInvitee
    liftIO $ assertEqual "Team ID in registration and team table do not match" (Just tid) (userTeam invitee)
    selfTeam <- userTeam . selfUser <$> getSelfProfile brig (userId invitee)
    liftIO $ assertEqual "Team ID in self profile and team table do not match" selfTeam (Just tid)
    return invitee

updatePermissions :: UserId -> TeamId -> (UserId, Team.Permissions) -> Galley -> Http ()
updatePermissions from tid (to, perm) galley =
    put ( galley
        . paths ["teams", toByteString' tid, "members"]
        . zUser from
        . zConn "conn"
        . Bilge.json changeMember
        ) !!! const 200 === statusCode
  where
    changeMember = Team.newNewTeamMember $ Team.newTeamMember to perm Nothing

createTeamConv :: HasCallStack => Galley -> TeamId -> UserId -> [UserId] -> Maybe Milliseconds -> Http ConvId
createTeamConv g tid u us mtimer = do
    let tinfo = Just $ ConvTeamInfo tid False
    let conv = NewConvUnmanaged $
               NewConv us Nothing (Set.fromList []) Nothing tinfo mtimer Nothing
    r <- post ( g
              . path "/conversations"
              . zUser u
              . zConn "conn"
              . contentJson
              . lbytes (encode conv)
              ) <!! const 201 === statusCode
    maybe (error "invalid conv id") return $
        fromByteString $ getHeader' "Location" r

-- See Note [managed conversations]
createManagedConv :: HasCallStack => Galley -> TeamId -> UserId -> [UserId] -> Maybe Milliseconds -> Http ConvId
createManagedConv g tid u us mtimer = do
    let tinfo = Just $ ConvTeamInfo tid True
    let conv = NewConvManaged $
               NewConv us Nothing (Set.fromList []) Nothing tinfo mtimer Nothing
    r <- post ( g
              . path "/i/conversations/managed"
              . zUser u
              . zConn "conn"
              . contentJson
              . lbytes (encode conv)
              ) <!! const 201 === statusCode
    maybe (error "invalid conv id") return $
        fromByteString $ getHeader' "Location" r

deleteTeamConv :: HasCallStack => Galley -> TeamId -> ConvId -> UserId -> Http ()
deleteTeamConv g tid cid u = do
    delete ( g
           . paths ["teams", toByteString' tid, "conversations", toByteString' cid]
           . zUser u
           . zConn "conn"
           ) !!! const 200 === statusCode

deleteTeam :: HasCallStack => Galley -> TeamId -> UserId -> Http ()
deleteTeam g tid u = do
    delete ( g
           . paths ["teams", toByteString' tid]
           . zUser u
           . zConn "conn"
           . lbytes (encode $ Team.newTeamDeleteData $ Just Util.defPassword)
           ) !!! const 202 === statusCode

getTeams :: UserId -> Galley -> Http Team.TeamList
getTeams u galley =
    decodeBody =<<
         get ( galley
             . paths ["teams"]
             . zAuthAccess u "conn"
             . expect2xx
             )

newTeam :: Team.BindingNewTeam
newTeam = Team.BindingNewTeam $ Team.newNewTeam (unsafeRange "teamName") (unsafeRange "defaultIcon")

accept :: Email -> InvitationCode -> RequestBody
accept email code = RequestBodyLBS . encode $ object
    [ "name"      .= ("Bob" :: Text)
    , "email"     .= fromEmail email
    , "password"  .= defPassword
    , "team_code" .= code
    ]

register :: Email -> Team.BindingNewTeam -> Brig -> Http (Response (Maybe LByteString))
register e t brig = post (brig . path "/register" . contentJson . body (
    RequestBodyLBS . encode  $ object
        [ "name"            .= ("Bob" :: Text)
        , "email"           .= fromEmail e
        , "password"        .= defPassword
        , "team"            .= t
        ]
    ))

register' :: Email -> Team.BindingNewTeam -> ActivationCode -> Brig -> Http (Response (Maybe LByteString))
register' e t c brig = post (brig . path "/register" . contentJson . body (
    RequestBodyLBS . encode  $ object
        [ "name"            .= ("Bob" :: Text)
        , "email"           .= fromEmail e
        , "email_code"      .= c
        , "password"        .= defPassword
        , "team"            .= t
        ]
    ))

listConnections :: HasCallStack => UserId -> Brig -> Http UserConnectionList
listConnections u brig = do
    decodeBody =<<
         get ( brig
             . path "connections"
             . zUser u
             )

getInvitation :: Brig -> InvitationCode -> Http (Maybe Invitation)
getInvitation brig c = do
    r <- get $ brig
             . path "/teams/invitations/info"
             . queryItem "code" (toByteString' c)
    return . decode . fromMaybe "" $ responseBody r

postInvitation :: Brig -> TeamId -> UserId -> InvitationRequest -> Http ResponseLBS
postInvitation brig t u i = post $ brig
    . paths ["teams", toByteString' t, "invitations"]
    . contentJson
    . body (RequestBodyLBS $ encode i)
    . zAuthAccess u "conn"

suspendTeam :: Brig -> TeamId -> Http (Response (Maybe LByteString))
suspendTeam brig t = post $ brig
    . paths ["i", "teams", toByteString' t, "suspend"]
    . contentJson

unsuspendTeam :: Brig -> TeamId -> Http ResponseLBS
unsuspendTeam brig t = post $ brig
    . paths ["i", "teams", toByteString' t, "unsuspend"]
    . contentJson

getTeam :: HasCallStack => Galley -> TeamId -> Http Team.TeamData
getTeam galley t =
    decodeBody =<< get (galley . paths ["i", "teams", toByteString' t])

getInvitationCode :: HasCallStack => Brig -> TeamId -> InvitationId -> Http (Maybe InvitationCode)
getInvitationCode brig t ref = do
    r <- get ( brig
             . path "/i/teams/invitation-code"
             . queryItem "team" (toByteString' t)
             . queryItem "invitation_id" (toByteString' ref)
             )
    let lbs   = fromMaybe "" $ responseBody r
    return $ fromByteString . fromMaybe (error "No code?") $ T.encodeUtf8 <$> (lbs ^? key "code"  . _String)

assertNoInvitationCode :: HasCallStack => Brig -> TeamId -> InvitationId -> Http ()
assertNoInvitationCode brig t i =
    get ( brig
        . path "/i/teams/invitation-code"
        . queryItem "team" (toByteString' t)
        . queryItem "invitation_id" (toByteString' i)
        ) !!! do
          const 400 === statusCode
          const (Just "invalid-invitation-code") === fmap Error.label . decodeBody

decodeBody' :: (Typeable a, FromJSON a) => Response (Maybe LByteString) -> Http a
decodeBody' x = maybe (error $ "Failed to decodeBody: " ++ show x) return $ decodeBody x

isActivatedUser :: UserId -> Brig -> Http Bool
isActivatedUser uid brig = do
    resp <- get (brig . path "/i/users" . queryItem "ids" (toByteString' uid) . expect2xx)
    pure $ case decodeBody @[User] resp of
        Just (_:_) -> True
        _ -> False
