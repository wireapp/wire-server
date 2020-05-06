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

module API.Team.Util where

import API.User.Util
import Bilge hiding (accept, head, timeout)
import Bilge.Assert
import Brig.Types.Activation
import Brig.Types.Connection
import Brig.Types.Team.Invitation
import Brig.Types.Team.LegalHold (LegalHoldStatus, LegalHoldTeamConfig (..))
import Brig.Types.User
import Control.Lens ((^.), (^?), view)
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Id hiding (client)
import Data.Misc (Milliseconds)
import Data.Range
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T
import Galley.Types (ConvTeamInfo (..), NewConv (..), NewConvManaged (..), NewConvUnmanaged (..))
import Galley.Types.Conversations.Roles (roleNameWireAdmin)
import qualified Galley.Types.Teams as Team
import qualified Galley.Types.Teams.Intra as Team
import qualified Galley.Types.Teams.SearchVisibility as Team
import Imports
import qualified Network.Wai.Utilities.Error as Error
import Test.Tasty.HUnit
import Util
import Web.Cookie (parseSetCookie, setCookieName)

-- | FUTUREWORK: Remove 'createPopulatedBindingTeam', 'createPopulatedBindingTeamWithNames',
-- and rename 'createPopulatedBindingTeamWithNamesAndHandles' to 'createPopulatedBindingTeam'.
-- this makes understanding the tests easier because there are fewer setups that only differ
-- from real-world setups in being more artificial, and hopefully won't add too much run time.
createPopulatedBindingTeamWithNamesAndHandles :: Brig -> Galley -> Int -> Http (TeamId, User, [User])
createPopulatedBindingTeamWithNamesAndHandles brig galley numMembers = do
  names <- forM [1 .. numMembers] $ \_ -> randomName
  (tid, owner, mems) <- createPopulatedBindingTeamWithNames brig galley names
  membersWithHandle <- mapM (setRandomHandle brig) mems
  ownerWithHandle <- setRandomHandle brig owner
  return (tid, ownerWithHandle, membersWithHandle)

createPopulatedBindingTeam :: Brig -> Galley -> Int -> Http (TeamId, UserId, [User])
createPopulatedBindingTeam brig galley numMembers = do
  names <- forM [1 .. numMembers] $ \_ -> randomName
  (tid, owner, others) <- createPopulatedBindingTeamWithNames brig galley names
  return (tid, userId owner, others)

createPopulatedBindingTeamWithNames :: Brig -> Galley -> [Name] -> Http (TeamId, User, [User])
createPopulatedBindingTeamWithNames brig galley names = do
  (inviter, tid) <- createUserWithTeam' brig galley
  invitees <- forM names $ \name -> do
    inviteeEmail <- randomEmail
    let invite = stdInvitationRequest inviteeEmail name Nothing Nothing
    inv <- responseJsonError =<< postInvitation brig tid (userId inviter) invite
    Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
    rsp2 <-
      post
        ( brig . path "/register"
            . contentJson
            . body (acceptWithName name inviteeEmail inviteeCode)
        )
        <!! const 201 === statusCode
    let invitee :: User = responseJsonUnsafe rsp2
    do
      let invmeta = Just (userId inviter, inCreatedAt inv)
      mem <- getTeamMember (userId invitee) tid galley
      liftIO $ assertEqual "Member has no/wrong invitation metadata" invmeta (mem ^. Team.invitation)
    do
      let zuid = parseSetCookie <$> getHeader "Set-Cookie" rsp2
      liftIO $ assertEqual "Wrong cookie" (Just "zuid") (setCookieName <$> zuid)
    pure invitee
  pure (tid, inviter, invitees)

createTeam :: UserId -> Galley -> Http TeamId
createTeam u galley = do
  tid <- randomId
  r <-
    put
      ( galley
          . paths ["i", "teams", toByteString' tid]
          . contentJson
          . zAuthAccess u "conn"
          . expect2xx
          . lbytes (encode newTeam)
      )
  maybe (error "invalid team id") return
    $ fromByteString
    $ getHeader' "Location" r

-- | NB: the created user is the team owner.
createUserWithTeam :: Brig -> Galley -> Http (UserId, TeamId)
createUserWithTeam brig galley = do
  (user, tid) <- createUserWithTeam' brig galley
  return (userId user, tid)

-- | NB: the created user is the team owner.
createUserWithTeam' :: Brig -> Galley -> Http (User, TeamId)
createUserWithTeam' brig galley = do
  e <- randomEmail
  n <- randomName
  let p =
        RequestBodyLBS . encode $
          object
            [ "name" .= n,
              "email" .= fromEmail e,
              "password" .= defPassword,
              "team" .= newTeam
            ]
  user <- responseJsonError =<< post (brig . path "/i/users" . contentJson . body p)
  let Just tid = userTeam user
  (team : _) <- view Team.teamListTeams <$> getTeams (userId user) galley
  liftIO $ assertBool "Team ID in registration and team table do not match" (tid == view Team.teamId team)
  selfTeam <- userTeam . selfUser <$> getSelfProfile brig (userId user)
  liftIO $ assertBool "Team ID in self profile and team table do not match" (selfTeam == Just tid)
  return (user, tid)

-- | Create a team member with given permissions.
createTeamMember ::
  Brig ->
  Galley ->
  -- | Team owner
  UserId ->
  -- | Team where the new user will be created
  TeamId ->
  -- | Permissions that the new user will have
  Team.Permissions ->
  Http User
createTeamMember brig galley owner tid perm = do
  user <- inviteAndRegisterUser owner tid brig
  updatePermissions owner tid (userId user, perm) galley
  return user

inviteAndRegisterUser :: UserId -> TeamId -> Brig -> Http User
inviteAndRegisterUser u tid brig = do
  inviteeEmail <- randomEmail
  let invite = stdInvitationRequest inviteeEmail (Name "Bob") Nothing Nothing
  inv <- responseJsonError =<< postInvitation brig tid u invite
  Just inviteeCode <- getInvitationCode brig tid (inInvitation inv)
  rspInvitee <-
    post
      ( brig . path "/register"
          . contentJson
          . body (accept inviteeEmail inviteeCode)
      )
      <!! const 201
      === statusCode
  let Just invitee = responseJsonMaybe rspInvitee
  liftIO $ assertEqual "Team ID in registration and team table do not match" (Just tid) (userTeam invitee)
  selfTeam <- userTeam . selfUser <$> getSelfProfile brig (userId invitee)
  liftIO $ assertEqual "Team ID in self profile and team table do not match" selfTeam (Just tid)
  return invitee

updatePermissions :: HasCallStack => UserId -> TeamId -> (UserId, Team.Permissions) -> Galley -> Http ()
updatePermissions from tid (to, perm) galley =
  put
    ( galley
        . paths ["teams", toByteString' tid, "members"]
        . zUser from
        . zConn "conn"
        . Bilge.json changeMember
    )
    !!! const 200
    === statusCode
  where
    changeMember = Team.newNewTeamMember $ Team.newTeamMember to perm Nothing

createTeamConv :: HasCallStack => Galley -> TeamId -> UserId -> [UserId] -> Maybe Milliseconds -> Http ConvId
createTeamConv g tid u us mtimer = do
  let tinfo = Just $ ConvTeamInfo tid False
  let conv =
        NewConvUnmanaged $
          NewConv (makeIdOpaque <$> us) Nothing (Set.fromList []) Nothing tinfo mtimer Nothing roleNameWireAdmin
  r <-
    post
      ( g
          . path "/conversations"
          . zUser u
          . zConn "conn"
          . contentJson
          . lbytes (encode conv)
      )
      <!! const 201
      === statusCode
  maybe (error "invalid conv id") return
    $ fromByteString
    $ getHeader' "Location" r

-- See Note [managed conversations]
createManagedConv :: HasCallStack => Galley -> TeamId -> UserId -> [UserId] -> Maybe Milliseconds -> Http ConvId
createManagedConv g tid u us mtimer = do
  let tinfo = Just $ ConvTeamInfo tid True
  let conv =
        NewConvManaged $
          NewConv (makeIdOpaque <$> us) Nothing (Set.fromList []) Nothing tinfo mtimer Nothing roleNameWireAdmin
  r <-
    post
      ( g
          . path "/i/conversations/managed"
          . zUser u
          . zConn "conn"
          . contentJson
          . lbytes (encode conv)
      )
      <!! const 201
      === statusCode
  maybe (error "invalid conv id") return
    $ fromByteString
    $ getHeader' "Location" r

deleteTeamConv :: HasCallStack => Galley -> TeamId -> ConvId -> UserId -> Http ()
deleteTeamConv g tid cid u = do
  delete
    ( g
        . paths ["teams", toByteString' tid, "conversations", toByteString' cid]
        . zUser u
        . zConn "conn"
    )
    !!! const 200
    === statusCode

deleteTeam :: HasCallStack => Galley -> TeamId -> UserId -> Http ()
deleteTeam g tid u = do
  delete
    ( g
        . paths ["teams", toByteString' tid]
        . zUser u
        . zConn "conn"
        . lbytes (encode $ Team.newTeamDeleteData $ Just Util.defPassword)
    )
    !!! const 202
    === statusCode

getTeams :: UserId -> Galley -> Http Team.TeamList
getTeams u galley =
  responseJsonError
    =<< get
      ( galley
          . paths ["teams"]
          . zAuthAccess u "conn"
          . expect2xx
      )

newTeam :: Team.BindingNewTeam
newTeam = Team.BindingNewTeam $ Team.newNewTeam (unsafeRange "teamName") (unsafeRange "defaultIcon")

putLegalHoldEnabled :: HasCallStack => TeamId -> LegalHoldStatus -> Galley -> Http ()
putLegalHoldEnabled tid enabled g = do
  void . put $
    g
      . paths ["i", "teams", toByteString' tid, "features", "legalhold"]
      . contentJson
      . lbytes (encode (LegalHoldTeamConfig enabled))
      . expect2xx

accept :: Email -> InvitationCode -> RequestBody
accept email code = acceptWithName (Name "Bob") email code

acceptWithName :: Name -> Email -> InvitationCode -> RequestBody
acceptWithName name email code =
  RequestBodyLBS . encode $
    object
      [ "name" .= fromName name,
        "email" .= fromEmail email,
        "password" .= defPassword,
        "team_code" .= code
      ]

extAccept :: Email -> Name -> Phone -> ActivationCode -> InvitationCode -> RequestBody
extAccept email name phone phoneCode code =
  RequestBodyLBS . encode $
    object
      [ "name" .= name,
        "email" .= fromEmail email,
        "password" .= defPassword,
        "team_code" .= code,
        "phone" .= phone,
        "phone_code" .= phoneCode,
        "team_code" .= code
      ]

register :: Email -> Team.BindingNewTeam -> Brig -> Http (Response (Maybe LByteString))
register e t brig =
  post
    ( brig . path "/register" . contentJson
        . body
          ( RequestBodyLBS . encode $
              object
                [ "name" .= ("Bob" :: Text),
                  "email" .= fromEmail e,
                  "password" .= defPassword,
                  "team" .= t
                ]
          )
    )

register' :: Email -> Team.BindingNewTeam -> ActivationCode -> Brig -> Http (Response (Maybe LByteString))
register' e t c brig =
  post
    ( brig . path "/register" . contentJson
        . body
          ( RequestBodyLBS . encode $
              object
                [ "name" .= ("Bob" :: Text),
                  "email" .= fromEmail e,
                  "email_code" .= c,
                  "password" .= defPassword,
                  "team" .= t
                ]
          )
    )

listConnections :: HasCallStack => UserId -> Brig -> Http UserConnectionList
listConnections u brig = do
  responseJsonError
    =<< get
      ( brig
          . path "connections"
          . zUser u
      )

getInvitation :: Brig -> InvitationCode -> Http (Maybe Invitation)
getInvitation brig c = do
  r <-
    get $
      brig
        . path "/teams/invitations/info"
        . queryItem "code" (toByteString' c)
  return . decode . fromMaybe "" $ responseBody r

postInvitation :: Brig -> TeamId -> UserId -> InvitationRequest -> Http ResponseLBS
postInvitation brig t u i =
  post $
    brig
      . paths ["teams", toByteString' t, "invitations"]
      . contentJson
      . body (RequestBodyLBS $ encode i)
      . zAuthAccess u "conn"

suspendTeam :: Brig -> TeamId -> Http (Response (Maybe LByteString))
suspendTeam brig t =
  post $
    brig
      . paths ["i", "teams", toByteString' t, "suspend"]
      . contentJson

unsuspendTeam :: Brig -> TeamId -> Http ResponseLBS
unsuspendTeam brig t =
  post $
    brig
      . paths ["i", "teams", toByteString' t, "unsuspend"]
      . contentJson

getTeam :: HasCallStack => Galley -> TeamId -> Http Team.TeamData
getTeam galley t =
  responseJsonError =<< get (galley . paths ["i", "teams", toByteString' t])

getInvitationCode :: HasCallStack => Brig -> TeamId -> InvitationId -> Http (Maybe InvitationCode)
getInvitationCode brig t ref = do
  r <-
    get
      ( brig
          . path "/i/teams/invitation-code"
          . queryItem "team" (toByteString' t)
          . queryItem "invitation_id" (toByteString' ref)
      )
  let lbs = fromMaybe "" $ responseBody r
  return $ fromByteString . fromMaybe (error "No code?") $ T.encodeUtf8 <$> (lbs ^? key "code" . _String)

assertNoInvitationCode :: HasCallStack => Brig -> TeamId -> InvitationId -> Http ()
assertNoInvitationCode brig t i =
  get
    ( brig
        . path "/i/teams/invitation-code"
        . queryItem "team" (toByteString' t)
        . queryItem "invitation_id" (toByteString' i)
    )
    !!! do
      const 400 === statusCode
      const (Just "invalid-invitation-code") === fmap Error.label . responseJsonMaybe

isActivatedUser :: UserId -> Brig -> Http Bool
isActivatedUser uid brig = do
  resp <- get (brig . path "/i/users" . queryItem "ids" (toByteString' uid) . expect2xx)
  pure $ case responseJsonMaybe @[User] resp of
    Just (_ : _) -> True
    _ -> False

stdInvitationRequest :: Email -> Name -> Maybe Locale -> Maybe Team.Role -> InvitationRequest
stdInvitationRequest e inviterName loc role =
  InvitationRequest e inviterName loc role Nothing Nothing

setTeamTeamSearchVisibilityAvailable :: HasCallStack => Galley -> TeamId -> Team.TeamSearchVisibilityAvailable -> Http ()
setTeamTeamSearchVisibilityAvailable galley tid status =
  put
    ( galley
        . paths ["i/teams", toByteString' tid, "features/search-visibility"]
        . contentJson
        . body (RequestBodyLBS . encode $ Team.TeamSearchVisibilityAvailableView status)
    )
    !!! do
      const 204 === statusCode

setTeamSearchVisibility :: HasCallStack => Galley -> TeamId -> Team.TeamSearchVisibility -> Http ()
setTeamSearchVisibility galley tid typ =
  put
    ( galley
        . paths ["i/teams", toByteString' tid, "search-visibility"]
        . contentJson
        . body (RequestBodyLBS . encode $ Team.TeamSearchVisibilityView typ)
    )
    !!! do
      const 204 === statusCode
