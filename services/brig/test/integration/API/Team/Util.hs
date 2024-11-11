{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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

module API.Team.Util where

import API.User.Util
import Bilge hiding (accept, head, timeout)
import Bilge.Assert
import Control.Lens ((^?))
import Control.Monad.Catch (MonadCatch)
import Data.Aeson hiding (json)
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Id
import Data.Misc (Milliseconds)
import Data.Range
import Data.Set qualified as Set
import Data.Text.Encoding qualified as T
import Imports
import Network.Wai.Utilities.Error qualified as Error
import Test.Tasty.HUnit
import Util
import Web.Cookie (parseSetCookie, setCookieName)
import Wire.API.Conversation
import Wire.API.Conversation.Role
import Wire.API.Routes.Internal.Galley.TeamsIntra qualified as Team
import Wire.API.Team hiding (newTeam)
import Wire.API.Team.Feature (FeatureStatus (..))
import Wire.API.Team.Feature qualified as Public
import Wire.API.Team.Invitation
import Wire.API.Team.Member qualified as Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.Team.SearchVisibility
import Wire.API.User
import Wire.API.User qualified as Public
import Wire.API.User.Activation

-- | FUTUREWORK: Remove 'createPopulatedBindingTeam', 'createPopulatedBindingTeamWithNames',
-- and rename 'createPopulatedBindingTeamWithNamesAndHandles' to 'createPopulatedBindingTeam'.
-- this makes understanding the tests easier because there are fewer setups that only differ
-- from real-world setups in being more artificial, and hopefully won't add too much run time.
createPopulatedBindingTeamWithNamesAndHandles ::
  (MonadIO m, MonadCatch m, MonadFail m, MonadHttp m, HasCallStack) =>
  Brig ->
  Int ->
  m (TeamId, User, [User])
createPopulatedBindingTeamWithNamesAndHandles brig numMembers = do
  names <- replicateM numMembers randomName
  (tid, owner, mems) <- createPopulatedBindingTeamWithNames brig names
  membersWithHandle <- mapM (setRandomHandle brig) mems
  ownerWithHandle <- setRandomHandle brig owner
  pure (tid, ownerWithHandle, membersWithHandle)

createPopulatedBindingTeam ::
  (MonadIO m, MonadCatch m, MonadFail m, MonadHttp m, HasCallStack) =>
  Brig ->
  Int ->
  m (TeamId, UserId, [User])
createPopulatedBindingTeam brig numMembers = do
  names <- replicateM numMembers randomName
  (tid, owner, others) <- createPopulatedBindingTeamWithNames brig names
  pure (tid, userId owner, others)

createPopulatedBindingTeamWithNames ::
  (MonadIO m, MonadCatch m, MonadFail m, MonadHttp m, HasCallStack) =>
  Brig ->
  [Name] ->
  m (TeamId, User, [User])
createPopulatedBindingTeamWithNames brig names = do
  (inviter, tid) <- createUserWithTeam' brig
  invitees <- forM names $ \name -> do
    inviteeEmail <- randomEmail
    let invite = stdInvitationRequest inviteeEmail
    inv :: Invitation <-
      responseJsonError
        =<< postInvitation brig tid (userId inviter) invite
          <!! statusCode === const 201
    Just inviteeCode <- getInvitationCode brig tid inv.invitationId
    rsp2 <-
      post
        ( brig
            . path "/register"
            . contentJson
            . body (acceptWithName name inviteeEmail inviteeCode)
        )
        <!! const 201 === statusCode
    let invitee :: User = responseJsonUnsafe rsp2
    do
      let zuid = parseSetCookie <$> getHeader "Set-Cookie" rsp2
      liftIO $ assertEqual "Wrong cookie" (Just "zuid") (setCookieName <$> zuid)
    pure invitee
  pure (tid, inviter, invitees)

-- | Create user and binding team.
--
-- NB: the created user is the team owner.
createUserWithTeam :: (MonadIO m, MonadHttp m, MonadCatch m) => Brig -> m (UserId, TeamId)
createUserWithTeam brig = do
  (user, tid) <- createUserWithTeam' brig
  pure (userId user, tid)

-- | Create user and binding team.
--
-- NB: the created user is the team owner.
createUserWithTeam' :: (MonadIO m, MonadHttp m, MonadCatch m, HasCallStack) => Brig -> m (User, TeamId)
createUserWithTeam' brig = do
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
  user <-
    responseJsonError
      =<< post (brig . path "/i/users" . contentJson . body p)
        <!! const 201 === statusCode
  let Just tid = userTeam user
  selfTeam <- userTeam . selfUser <$> getSelfProfile brig (userId user)
  liftIO $ assertBool "Team ID in self profile and team table do not match" (selfTeam == Just tid)
  pure (user, tid)

-- | Create a team member with given permissions.
createTeamMember ::
  Brig ->
  Galley ->
  -- | Team owner
  UserId ->
  -- | Team where the new user will be created
  TeamId ->
  -- | Permissions that the new user will have
  Permissions ->
  Http User
createTeamMember brig galley owner tid perm = do
  user <- inviteAndRegisterUser owner tid brig
  updatePermissions owner tid (userId user, perm) galley
  pure user

inviteAndRegisterUser ::
  (MonadIO m, MonadCatch m, MonadFail m, MonadHttp m, HasCallStack) =>
  UserId ->
  TeamId ->
  Brig ->
  m User
inviteAndRegisterUser u tid brig = do
  inviteeEmail <- randomEmail
  let invite = stdInvitationRequest inviteeEmail
  inv :: Invitation <-
    responseJsonError
      =<< postInvitation brig tid u invite
        <!! statusCode === const 201
  Just inviteeCode <- getInvitationCode brig tid inv.invitationId
  rspInvitee <-
    post
      ( brig
          . path "/register"
          . contentJson
          . body (accept inviteeEmail inviteeCode)
      )
      <!! const 201
        === statusCode
  let Just invitee = responseJsonMaybe rspInvitee
  liftIO $ assertEqual "Team ID in registration and team table do not match" (Just tid) (userTeam invitee)
  selfTeam <- userTeam . selfUser <$> getSelfProfile brig (userId invitee)
  liftIO $ assertEqual "Team ID in self profile and team table do not match" selfTeam (Just tid)
  pure invitee

updatePermissions :: (HasCallStack) => UserId -> TeamId -> (UserId, Permissions) -> Galley -> Http ()
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
    changeMember = Member.mkNewTeamMember to perm Nothing

createTeamConv :: (HasCallStack) => Galley -> TeamId -> UserId -> [UserId] -> Maybe Milliseconds -> Http ConvId
createTeamConv = createTeamConvWithRole roleNameWireAdmin

createTeamConvWithRole :: (HasCallStack) => RoleName -> Galley -> TeamId -> UserId -> [UserId] -> Maybe Milliseconds -> Http ConvId
createTeamConvWithRole role g tid u us mtimer = do
  let tinfo = Just $ ConvTeamInfo tid
  let conv =
        NewConv
          us
          []
          Nothing
          (Set.fromList [])
          Nothing
          tinfo
          mtimer
          Nothing
          role
          BaseProtocolProteusTag
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
  maybe (error "invalid conv id") pure $
    fromByteString $
      getHeader' "Location" r

deleteTeamConv :: (HasCallStack) => Galley -> TeamId -> ConvId -> UserId -> Http ()
deleteTeamConv g tid cid u = do
  delete
    ( g
        . paths ["teams", toByteString' tid, "conversations", toByteString' cid]
        . zUser u
        . zConn "conn"
    )
    !!! const 200
      === statusCode

deleteTeam :: (HasCallStack) => Galley -> TeamId -> UserId -> Http ()
deleteTeam g tid u = do
  delete
    ( g
        . paths ["teams", toByteString' tid]
        . zUser u
        . zConn "conn"
        . json (newTeamDeleteData $ Just Util.defPassword)
    )
    !!! const 202
      === statusCode

newTeam :: NewTeam
newTeam = newNewTeam (unsafeRange "teamName") DefaultIcon

putLegalHoldEnabled :: (HasCallStack) => TeamId -> FeatureStatus -> Galley -> Http ()
putLegalHoldEnabled tid enabled g = do
  void . put $
    g
      . paths ["i", "teams", toByteString' tid, "features", "legalhold"]
      . contentJson
      . lbytes (encode (Public.Feature enabled Public.LegalholdConfig))
      . expect2xx

putLHWhitelistTeam :: (HasCallStack) => Galley -> TeamId -> Http ResponseLBS
putLHWhitelistTeam galley tid = do
  put
    ( galley
        . paths ["i", "legalhold", "whitelisted-teams", toByteString' tid]
    )

accept :: EmailAddress -> InvitationCode -> RequestBody
accept = acceptWithName (Name "Bob")

acceptWithName :: Name -> EmailAddress -> InvitationCode -> RequestBody
acceptWithName name email code =
  RequestBodyLBS . encode $
    object
      [ "name" .= fromName name,
        "email" .= fromEmail email,
        "password" .= defPassword,
        "team_code" .= code
      ]

extAccept :: EmailAddress -> Name -> Phone -> ActivationCode -> InvitationCode -> RequestBody
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

register :: EmailAddress -> NewTeam -> Brig -> Http (Response (Maybe LByteString))
register e t brig =
  post
    ( brig
        . path "/register"
        . contentJson
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

register' :: EmailAddress -> NewTeam -> ActivationCode -> Brig -> Http (Response (Maybe LByteString))
register' e t c brig =
  post
    ( brig
        . path "/register"
        . contentJson
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

getInvitationInfo :: Brig -> InvitationCode -> (MonadIO m, MonadHttp m) => m (Maybe Invitation)
getInvitationInfo brig c = do
  r <-
    get $
      brig
        . path "/teams/invitations/info"
        . queryItem "code" (toByteString' c)
  pure . decode . fromMaybe "" $ responseBody r

getInvitation :: Brig -> TeamId -> InvitationId -> UserId -> Http ResponseLBS
getInvitation brig tid iid uid =
  get (brig . paths ["teams", toByteString' tid, "invitations", toByteString' iid] . zUser uid)

deleteInvitation :: Brig -> TeamId -> InvitationId -> UserId -> Http ()
deleteInvitation brig tid iid uid =
  delete (brig . paths ["teams", toByteString' tid, "invitations", toByteString' iid] . zUser uid) !!! const 200 === statusCode

postInvitation ::
  (MonadHttp m, HasCallStack) =>
  Brig ->
  TeamId ->
  UserId ->
  InvitationRequest ->
  m ResponseLBS
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

getTeam :: (HasCallStack, MonadIO m, MonadHttp m, HasCallStack, MonadCatch m) => Galley -> TeamId -> m Team.TeamData
getTeam galley t =
  responseJsonError =<< get (galley . paths ["i", "teams", toByteString' t])

getInvitationCode ::
  (MonadIO m, MonadHttp m, HasCallStack) =>
  Brig ->
  TeamId ->
  InvitationId ->
  m (Maybe InvitationCode)
getInvitationCode brig t ref = do
  r <-
    get
      ( brig
          . path "/i/teams/invitation-code"
          . queryItem "team" (toByteString' t)
          . queryItem "invitation_id" (toByteString' ref)
      )
  let lbs = fromMaybe "" $ responseBody r
  pure $ fromByteString (maybe (error "No code?") T.encodeUtf8 (lbs ^? key "code" . _String))

assertNoInvitationCode :: (HasCallStack) => Brig -> TeamId -> InvitationId -> (MonadIO m, MonadHttp m, MonadCatch m) => m ()
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

stdInvitationRequest :: EmailAddress -> InvitationRequest
stdInvitationRequest = stdInvitationRequest' Nothing Nothing

stdInvitationRequest' :: Maybe Locale -> Maybe Role -> EmailAddress -> InvitationRequest
stdInvitationRequest' loc role email =
  InvitationRequest loc role Nothing email True

setTeamTeamSearchVisibilityAvailable :: (HasCallStack, MonadHttp m, MonadIO m, MonadCatch m) => Galley -> TeamId -> FeatureStatus -> m ()
setTeamTeamSearchVisibilityAvailable galley tid status =
  put
    ( galley
        . paths ["i/teams", toByteString' tid, "features/searchVisibility"]
        . contentJson
        . body (RequestBodyLBS . encode $ Public.Feature status Public.SearchVisibilityAvailableConfig)
    )
    !!! do
      const 200 === statusCode

setTeamSearchVisibility :: (HasCallStack) => Galley -> TeamId -> TeamSearchVisibility -> Http ()
setTeamSearchVisibility galley tid typ =
  put
    ( galley
        . paths ["i/teams", toByteString' tid, "search-visibility"]
        . contentJson
        . body (RequestBodyLBS . encode $ TeamSearchVisibilityView typ)
    )
    !!! do
      const 204 === statusCode

setTeamSearchVisibilityInboundAvailable :: (HasCallStack, MonadHttp m, MonadIO m, MonadCatch m) => Galley -> TeamId -> FeatureStatus -> m ()
setTeamSearchVisibilityInboundAvailable galley tid status =
  put
    ( galley
        . paths ["i", "teams", toByteString' tid, "features", Public.featureNameBS @Public.SearchVisibilityInboundConfig]
        . contentJson
        . body (RequestBodyLBS . encode $ Public.Feature status Public.SearchVisibilityInboundConfig)
    )
    !!! do
      const 200 === statusCode

setUserEmail :: Brig -> UserId -> UserId -> EmailAddress -> Http ResponseLBS
setUserEmail brig from uid email = do
  put
    ( brig
        . paths ["users", toByteString' uid, "email"]
        . zUser from
        . zConn "conn"
        . contentJson
        . body (RequestBodyLBS . encode $ Public.EmailUpdate email)
    )
