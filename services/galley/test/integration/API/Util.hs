{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

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

module API.Util where

import qualified API.SQS as SQS
import Bilge hiding (timeout)
import Bilge.Assert
import Brig.Types
import Brig.Types.Team.Invitation
import Brig.Types.User.Auth (CookieLabel (..))
import Control.Lens hiding (from, to, (#), (.=))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Fail (MonadFail)
import Control.Retry (constantDelay, limitRetries, retrying)
import Data.Aeson hiding (json)
import Data.Aeson.Lens (key, _String)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.Currency as Currency
import qualified Data.HashMap.Strict as HashMap
import Data.Id
import Data.List1 as List1
import qualified Data.Map.Strict as Map
import Data.Misc
import Data.ProtocolBuffers (encodeMessage)
import Data.Range
import Data.Serialize (runPut)
import qualified Data.Set as Set
import Data.String.Conversions (ST, cs)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import Data.UUID.V4
import qualified Galley.Options as Opts
import qualified Galley.Run as Run
import Galley.Types hiding (InternalMember (..), Member)
import Galley.Types.Conversations.Roles hiding (DeleteConversation)
import Galley.Types.Teams hiding (Event, EventType (..))
import qualified Galley.Types.Teams as Team
import Galley.Types.Teams.Intra
import Gundeck.Types.Notification
  ( Notification (..),
    NotificationId,
    QueuedNotification,
    QueuedNotificationList,
    queuedHasMore,
    queuedNotificationId,
    queuedNotificationPayload,
    queuedNotifications,
    queuedTime,
  )
import Imports
import qualified Network.Wai.Test as WaiTest
import qualified Test.QuickCheck as Q
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestSetup
import UnliftIO.Timeout
import Web.Cookie
import Wire.API.Conversation.Member (Member (..))
import qualified Wire.API.Event.Team as TE
import qualified Wire.API.Message.Proto as Proto

-------------------------------------------------------------------------------
-- API Operations

symmPermissions :: [Perm] -> Permissions
symmPermissions p = let s = Set.fromList p in fromJust (newPermissions s s)

createBindingTeam :: HasCallStack => TestM (UserId, TeamId)
createBindingTeam = do
  ownerid <- randomTeamCreator
  teams <- getTeams ownerid
  let [team] = view (teamListTeams) teams
  let tid = view teamId team
  SQS.assertQueue "create team" SQS.tActivate
  refreshIndex
  pure (ownerid, tid)

createBindingTeamWithMembers :: HasCallStack => Int -> TestM (TeamId, UserId, [UserId])
createBindingTeamWithMembers numUsers = do
  (owner, tid) <- createBindingTeam
  members <- forM [2 .. numUsers] $ \n -> do
    mem <- addUserToTeam owner tid
    SQS.assertQueue "add member" $ SQS.tUpdate (fromIntegral n) [owner]
    refreshIndex
    return $ view Galley.Types.Teams.userId $ mem

  return (tid, owner, members)

getTeams :: UserId -> TestM TeamList
getTeams u = do
  g <- view tsGalley
  r <-
    get
      ( g
          . paths ["teams"]
          . zUser u
          . zConn "conn"
          . zType "access"
          . expect2xx
      )
  return $ responseJsonUnsafe r

createBindingTeamWithNMembers :: Int -> TestM (UserId, TeamId, [UserId])
createBindingTeamWithNMembers n = do
  (owner, tid) <- createBindingTeam
  mems <- replicateM n $ do
    member1 <- randomUser
    addTeamMemberInternal tid $ newTeamMember member1 (rolePermissions RoleMember) Nothing
    pure member1
  SQS.ensureQueueEmpty
  pure (owner, tid, mems)

-- | FUTUREWORK: this is dead code (see 'NonBindingNewTeam').  remove!
createNonBindingTeam :: HasCallStack => Text -> UserId -> [TeamMember] -> TestM TeamId
createNonBindingTeam name owner mems = do
  g <- view tsGalley
  let mm = if null mems then Nothing else Just $ unsafeRange (take 127 mems)
  let nt = NonBindingNewTeam $ newNewTeam (unsafeRange name) (unsafeRange "icon") & newTeamMembers .~ mm
  resp <-
    post (g . path "/teams" . zUser owner . zConn "conn" . zType "access" . json nt) <!! do
      const 201 === statusCode
      const True === isJust . getHeader "Location"
  fromBS (getHeader' "Location" resp)

changeTeamStatus :: HasCallStack => TeamId -> TeamStatus -> TestM ()
changeTeamStatus tid s = do
  g <- view tsGalley
  put
    ( g . paths ["i", "teams", toByteString' tid, "status"]
        . json (TeamStatusUpdate s Nothing)
    )
    !!! const 200
    === statusCode

createBindingTeamInternal :: HasCallStack => Text -> UserId -> TestM TeamId
createBindingTeamInternal name owner = do
  tid <- createBindingTeamInternalNoActivate name owner
  changeTeamStatus tid Active
  return tid

createBindingTeamInternalNoActivate :: HasCallStack => Text -> UserId -> TestM TeamId
createBindingTeamInternalNoActivate name owner = do
  g <- view tsGalley
  tid <- randomId
  let nt = BindingNewTeam $ newNewTeam (unsafeRange name) (unsafeRange "icon")
  _ <-
    put (g . paths ["/i/teams", toByteString' tid] . zUser owner . zConn "conn" . zType "access" . json nt) <!! do
      const 201 === statusCode
      const True === isJust . getHeader "Location"
  return tid

createBindingTeamInternalWithCurrency :: HasCallStack => Text -> UserId -> Currency.Alpha -> TestM TeamId
createBindingTeamInternalWithCurrency name owner cur = do
  g <- view tsGalley
  tid <- createBindingTeamInternalNoActivate name owner
  _ <-
    put (g . paths ["i", "teams", toByteString' tid, "status"] . json (TeamStatusUpdate Active $ Just cur))
      !!! const 200 === statusCode
  return tid

getTeamInternal :: HasCallStack => TeamId -> TestM TeamData
getTeamInternal tid = do
  g <- view tsGalley
  r <- get (g . paths ["i/teams", toByteString' tid]) <!! const 200 === statusCode
  responseJsonError r

getTeam :: HasCallStack => UserId -> TeamId -> TestM Team
getTeam usr tid = do
  g <- view tsGalley
  r <- get (g . paths ["teams", toByteString' tid] . zUser usr) <!! const 200 === statusCode
  responseJsonError r

getTeamMembers :: HasCallStack => UserId -> TeamId -> TestM TeamMemberList
getTeamMembers usr tid = do
  g <- view tsGalley
  r <- get (g . paths ["teams", toByteString' tid, "members"] . zUser usr) <!! const 200 === statusCode
  responseJsonError r

getTeamMembersTruncated :: HasCallStack => UserId -> TeamId -> Int -> TestM TeamMemberList
getTeamMembersTruncated usr tid n = do
  g <- view tsGalley
  r <- get (g . paths ["teams", toByteString' tid, "members"] . zUser usr . queryItem "maxResults" (C.pack $ show n)) <!! const 200 === statusCode
  responseJsonError r

getTeamMembersInternalTruncated :: HasCallStack => TeamId -> Int -> TestM TeamMemberList
getTeamMembersInternalTruncated tid n = do
  g <- view tsGalley
  r <-
    get
      ( g
          . paths ["i", "teams", toByteString' tid, "members"]
          . queryItem "maxResults" (C.pack $ show n)
      )
      <!! const 200
      === statusCode
  responseJsonError r

bulkGetTeamMembers :: HasCallStack => UserId -> TeamId -> [UserId] -> TestM TeamMemberList
bulkGetTeamMembers usr tid uids = do
  g <- view tsGalley
  r <-
    post
      ( g
          . paths ["teams", toByteString' tid, "get-members-by-ids-using-post"]
          . zUser usr
          . json (UserIdList uids)
      )
      <!! const 200
      === statusCode
  responseJsonError r

bulkGetTeamMembersTruncated :: HasCallStack => UserId -> TeamId -> [UserId] -> Int -> TestM ResponseLBS
bulkGetTeamMembersTruncated usr tid uids trnc = do
  g <- view tsGalley
  post
    ( g
        . paths ["teams", toByteString' tid, "get-members-by-ids-using-post"]
        . zUser usr
        . queryItem "maxResults" (C.pack $ show trnc)
        . json (UserIdList uids)
    )

getTeamMember :: HasCallStack => UserId -> TeamId -> UserId -> TestM TeamMember
getTeamMember usr tid mid = do
  g <- view tsGalley
  r <- get (g . paths ["teams", toByteString' tid, "members", toByteString' mid] . zUser usr) <!! const 200 === statusCode
  responseJsonError r

getTeamMemberInternal :: HasCallStack => TeamId -> UserId -> TestM TeamMember
getTeamMemberInternal tid mid = do
  g <- view tsGalley
  r <- get (g . paths ["i", "teams", toByteString' tid, "members", toByteString' mid]) <!! const 200 === statusCode
  responseJsonError r

addTeamMember :: HasCallStack => UserId -> TeamId -> TeamMember -> TestM ()
addTeamMember usr tid mem = do
  g <- view tsGalley
  let payload = json (newNewTeamMember mem)
  post (g . paths ["teams", toByteString' tid, "members"] . zUser usr . zConn "conn" . payload)
    !!! const 200 === statusCode

addTeamMemberInternal :: HasCallStack => TeamId -> TeamMember -> TestM ()
addTeamMemberInternal tid mem = addTeamMemberInternal' tid mem !!! const 200 === statusCode

addTeamMemberInternal' :: HasCallStack => TeamId -> TeamMember -> TestM ResponseLBS
addTeamMemberInternal' tid mem = do
  g <- view tsGalley
  let payload = json (newNewTeamMember mem)
  post (g . paths ["i", "teams", toByteString' tid, "members"] . payload)

stdInvitationRequest :: Email -> Name -> Maybe Locale -> Maybe Team.Role -> InvitationRequest
stdInvitationRequest e inviterName loc role =
  InvitationRequest e inviterName loc role Nothing Nothing

addUserToTeam :: HasCallStack => UserId -> TeamId -> TestM TeamMember
addUserToTeam = addUserToTeamWithRole Nothing

addUserToTeam' :: HasCallStack => UserId -> TeamId -> TestM ResponseLBS
addUserToTeam' u t = snd <$> addUserToTeamWithRole' Nothing u t

addUserToTeamWithRole :: HasCallStack => Maybe Role -> UserId -> TeamId -> TestM TeamMember
addUserToTeamWithRole role inviter tid = do
  (inv, rsp2) <- addUserToTeamWithRole' role inviter tid -- TODO: <!! const 201 === statusCode
  let invitee :: User = responseJsonUnsafe rsp2
      inviteeId = Brig.Types.userId invitee
  let invmeta = Just (inviter, inCreatedAt inv)
  mem <- getTeamMember inviteeId tid inviteeId
  liftIO $ assertEqual "Member has no/wrong invitation metadata" invmeta (mem ^. Team.invitation)
  let zuid = parseSetCookie <$> getHeader "Set-Cookie" rsp2
  liftIO $ assertEqual "Wrong cookie" (Just "zuid") (setCookieName <$> zuid)
  pure mem

addUserToTeamWithRole' :: HasCallStack => Maybe Role -> UserId -> TeamId -> TestM (Invitation, ResponseLBS)
addUserToTeamWithRole' role inviter tid = do
  brig <- view tsBrig
  inviteeEmail <- randomEmail
  let name = Name $ fromEmail inviteeEmail
  let invite = stdInvitationRequest inviteeEmail name Nothing role
  invResponse <- postInvitation tid inviter invite
  inv <- responseJsonError invResponse
  Just inviteeCode <- getInvitationCode tid (inInvitation inv)
  r <-
    post
      ( brig . path "/register"
          . contentJson
          . body (acceptInviteBody name inviteeEmail inviteeCode)
      )
  return (inv, r)

addUserToTeamWithSSO :: HasCallStack => Bool -> TeamId -> TestM TeamMember
addUserToTeamWithSSO hasEmail tid = do
  let ssoid = UserSSOId "nil" "nil"
  user <- responseJsonError =<< postSSOUser "SSO User" hasEmail ssoid tid
  let uid = Brig.Types.userId user
  getTeamMember uid tid uid

makeOwner :: HasCallStack => UserId -> TeamMember -> TeamId -> TestM ()
makeOwner owner mem tid = do
  galley <- view tsGalley
  let changeMember = newNewTeamMember (mem & permissions .~ fullPermissions)
  put
    ( galley
        . paths ["teams", toByteString' tid, "members"]
        . zUser owner
        . zConn "conn"
        . json changeMember
    )
    !!! const 200
    === statusCode

acceptInviteBody :: Name -> Email -> InvitationCode -> RequestBody
acceptInviteBody name email code =
  RequestBodyLBS . encode $
    object
      [ "name" .= fromName name,
        "email" .= fromEmail email,
        "password" .= defPassword,
        "team_code" .= code
      ]

postInvitation :: TeamId -> UserId -> InvitationRequest -> TestM ResponseLBS
postInvitation t u i = do
  brig <- view tsBrig
  post $
    brig
      . paths ["teams", toByteString' t, "invitations"]
      . contentJson
      . body (RequestBodyLBS $ encode i)
      . zAuthAccess u "conn"

zAuthAccess :: UserId -> ByteString -> (Request -> Request)
zAuthAccess u conn =
  zUser u
    . zConn conn
    . zType "access"

getInvitationCode :: HasCallStack => TeamId -> InvitationId -> TestM (Maybe InvitationCode)
getInvitationCode t ref = do
  brig <- view tsBrig
  r <-
    get
      ( brig
          . path "/i/teams/invitation-code"
          . queryItem "team" (toByteString' t)
          . queryItem "invitation_id" (toByteString' ref)
      )
  let lbs = fromMaybe "" $ responseBody r
  return $ fromByteString . fromMaybe (error "No code?") $ Text.encodeUtf8 <$> (lbs ^? key "code" . _String)

-- Note that here we don't make use of the datatype because NewConv has a default
-- and therefore cannot be unset. However, given that this is to test the legacy
-- API (i.e., no roles) it's fine to hardcode the JSON object in the test since
-- it clearly shows the API that old(er) clients use.
createTeamConvLegacy :: HasCallStack => UserId -> TeamId -> [UserId] -> Maybe Text -> TestM ConvId
createTeamConvLegacy u tid us name = do
  g <- view tsGalley
  let tinfo = ConvTeamInfo tid False
  let convPayload =
        object
          [ "users" .= us,
            "name" .= name,
            "team" .= Just tinfo
          ]
  post
    ( g
        . path "/conversations"
        . zUser u
        . zConn "conn"
        . zType "access"
        . json convPayload
    )
    >>= \r -> fromBS (getHeader' "Location" r)

createTeamConv :: HasCallStack => UserId -> TeamId -> [UserId] -> Maybe Text -> Maybe (Set Access) -> Maybe Milliseconds -> TestM ConvId
createTeamConv u tid us name acc mtimer = createTeamConvAccess u tid us name acc Nothing mtimer (Just roleNameWireAdmin)

createTeamConvWithRole :: HasCallStack => UserId -> TeamId -> [UserId] -> Maybe Text -> Maybe (Set Access) -> Maybe Milliseconds -> RoleName -> TestM ConvId
createTeamConvWithRole u tid us name acc mtimer convRole = createTeamConvAccess u tid us name acc Nothing mtimer (Just convRole)

createTeamConvAccess :: HasCallStack => UserId -> TeamId -> [UserId] -> Maybe Text -> Maybe (Set Access) -> Maybe AccessRole -> Maybe Milliseconds -> Maybe RoleName -> TestM ConvId
createTeamConvAccess u tid us name acc role mtimer convRole = do
  r <- createTeamConvAccessRaw u tid us name acc role mtimer convRole <!! const 201 === statusCode
  fromBS (getHeader' "Location" r)

createTeamConvAccessRaw :: UserId -> TeamId -> [UserId] -> Maybe Text -> Maybe (Set Access) -> Maybe AccessRole -> Maybe Milliseconds -> Maybe RoleName -> TestM ResponseLBS
createTeamConvAccessRaw u tid us name acc role mtimer convRole = do
  g <- view tsGalley
  let tinfo = ConvTeamInfo tid False
  let conv =
        NewConvUnmanaged $
          NewConv (makeIdOpaque <$> us) name (fromMaybe (Set.fromList []) acc) role (Just tinfo) mtimer Nothing (fromMaybe roleNameWireAdmin convRole)
  post
    ( g
        . path "/conversations"
        . zUser u
        . zConn "conn"
        . zType "access"
        . json conv
    )

updateTeamConv :: UserId -> ConvId -> ConversationRename -> TestM ResponseLBS
updateTeamConv zusr convid upd = do
  g <- view tsGalley
  put
    ( g
        . paths ["/conversations", toByteString' convid]
        . zUser zusr
        . zConn "conn"
        . zType "access"
        . json upd
    )

-- | See Note [managed conversations]
createManagedConv :: HasCallStack => UserId -> TeamId -> [UserId] -> Maybe Text -> Maybe (Set Access) -> Maybe Milliseconds -> TestM ConvId
createManagedConv u tid us name acc mtimer = do
  g <- view tsGalley
  let tinfo = ConvTeamInfo tid True
  let conv =
        NewConvManaged $
          NewConv (makeIdOpaque <$> us) name (fromMaybe (Set.fromList []) acc) Nothing (Just tinfo) mtimer Nothing roleNameWireAdmin
  r <-
    post
      ( g
          . path "i/conversations/managed"
          . zUser u
          . zConn "conn"
          . zType "access"
          . json conv
      )
      <!! const 201 === statusCode
  fromBS (getHeader' "Location" r)

createOne2OneTeamConv :: UserId -> UserId -> Maybe Text -> TeamId -> TestM ResponseLBS
createOne2OneTeamConv u1 u2 n tid = do
  g <- view tsGalley
  let conv =
        NewConvUnmanaged $
          NewConv [makeIdOpaque u2] n mempty Nothing (Just $ ConvTeamInfo tid False) Nothing Nothing roleNameWireAdmin
  post $ g . path "/conversations/one2one" . zUser u1 . zConn "conn" . zType "access" . json conv

postConv :: UserId -> [UserId] -> Maybe Text -> [Access] -> Maybe AccessRole -> Maybe Milliseconds -> TestM ResponseLBS
postConv u us name a r mtimer = postConvWithRole u us name a r mtimer roleNameWireAdmin

postConvWithRole :: UserId -> [UserId] -> Maybe Text -> [Access] -> Maybe AccessRole -> Maybe Milliseconds -> RoleName -> TestM ResponseLBS
postConvWithRole u us name a r mtimer role = do
  g <- view tsGalley
  let conv = NewConvUnmanaged $ NewConv (makeIdOpaque <$> us) name (Set.fromList a) r Nothing mtimer Nothing role
  post $ g . path "/conversations" . zUser u . zConn "conn" . zType "access" . json conv

postConvWithReceipt :: UserId -> [UserId] -> Maybe Text -> [Access] -> Maybe AccessRole -> Maybe Milliseconds -> ReceiptMode -> TestM ResponseLBS
postConvWithReceipt u us name a r mtimer rcpt = do
  g <- view tsGalley
  let conv = NewConvUnmanaged $ NewConv (makeIdOpaque <$> us) name (Set.fromList a) r Nothing mtimer (Just rcpt) roleNameWireAdmin
  post $ g . path "/conversations" . zUser u . zConn "conn" . zType "access" . json conv

postSelfConv :: UserId -> TestM ResponseLBS
postSelfConv u = do
  g <- view tsGalley
  post $ g . path "/conversations/self" . zUser u . zConn "conn" . zType "access"

postO2OConv :: UserId -> UserId -> Maybe Text -> TestM ResponseLBS
postO2OConv u1 u2 n = do
  g <- view tsGalley
  let conv = NewConvUnmanaged $ NewConv [makeIdOpaque u2] n mempty Nothing Nothing Nothing Nothing roleNameWireAdmin
  post $ g . path "/conversations/one2one" . zUser u1 . zConn "conn" . zType "access" . json conv

postConnectConv :: UserId -> UserId -> Text -> Text -> Maybe Text -> TestM ResponseLBS
postConnectConv a b name msg email = do
  g <- view tsGalley
  post $
    g
      . path "/i/conversations/connect"
      . zUser a
      . zConn "conn"
      . zType "access"
      . json (Connect b (Just msg) (Just name) email)

putConvAccept :: UserId -> ConvId -> TestM ResponseLBS
putConvAccept invited cid = do
  g <- view tsGalley
  put $
    g
      . paths ["/i/conversations", C.pack $ show cid, "accept", "v2"]
      . zUser invited
      . zType "access"
      . zConn "conn"

postOtrMessage ::
  (Request -> Request) ->
  UserId ->
  ClientId ->
  ConvId ->
  [(UserId, ClientId, Text)] ->
  TestM ResponseLBS
postOtrMessage = postOtrMessage' Nothing

postOtrMessage' ::
  Maybe [OpaqueUserId] ->
  (Request -> Request) ->
  UserId ->
  ClientId ->
  ConvId ->
  [(UserId, ClientId, Text)] ->
  TestM ResponseLBS
postOtrMessage' reportMissing f u d c rec = do
  g <- view tsGalley
  post $
    g
      . f
      . paths ["conversations", toByteString' c, "otr", "messages"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json (mkOtrPayload d rec reportMissing)

-- | FUTUREWORK: remove first argument, it's 'id' in all calls to this function!
postOtrBroadcastMessage :: (Request -> Request) -> UserId -> ClientId -> [(UserId, ClientId, Text)] -> TestM ResponseLBS
postOtrBroadcastMessage req usrs clt rcps = do
  g <- view tsGalley
  postOtrBroadcastMessage' g Nothing req usrs clt rcps

-- | 'postOtrBroadcastMessage' with @"report_missing"@ in body.
postOtrBroadcastMessage' :: (Monad m, MonadCatch m, MonadIO m, MonadHttp m, MonadFail m, HasCallStack) => (Request -> Request) -> Maybe [OpaqueUserId] -> (Request -> Request) -> UserId -> ClientId -> [(UserId, ClientId, Text)] -> m ResponseLBS
postOtrBroadcastMessage' g reportMissingBody f u d rec = do
  post $
    g
      . f
      . paths ["broadcast", "otr", "messages"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json (mkOtrPayload d rec reportMissingBody)

mkOtrPayload :: ClientId -> [(UserId, ClientId, Text)] -> Maybe [OpaqueUserId] -> Value
mkOtrPayload sender rec reportMissingBody =
  object
    [ "sender" .= sender,
      "recipients" .= (HashMap.map toJSON . HashMap.fromListWith HashMap.union $ map mkOtrMessage rec),
      "data" .= Just ("data" :: Text),
      "report_missing" .= reportMissingBody
    ]

mkOtrMessage :: (UserId, ClientId, Text) -> (Text, HashMap.HashMap Text Text)
mkOtrMessage (usr, clt, m) = (fn usr, HashMap.singleton (fn clt) m)
  where
    fn :: (FromByteString a, ToByteString a) => a -> Text
    fn = fromJust . fromByteString . toByteString'

postProtoOtrMessage :: UserId -> ClientId -> ConvId -> OtrRecipients -> TestM ResponseLBS
postProtoOtrMessage = postProtoOtrMessage' Nothing id

postProtoOtrMessage' :: Maybe [OpaqueUserId] -> (Request -> Request) -> UserId -> ClientId -> ConvId -> OtrRecipients -> TestM ResponseLBS
postProtoOtrMessage' reportMissing modif u d c rec = do
  g <- view tsGalley
  let m = runPut (encodeMessage $ mkOtrProtoMessage d rec reportMissing)
   in post $
        g
          . modif
          . paths ["conversations", toByteString' c, "otr", "messages"]
          . zUser u
          . zConn "conn"
          . zType "access"
          . contentProtobuf
          . bytes m

postProtoOtrBroadcast :: UserId -> ClientId -> OtrRecipients -> TestM ResponseLBS
postProtoOtrBroadcast = postProtoOtrBroadcast' Nothing id

postProtoOtrBroadcast' :: Maybe [OpaqueUserId] -> (Request -> Request) -> UserId -> ClientId -> OtrRecipients -> TestM ResponseLBS
postProtoOtrBroadcast' reportMissing modif u d rec = do
  g <- view tsGalley
  let m = runPut (encodeMessage $ mkOtrProtoMessage d rec reportMissing)
   in post $
        g
          . modif
          . paths ["broadcast", "otr", "messages"]
          . zUser u
          . zConn "conn"
          . zType "access"
          . contentProtobuf
          . bytes m

mkOtrProtoMessage :: ClientId -> OtrRecipients -> Maybe [OpaqueUserId] -> Proto.NewOtrMessage
mkOtrProtoMessage sender rec reportMissing =
  let rcps = Proto.fromOtrRecipients rec
      sndr = Proto.fromClientId sender
      rmis = Proto.fromUserId <$> fromMaybe [] reportMissing
   in Proto.newOtrMessage sndr rcps
        & Proto.newOtrMessageData ?~ "data"
        & Proto.newOtrMessageReportMissing .~ rmis

getConvs :: UserId -> Maybe (Either [ConvId] ConvId) -> Maybe Int32 -> TestM ResponseLBS
getConvs u r s = do
  g <- view tsGalley
  get $
    g
      . path "/conversations"
      . zUser u
      . zConn "conn"
      . zType "access"
      . convRange r s

getConv :: UserId -> ConvId -> TestM ResponseLBS
getConv u c = do
  g <- view tsGalley
  get $
    g
      . paths ["conversations", toByteString' c]
      . zUser u
      . zConn "conn"
      . zType "access"

getConvIds :: UserId -> Maybe (Either [ConvId] ConvId) -> Maybe Int32 -> TestM ResponseLBS
getConvIds u r s = do
  g <- view tsGalley
  get $
    g
      . path "/conversations/ids"
      . zUser u
      . zConn "conn"
      . zType "access"
      . convRange r s

postMembers :: UserId -> List1 UserId -> ConvId -> TestM ResponseLBS
postMembers u us c = do
  g <- view tsGalley
  let i = newInvite (makeIdOpaque <$> us)
  post $
    g
      . paths ["conversations", toByteString' c, "members"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json i

postMembersWithRole :: UserId -> List1 UserId -> ConvId -> RoleName -> TestM ResponseLBS
postMembersWithRole u us c r = do
  g <- view tsGalley
  let i = (newInvite (makeIdOpaque <$> us)) {invRoleName = r}
  post $
    g
      . paths ["conversations", toByteString' c, "members"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json i

deleteMember :: UserId -> UserId -> ConvId -> TestM ResponseLBS
deleteMember u1 u2 c = do
  g <- view tsGalley
  delete $
    g
      . zUser u1
      . paths ["conversations", toByteString' c, "members", toByteString' u2]
      . zConn "conn"
      . zType "access"

getSelfMember :: UserId -> ConvId -> TestM ResponseLBS
getSelfMember u c = do
  g <- view tsGalley
  get $
    g
      . paths ["conversations", toByteString' c, "self"]
      . zUser u
      . zConn "conn"
      . zType "access"

putMember :: UserId -> MemberUpdate -> ConvId -> TestM ResponseLBS
putMember u m c = do
  g <- view tsGalley
  put $
    g
      . paths ["conversations", toByteString' c, "self"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json m

putOtherMember :: UserId -> UserId -> OtherMemberUpdate -> ConvId -> TestM ResponseLBS
putOtherMember from to m c = do
  g <- view tsGalley
  put $
    g
      . paths ["conversations", toByteString' c, "members", toByteString' to]
      . zUser from
      . zConn "conn"
      . zType "access"
      . json m

putConversationName :: UserId -> ConvId -> Text -> TestM ResponseLBS
putConversationName u c n = do
  g <- view tsGalley
  let update = ConversationRename n
  put
    ( g
        . paths ["conversations", toByteString' c]
        . zUser u
        . zConn "conn"
        . zType "access"
        . json update
    )

putReceiptMode :: UserId -> ConvId -> ReceiptMode -> TestM ResponseLBS
putReceiptMode u c r = do
  g <- view tsGalley
  let update = ConversationReceiptModeUpdate r
  put
    ( g
        . paths ["conversations", toByteString' c, "receipt-mode"]
        . zUser u
        . zConn "conn"
        . zType "access"
        . json update
    )

postJoinConv :: UserId -> ConvId -> TestM ResponseLBS
postJoinConv u c = do
  g <- view tsGalley
  post $
    g
      . paths ["/conversations", toByteString' c, "join"]
      . zUser u
      . zConn "conn"
      . zType "access"

postJoinCodeConv :: UserId -> ConversationCode -> TestM ResponseLBS
postJoinCodeConv u j = do
  g <- view tsGalley
  post $
    g
      . paths ["/conversations", "join"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json j

putAccessUpdate :: UserId -> ConvId -> ConversationAccessUpdate -> TestM ResponseLBS
putAccessUpdate u c acc = do
  g <- view tsGalley
  put $
    g
      . paths ["/conversations", toByteString' c, "access"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json acc

putMessageTimerUpdate ::
  UserId -> ConvId -> ConversationMessageTimerUpdate -> TestM ResponseLBS
putMessageTimerUpdate u c acc = do
  g <- view tsGalley
  put $
    g
      . paths ["/conversations", toByteString' c, "message-timer"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json acc

postConvCode :: UserId -> ConvId -> TestM ResponseLBS
postConvCode u c = do
  g <- view tsGalley
  post $
    g
      . paths ["/conversations", toByteString' c, "code"]
      . zUser u
      . zConn "conn"
      . zType "access"

postConvCodeCheck :: ConversationCode -> TestM ResponseLBS
postConvCodeCheck code = do
  g <- view tsGalley
  post $
    g
      . path "/conversations/code-check"
      . json code

getConvCode :: UserId -> ConvId -> TestM ResponseLBS
getConvCode u c = do
  g <- view tsGalley
  get $
    g
      . paths ["/conversations", toByteString' c, "code"]
      . zUser u
      . zConn "conn"
      . zType "access"

deleteConvCode :: UserId -> ConvId -> TestM ResponseLBS
deleteConvCode u c = do
  g <- view tsGalley
  delete $
    g
      . paths ["/conversations", toByteString' c, "code"]
      . zUser u
      . zConn "conn"
      . zType "access"

deleteClientInternal :: UserId -> ClientId -> TestM ResponseLBS
deleteClientInternal u c = do
  g <- view tsGalley
  delete $
    g
      . zUser u
      . zConn "conn"
      . paths ["i", "clients", toByteString' c]

deleteUser :: HasCallStack => UserId -> TestM ()
deleteUser u = do
  g <- view tsGalley
  delete (g . path "/i/user" . zUser u) !!! const 200 === statusCode

getTeamQueue :: HasCallStack => UserId -> Maybe NotificationId -> Maybe (Int, Bool) -> Bool -> TestM [(NotificationId, UserId)]
getTeamQueue zusr msince msize onlyLast = do
  parseEventList . responseJsonUnsafe
    <$> ( getTeamQueue' zusr msince (fst <$> msize) onlyLast
            <!! const 200 === statusCode
        )
  where
    parseEventList :: QueuedNotificationList -> [(NotificationId, UserId)]
    parseEventList qnl
      | isJust msize && qnl ^. queuedHasMore /= (snd $ fromJust msize) =
        error $ "expected has_more: " <> show (snd $ fromJust msize) <> "; but found: " <> show (qnl ^. queuedHasMore)
      | qnl ^. queuedTime /= Nothing =
        error $ "expected time: Nothing; but found: " <> show (qnl ^. queuedTime)
      | otherwise =
        fmap (_2 %~ parseEvt) . mconcat . fmap parseEvts . view queuedNotifications $ qnl
    --
    parseEvts :: QueuedNotification -> [(NotificationId, Object)]
    parseEvts qn = (qn ^. queuedNotificationId,) <$> (toList . toNonEmpty $ qn ^. queuedNotificationPayload)
    --
    parseEvt :: Object -> UserId
    parseEvt o = case fromJSON (Object o) of
      (Error msg) -> error msg
      (Success (e :: TE.Event)) ->
        case e ^. TE.eventData of
          Just (EdMemberJoin uid) -> uid
          _ -> error ("bad even type: " <> show (e ^. TE.eventType))

getTeamQueue' :: HasCallStack => UserId -> Maybe NotificationId -> Maybe Int -> Bool -> TestM ResponseLBS
getTeamQueue' zusr msince msize onlyLast = do
  g <- view tsGalley
  get
    ( g . path "/teams/notifications"
        . zUser zusr
        . zConn "conn"
        . zType "access"
        . query
          [ ("since", toByteString' <$> msince),
            ("size", toByteString' <$> msize),
            ("last", if onlyLast then Just "true" else Nothing)
          ]
    )

-------------------------------------------------------------------------------
-- Common Assertions

assertConvMemberWithRole :: HasCallStack => RoleName -> ConvId -> UserId -> TestM ()
assertConvMemberWithRole r c u =
  getSelfMember u c !!! do
    const 200 === statusCode
    const (Right (makeIdOpaque u)) === (fmap memId <$> responseJsonEither)
    const (Right r) === (fmap memConvRoleName <$> responseJsonEither)

assertConvMember :: HasCallStack => UserId -> ConvId -> TestM ()
assertConvMember u c =
  getSelfMember u c !!! do
    const 200 === statusCode
    const (Right (makeIdOpaque u)) === (fmap memId <$> responseJsonEither)

assertNotConvMember :: HasCallStack => UserId -> ConvId -> TestM ()
assertNotConvMember u c =
  getSelfMember u c !!! do
    const 200 === statusCode
    const (Right Null) === responseJsonEither

assertConvEquals :: (HasCallStack, MonadIO m) => Conversation -> Conversation -> m ()
assertConvEquals c1 c2 = liftIO $ do
  assertEqual "id" (cnvId c1) (cnvId c2)
  assertEqual "type" (cnvType c1) (cnvType c2)
  assertEqual "creator" (cnvCreator c1) (cnvCreator c2)
  assertEqual "access" (accessSet c1) (accessSet c2)
  assertEqual "name" (cnvName c1) (cnvName c2)
  assertEqual "self member" (selfMember c1) (selfMember c2)
  assertEqual "other members" (otherMembers c1) (otherMembers c2)
  where
    accessSet = Set.fromList . toList . cnvAccess
    selfMember = cmSelf . cnvMembers
    otherMembers = Set.fromList . cmOthers . cnvMembers

assertConv ::
  HasCallStack =>
  Response (Maybe Lazy.ByteString) ->
  ConvType ->
  UserId ->
  UserId ->
  [UserId] ->
  Maybe Text ->
  Maybe Milliseconds ->
  TestM ConvId
assertConv r t c s us n mt = assertConvWithRole r t c s us n mt roleNameWireAdmin

assertConvWithRole ::
  HasCallStack =>
  Response (Maybe Lazy.ByteString) ->
  ConvType ->
  UserId ->
  UserId ->
  [UserId] ->
  Maybe Text ->
  Maybe Milliseconds ->
  RoleName ->
  TestM ConvId
assertConvWithRole r t c s us n mt role = do
  cId <- fromBS $ getHeader' "Location" r
  let cnv = responseJsonMaybe @Conversation r
  let _self = cmSelf . cnvMembers <$> cnv
  let others = cmOthers . cnvMembers <$> cnv
  liftIO $ do
    assertEqual "id" (Just cId) (cnvId <$> cnv)
    assertEqual "name" n (cnv >>= cnvName)
    assertEqual "type" (Just t) (cnvType <$> cnv)
    assertEqual "creator" (Just c) (cnvCreator <$> cnv)
    assertEqual "message_timer" (Just mt) (cnvMessageTimer <$> cnv)
    assertEqual "self" (Just (makeIdOpaque s)) (memId <$> _self)
    assertEqual "others" (Just . Set.fromList $ makeIdOpaque <$> us) (Set.fromList . map omId . toList <$> others)
    assertEqual "creator is always and admin" (Just roleNameWireAdmin) (memConvRoleName <$> _self)
    assertBool "others role" (all (\x -> x == role) $ fromMaybe (error "Cannot be null") ((map omConvRoleName . toList <$> others)))
    assertBool "otr muted not false" (Just False == (memOtrMuted <$> _self))
    assertBool "otr muted ref not empty" (isNothing (memOtrMutedRef =<< _self))
    assertBool "otr archived not false" (Just False == (memOtrArchived <$> _self))
    assertBool "otr archived ref not empty" (isNothing (memOtrArchivedRef =<< _self))
    case t of
      SelfConv -> assertEqual "access" (Just privateAccess) (cnvAccess <$> cnv)
      ConnectConv -> assertEqual "access" (Just privateAccess) (cnvAccess <$> cnv)
      One2OneConv -> assertEqual "access" (Just privateAccess) (cnvAccess <$> cnv)
      _ -> return ()
  return cId

wsAssertOtr :: ConvId -> UserId -> ClientId -> ClientId -> Text -> Notification -> IO ()
wsAssertOtr = wsAssertOtr' "data"

wsAssertOtr' :: Text -> ConvId -> UserId -> ClientId -> ClientId -> Text -> Notification -> IO ()
wsAssertOtr' evData conv usr from to txt n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= conv
  evtType e @?= OtrMessageAdd
  evtFrom e @?= usr
  evtData e @?= Just (EdOtrMessage (OtrMessage from to txt (Just evData)))

-- | This assumes the default role name
wsAssertMemberJoin :: ConvId -> UserId -> [UserId] -> Notification -> IO ()
wsAssertMemberJoin conv usr new n = wsAssertMemberJoinWithRole conv usr new roleNameWireAdmin n

wsAssertMemberJoinWithRole :: ConvId -> UserId -> [UserId] -> RoleName -> Notification -> IO ()
wsAssertMemberJoinWithRole conv usr new role n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= conv
  evtType e @?= MemberJoin
  evtFrom e @?= usr
  evtData e @?= Just (EdMembersJoin $ SimpleMembers (fmap (\x -> SimpleMember x role) new))

wsAssertMemberUpdateWithRole :: ConvId -> UserId -> UserId -> RoleName -> Notification -> IO ()
wsAssertMemberUpdateWithRole conv usr target role n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= conv
  evtType e @?= MemberStateUpdate
  evtFrom e @?= usr
  case evtData e of
    Just (Galley.Types.EdMemberUpdate mis) -> do
      assertEqual "target" (Just target) (misTarget mis)
      assertEqual "conversation_role" (Just role) (misConvRoleName mis)
    x -> assertFailure $ "Unexpected event data: " ++ show x

wsAssertConvAccessUpdate :: ConvId -> UserId -> ConversationAccessUpdate -> Notification -> IO ()
wsAssertConvAccessUpdate conv usr new n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= conv
  evtType e @?= ConvAccessUpdate
  evtFrom e @?= usr
  evtData e @?= Just (EdConvAccessUpdate new)

wsAssertConvMessageTimerUpdate :: ConvId -> UserId -> ConversationMessageTimerUpdate -> Notification -> IO ()
wsAssertConvMessageTimerUpdate conv usr new n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= conv
  evtType e @?= ConvMessageTimerUpdate
  evtFrom e @?= usr
  evtData e @?= Just (EdConvMessageTimerUpdate new)

wsAssertMemberLeave :: ConvId -> UserId -> [UserId] -> Notification -> IO ()
wsAssertMemberLeave conv usr old n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= conv
  evtType e @?= MemberLeave
  evtFrom e @?= usr
  sorted (evtData e) @?= sorted (Just (EdMembersLeave (UserIdList old)))
  where
    sorted (Just (EdMembersLeave (UserIdList m))) = Just (EdMembersLeave (UserIdList (sort m)))
    sorted x = x

assertNoMsg :: HasCallStack => WS.WebSocket -> (Notification -> Assertion) -> TestM ()
assertNoMsg ws f = do
  x <- WS.awaitMatch (1 # Second) ws f
  liftIO $ case x of
    Left _ -> return () -- expected
    Right _ -> assertFailure "Unexpected message"

-------------------------------------------------------------------------------
-- Helpers

testResponse :: HasCallStack => Int -> Maybe TestErrorLabel -> Assertions ()
testResponse status mlabel = do
  const status === statusCode
  case mlabel of
    Just label -> responseJsonEither === const (Right label)
    Nothing -> (isLeft <$> responseJsonEither @TestErrorLabel) === const True

newtype TestErrorLabel = TestErrorLabel {fromTestErrorLabel :: ST}
  deriving (Eq, Show)

instance IsString TestErrorLabel where
  fromString = TestErrorLabel . cs

instance FromJSON TestErrorLabel where
  parseJSON = fmap TestErrorLabel . withObject "TestErrorLabel" (.: "label")

decodeConvCode :: Response (Maybe Lazy.ByteString) -> ConversationCode
decodeConvCode = responseJsonUnsafe

decodeConvCodeEvent :: Response (Maybe Lazy.ByteString) -> ConversationCode
decodeConvCodeEvent r = case responseJsonUnsafe r of
  (Event ConvCodeUpdate _ _ _ (Just (EdConvCodeUpdate c))) -> c
  _ -> error "Failed to parse ConversationCode from Event"

decodeConvId :: Response (Maybe Lazy.ByteString) -> ConvId
decodeConvId = cnvId . responseJsonUnsafe

decodeConvList :: Response (Maybe Lazy.ByteString) -> [Conversation]
decodeConvList = convList . responseJsonUnsafeWithMsg "conversations"

decodeConvIdList :: Response (Maybe Lazy.ByteString) -> [ConvId]
decodeConvIdList = convList . responseJsonUnsafeWithMsg "conversation-ids"

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . toByteString'

zBot :: UserId -> Request -> Request
zBot = header "Z-Bot" . toByteString'

zConn :: ByteString -> Request -> Request
zConn = header "Z-Connection"

zProvider :: ProviderId -> Request -> Request
zProvider = header "Z-Provider" . toByteString'

zConv :: ConvId -> Request -> Request
zConv = header "Z-Conversation" . toByteString'

zType :: ByteString -> Request -> Request
zType = header "Z-Type"

-- TODO: it'd be nicer to just take a list here and handle the cases with 0
-- users differently
connectUsers :: UserId -> List1 UserId -> TestM ()
connectUsers u us = void $ connectUsersWith expect2xx u us

connectUsersUnchecked ::
  UserId ->
  List1 UserId ->
  TestM (List1 (Response (Maybe Lazy.ByteString), Response (Maybe Lazy.ByteString)))
connectUsersUnchecked = connectUsersWith id

connectUsersWith ::
  (Request -> Request) ->
  UserId ->
  List1 UserId ->
  TestM (List1 (Response (Maybe Lazy.ByteString), Response (Maybe Lazy.ByteString)))
connectUsersWith fn u us = mapM connectTo us
  where
    connectTo v = do
      b <- view tsBrig
      r1 <-
        post
          ( b
              . zUser u
              . zConn "conn"
              . path "/connections"
              . json (ConnectionRequest (makeIdOpaque v) "chat" (Message "Y"))
              . fn
          )
      r2 <-
        put
          ( b
              . zUser v
              . zConn "conn"
              . paths ["connections", toByteString' u]
              . json (ConnectionUpdate Accepted)
              . fn
          )
      return (r1, r2)

-- | A copy of 'putConnection' from Brig integration tests.
putConnection :: UserId -> UserId -> Relation -> TestM ResponseLBS
putConnection from to r = do
  b <- view tsBrig
  put $
    b
      . paths ["/connections", toByteString' to]
      . contentJson
      . body payload
      . zUser from
      . zConn "conn"
  where
    payload = RequestBodyLBS . encode $ object ["status" .= r]

randomUsers :: Int -> TestM [UserId]
randomUsers n = replicateM n randomUser

randomUser :: HasCallStack => TestM UserId
randomUser = randomUser' False True True

randomTeamCreator :: HasCallStack => TestM UserId
randomTeamCreator = randomUser' True True True

randomUser' :: HasCallStack => Bool -> Bool -> Bool -> TestM UserId
randomUser' isCreator hasPassword hasEmail = do
  b <- view tsBrig
  e <- liftIO randomEmail
  let p =
        object $
          ["name" .= fromEmail e]
            <> ["password" .= defPassword | hasPassword]
            <> ["email" .= fromEmail e | hasEmail]
            <> ["team" .= (Team.BindingNewTeam $ Team.newNewTeam (unsafeRange "teamName") (unsafeRange "defaultIcon")) | isCreator]
  r <- post (b . path "/i/users" . json p) <!! const 201 === statusCode
  fromBS (getHeader' "Location" r)

ephemeralUser :: HasCallStack => TestM UserId
ephemeralUser = do
  b <- view tsBrig
  name <- UUID.toText <$> liftIO nextRandom
  let p = object ["name" .= name]
  r <- post (b . path "/register" . json p) <!! const 201 === statusCode
  user <- responseJsonError r
  return $ Brig.Types.userId user

randomClient :: HasCallStack => UserId -> LastPrekey -> TestM ClientId
randomClient uid lk = do
  b <- view tsBrig
  resp <-
    post (b . paths ["i", "clients", toByteString' uid] . json newClientBody)
      <!! const rStatus === statusCode
  client <- responseJsonError resp
  return (clientId client)
  where
    cType = PermanentClientType
    rStatus = 201
    newClientBody =
      (newClient cType lk)
        { newClientPassword = Just defPassword
        }

ensureDeletedState :: HasCallStack => Bool -> UserId -> UserId -> TestM ()
ensureDeletedState check from u = do
  state <- getDeletedState from u
  liftIO $ assertEqual "Unxpected deleted state" state (Just check)

getDeletedState :: HasCallStack => UserId -> UserId -> TestM (Maybe Bool)
getDeletedState from u = do
  b <- view tsBrig
  fmap profileDeleted . responseJsonMaybe
    <$> get
      ( b
          . paths ["users", toByteString' u]
          . zUser from
          . zConn "conn"
      )

getClients :: UserId -> TestM ResponseLBS
getClients u = do
  b <- view tsBrig
  get $
    b
      . paths ["clients"]
      . zUser u
      . zConn "conn"

-- TODO: Refactor, as used also in brig
deleteClient :: UserId -> ClientId -> Maybe PlainTextPassword -> TestM ResponseLBS
deleteClient u c pw = do
  b <- view tsBrig
  delete $
    b
      . paths ["clients", toByteString' c]
      . zUser u
      . zConn "conn"
      . contentJson
      . body payload
  where
    payload =
      RequestBodyLBS . encode $
        object
          [ "password" .= pw
          ]

-- TODO: Refactor, as used also in brig
isUserDeleted :: HasCallStack => UserId -> TestM Bool
isUserDeleted u = do
  b <- view tsBrig
  r <-
    get (b . paths ["i", "users", toByteString' u, "status"])
      <!! const 200 === statusCode
  case responseBody r of
    Nothing -> error $ "getStatus: failed to parse response: " ++ show r
    Just j -> do
      let st = maybeFromJSON =<< (j ^? key "status")
      let decoded = fromMaybe (error $ "getStatus: failed to decode status" ++ show j) st
      return $ decoded == Deleted
  where
    maybeFromJSON :: FromJSON a => Value -> Maybe a
    maybeFromJSON v = case fromJSON v of
      Success a -> Just a
      _ -> Nothing

isMember :: UserId -> ConvId -> TestM Bool
isMember usr cnv = do
  g <- view tsGalley
  res <-
    get $
      g
        . paths ["i", "conversations", toByteString' cnv, "members", toByteString' usr]
        . expect2xx
  return $ isJust (responseJsonMaybe @Member res)

randomUserWithClient :: LastPrekey -> TestM (UserId, ClientId)
randomUserWithClient lk = do
  u <- randomUser
  c <- randomClient u lk
  return (u, c)

newNonce :: TestM (Id ())
newNonce = randomId

fromBS :: (HasCallStack, FromByteString a, Monad m) => ByteString -> m a
fromBS = maybe (fail "fromBS: no parse") return . fromByteString

convRange :: Maybe (Either [ConvId] ConvId) -> Maybe Int32 -> Request -> Request
convRange range size =
  maybe id (queryItem "size" . C.pack . show) size
    . case range of
      Just (Left l) -> queryItem "ids" (C.intercalate "," $ map toByteString' l)
      Just (Right c) -> queryItem "start" (toByteString' c)
      Nothing -> id

privateAccess :: [Access]
privateAccess = [PrivateAccess]

eqMismatch ::
  [(UserId, Set ClientId)] ->
  [(UserId, Set ClientId)] ->
  [(UserId, Set ClientId)] ->
  Maybe ClientMismatch ->
  Bool
eqMismatch _ _ _ Nothing = False
eqMismatch mssd rdnt dltd (Just other) =
  userClients mssd == missingClients other
    && userClients rdnt == redundantClients other
    && userClients dltd == deletedClients other
  where
    userClients :: [(UserId, Set ClientId)] -> UserClients
    userClients = UserClients . Map.mapKeys makeIdOpaque . Map.fromList

otrRecipients :: [(UserId, [(ClientId, Text)])] -> OtrRecipients
otrRecipients = OtrRecipients . UserClientMap . buildMap
  where
    buildMap = fmap Map.fromList . Map.mapKeys makeIdOpaque . Map.fromList

encodeCiphertext :: ByteString -> Text
encodeCiphertext = decodeUtf8 . B64.encode

genRandom :: (Q.Arbitrary a, MonadIO m) => m a
genRandom = liftIO . Q.generate $ Q.arbitrary

defPassword :: PlainTextPassword
defPassword = PlainTextPassword "secret"

randomEmail :: MonadIO m => m Email
randomEmail = do
  uid <- liftIO nextRandom
  return $ Email ("success+" <> UUID.toText uid) "simulator.amazonses.com"

selfConv :: UserId -> Id C
selfConv u = Id (toUUID u)

-- TODO: Refactor, as used also in other services
retryWhileN :: (MonadIO m) => Int -> (a -> Bool) -> m a -> m a
retryWhileN n f m =
  retrying
    (constantDelay 1000000 <> limitRetries n)
    (const (return . f))
    (const m)

-- | Changing this will break tests; all prekeys and client Id must match the same
-- fingerprint
someClientId :: ClientId
someClientId = ClientId "1dbfbe22c8a35cb2"

-- | Changing these will break tests; all prekeys and client Id must match the same
-- fingerprint
somePrekeys :: [Prekey]
somePrekeys =
  [ Prekey (PrekeyId 1) "pQABAQECoQBYIOjl7hw0D8YRNqkkBQETCxyr7/ywE/2R5RWcUPM+GJACA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 2) "pQABAQICoQBYIGoXawUQWQ9ZW+MXhvuo9ALOBUjLff8S5VdAokN29C1OA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 3) "pQABAQMCoQBYIEjdt+YWd3lHmG8pamULLMubAMZw556IO8kW7s1MLFytA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 4) "pQABAQQCoQBYIPIaOA3Xqfk4Lh2/pU88Owd2eW5eplHpywr+Mx4QGyiMA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 5) "pQABAQUCoQBYIHnafNR4Gh3ID71lYzToewEVag4EKskDFq+gaeraOlSJA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 6) "pQABAQYCoQBYIFXUkVftE7kK22waAzhOjOmJVex3EBTU8RHZFx2o1Ed8A6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 7) "pQABAQcCoQBYIDXdN8VlKb5lbgPmoDPLPyqNIEyShG4oT/DlW0peRRZUA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 8) "pQABAQgCoQBYIJH1ewvIVV3yGqQvdr/QM9HARzMgo5ksOTRyKEuN2aZzA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 9) "pQABAQkCoQBYIFcAnXdx0M1Q1hoDDfgMK9r+Zchn8YlVHHaQwQYhRk1dA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 10) "pQABAQoCoQBYIGs3vyxwmzEZ+qKNy4wpFkxc+Bgkb0D76ZEbxeeh/9DVA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 11) "pQABAQsCoQBYIGUiBeOJALP5dkMduUZ/u6MDhHNrsrBUa3f0YlSSWZbzA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 12) "pQABAQwCoQBYIMp6QNNTPDZgL3DSSD/QWWnBI7LsTZp2RhY/HLqnIwRZA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 13) "pQABAQ0CoQBYIJXSSUrE5RCNyB5pg+m6vGwK7RvJ+rs9dsdHitxnfDhuA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 14) "pQABAQ4CoQBYIHmtOX7jCKBHFDysb4H0z/QWoCSaEyjerZaT/HOP8bgDA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 15) "pQABAQ8CoQBYIIaMCTcPKj2HuYQ7i9ZaxUw9j5Bz8TPjoAaTZ5eB0w1kA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 16) "pQABARACoQBYIHWAOacKuWH81moJVveJ0FSfipWocfspOIBhaU6VLWUsA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 17) "pQABARECoQBYIA8XtUXtnMxQslULnNAeHBIivlLRe/+qdh2j6nTfDAchA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 18) "pQABARICoQBYIGgzg6SzgTTOgnk48pa6y2Rgjy004DkeBo4CMld3Jlr6A6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 19) "pQABARMCoQBYIEoEFiIpCHgn74CAD+GhIfIgbQtdCqQqkOXHWxRlG6Y6A6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 20) "pQABARQCoQBYINVEwTRxNSe0rxZxon4Rifz2l4rtQZn7mHtKYCiFAK9IA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 21) "pQABARUCoQBYIN3aeX2Ayi2rPFbiaYb+O2rdHUpFhzRs2j28pCmbGpflA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 22) "pQABARYCoQBYIJe5OJ17YKQrNmIH3sE++r++4Z5ld36axqAMjjQ3jtQWA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 23) "pQABARcCoQBYIASE94LjK6Raipk/lN/YewouqO+kcQGpxIqP+iW2hyHiA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY=",
    Prekey (PrekeyId 24) "pQABARgYAqEAWCBZ222LpS6/99Btlw+83PihrA655skwsNevt//8oz5axQOhAKEAWCCy39UyMEgetquvTo7P19bcyfnWBzQMOEG1v+0wub0magT2",
    Prekey (PrekeyId 25) "pQABARgZAqEAWCDGEwo61w4O8T8lyw0HdoOjGWBKQUNqo6+jSfrPR9alrAOhAKEAWCCy39UyMEgetquvTo7P19bcyfnWBzQMOEG1v+0wub0magT2",
    Prekey (PrekeyId 26) "pQABARgaAqEAWCBMSQoQ6B35plC80i1O3AWlJSftCEbCbju97Iykg5+NWQOhAKEAWCCy39UyMEgetquvTo7P19bcyfnWBzQMOEG1v+0wub0magT2"
  ]

-- | Changing these will break tests; all prekeys and client Id must match the same
-- fingerprint
someLastPrekeys :: [LastPrekey]
someLastPrekeys =
  [ lastPrekey "pQABARn//wKhAFggnCcZIK1pbtlJf4wRQ44h4w7/sfSgj5oWXMQaUGYAJ/sDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggwO2any+CjiGP8XFYrY67zHPvLgp+ysY5k7vci57aaLwDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggoChErA5oTI5JT769hJV+VINmU8kougGdYqGd2U7hPa8DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggPLk4BBJ8THVLGm7r0K7EJITRlJnt6bpNzM9GTNRYcCcDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggqHASsRlZ1i8dESXRXBL2OvR+0yGUtqK9vJfzol1E+osDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggx/N1YhKXSJYJQxhWgHSA4ASaJKIHDJfmEnojfnp9VQ8DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggVL6QIpoqmtKxmB8HToiAPxfjSDEzJEUAoFKfhXou06YDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggRs74/ViOrHN+aS2RbGCwC0sJv1Sp/Q0pmRB15s9DCBMDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFggtNO/hrwzt9M/1X6eK2sG6YFmA7BDqlFMEipbZOsg0vcDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g==",
    lastPrekey "pQABARn//wKhAFgg1rZEY6vbAnEz+Ern5kRny/uKiIrXTb/usQxGnceV2HADoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g=="
  ]

-- | ES is only refreshed occasionally; we don't want to wait for that in tests.
refreshIndex :: TestM ()
refreshIndex = do
  brig <- view tsBrig
  post (brig . path "/i/index/refresh") !!! const 200 === statusCode

postSSOUser :: Text -> Bool -> UserSSOId -> TeamId -> TestM ResponseLBS
postSSOUser name hasEmail ssoid teamid = do
  brig <- view tsBrig
  email <- randomEmail
  let o =
        object $
          [ "name" .= name,
            "cookie" .= defCookieLabel,
            "sso_id" .= ssoid,
            "team_id" .= teamid
          ]
            <> ["email" .= fromEmail email | hasEmail]
      bdy = Bilge.json o
  post (brig . path "/i/users" . bdy)

defCookieLabel :: CookieLabel
defCookieLabel = CookieLabel "auth"

-- | This allows you to run requests against a galley instantiated using the given options.
--   Note that ONLY 'galley' calls should occur within the provided action, calls to other
--   services will fail.
withSettingsOverrides :: MonadIO m => Opts.Opts -> WaiTest.Session a -> m a
withSettingsOverrides opts action = liftIO $ do
  (galleyApp, _) <- Run.mkApp opts
  WaiTest.runSession action galleyApp

waitForMemberDeletion :: UserId -> TeamId -> UserId -> TestM ()
waitForMemberDeletion zusr tid uid = do
  maybeTimedOut <- timeout 2000000 loop
  liftIO $
    when (isNothing maybeTimedOut) $
      assertFailure "Timed out waiting for member deletion"
  where
    loop = do
      galley <- view tsGalley
      res <- get (galley . paths ["teams", toByteString' tid, "members", toByteString' uid] . zUser zusr)
      case statusCode res of
        404 -> pure ()
        _ -> loop

deleteTeamMember :: (MonadIO m, MonadCatch m, MonadHttp m) => (Request -> Request) -> TeamId -> UserId -> UserId -> m ()
deleteTeamMember g tid owner deletee =
  delete
    ( g
        . paths ["teams", toByteString' tid, "members", toByteString' deletee]
        . zUser owner
        . zConn "conn"
        . json (newTeamMemberDeleteData (Just defPassword))
    )
    !!! do
      const 202 === statusCode
