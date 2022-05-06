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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module API.Util where

import qualified API.SQS as SQS
import Bilge hiding (timeout)
import Bilge.Assert
import Bilge.TestSession
import Brig.Types
import Brig.Types.Intra (UserAccount (..), UserSet (..))
import Brig.Types.Team.Invitation
import Brig.Types.User.Auth (CookieLabel (..))
import Control.Concurrent.Async
import Control.Exception (throw)
import Control.Lens hiding (from, to, (#), (.=))
import Control.Monad.Catch (MonadCatch, MonadMask, finally)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Retry (constantDelay, exponentialBackoff, limitRetries, retrying)
import Data.Aeson hiding (json)
import Data.Aeson.Lens (key, _String)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Char8 as C8
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.CaseInsensitive as CI
import qualified Data.Code as Code
import qualified Data.Currency as Currency
import Data.Default
import Data.Domain
import qualified Data.Handle as Handle
import qualified Data.HashMap.Strict as HashMap
import Data.Id
import Data.Json.Util hiding ((#))
import Data.LegalHold (defUserLegalHoldStatus)
import Data.List.NonEmpty (NonEmpty)
import Data.List1 as List1
import qualified Data.Map as LMap
import qualified Data.Map.Strict as Map
import Data.Misc
import qualified Data.ProtoLens as Protolens
import Data.ProtocolBuffers (encodeMessage)
import Data.Qualified
import Data.Range
import qualified Data.Sequence as Seq
import Data.Serialize (runPut)
import qualified Data.Set as Set
import Data.Singletons
import Data.String.Conversions (ST, cs)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy.Encoding as T
import Data.Time (getCurrentTime)
import Data.Tuple.Extra
import qualified Data.UUID as UUID
import Data.UUID.V4
import Federator.MockServer (FederatedRequest (..))
import qualified Federator.MockServer as Mock
import GHC.TypeLits
import Galley.Intra.User (chunkify)
import qualified Galley.Options as Opts
import qualified Galley.Run as Run
import Galley.Types
import qualified Galley.Types as Conv
import Galley.Types.Conversations.Intra
import Galley.Types.Conversations.One2One (one2OneConvId)
import Galley.Types.Conversations.Roles hiding (DeleteConversation)
import Galley.Types.Teams hiding (Event, EventType (..), self)
import qualified Galley.Types.Teams as Team
import Galley.Types.Teams.Intra
import Galley.Types.UserList
import Imports
import Network.HTTP.Media.MediaType
import Network.HTTP.Media.RenderHeader (renderHeader)
import Network.HTTP.Types (http11, renderQuery)
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Application, defaultRequest)
import qualified Network.Wai as Wai
import qualified Network.Wai.Test as Wai
import qualified Network.Wai.Test as WaiTest
import Servant (Handler, HasServer, Server, ServerT, serve, (:<|>) (..))
import Servant.Client (ClientError (FailureResponse))
import qualified Servant.Client as Servant
import Servant.Client.Core (RunClient (throwClientError))
import qualified Servant.Client.Core as Servant
import qualified Servant.Client.Core.Request as ServantRequest
import System.Exit
import System.Process
import System.Random
import qualified Test.QuickCheck as Q
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers (viewFederationDomain)
import TestSetup
import UnliftIO.Timeout
import Util.Options
import Web.Cookie
import Wire.API.Conversation
import Wire.API.Conversation.Action
import Wire.API.Event.Conversation
import qualified Wire.API.Event.Team as TE
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Domain (originDomainHeaderName)
import Wire.API.Internal.Notification
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
import Wire.API.MLS.Serialisation
import Wire.API.Message
import qualified Wire.API.Message.Proto as Proto
import Wire.API.Routes.Internal.Brig.Connection
import qualified Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti as Multi
import Wire.API.Routes.MultiTablePaging
import Wire.API.Team (Icon (..))
import Wire.API.Team.Feature
import Wire.API.Team.Member (mkNewTeamMember)
import Wire.API.User.Client
import qualified Wire.API.User.Client as Client
import Wire.API.User.Identity (mkSimpleSampleUref)

-------------------------------------------------------------------------------
-- API Operations

-- | A class for monads with access to a Sem r instance
class HasGalley m where
  viewGalley :: m GalleyR
  viewGalleyOpts :: m Opts.Opts

instance HasGalley TestM where
  viewGalley = view tsGalley
  viewGalleyOpts = view tsGConf

instance (HasGalley m, Monad m) => HasGalley (SessionT m) where
  viewGalley = lift viewGalley
  viewGalleyOpts = lift viewGalleyOpts

symmPermissions :: [Perm] -> Permissions
symmPermissions p = let s = Set.fromList p in fromJust (newPermissions s s)

createBindingTeam :: HasCallStack => TestM (UserId, TeamId)
createBindingTeam = do
  (first Brig.Types.userId) <$> createBindingTeam'

createBindingTeam' :: HasCallStack => TestM (User, TeamId)
createBindingTeam' = do
  owner <- randomTeamCreator'
  teams <- getTeams (Brig.Types.userId owner) []
  let [team] = view teamListTeams teams
  let tid = view teamId team
  SQS.assertQueue "create team" SQS.tActivate
  refreshIndex
  pure (owner, tid)

createBindingTeamWithMembers :: HasCallStack => Int -> TestM (TeamId, UserId, [UserId])
createBindingTeamWithMembers numUsers = do
  (owner, tid) <- createBindingTeam
  members <- forM [2 .. numUsers] $ \n -> do
    mem <- addUserToTeam owner tid
    SQS.assertQueue "add member" $ SQS.tUpdate (fromIntegral n) [owner]
    -- 'refreshIndex' needs to happen here to make tests more realistic.  one effect of
    -- refreshing the index once at the end would be that the hard member limit wouldn't hold
    -- any more.
    refreshIndex
    return $ view Galley.Types.Teams.userId mem

  return (tid, owner, members)

createBindingTeamWithQualifiedMembers :: HasCallStack => Int -> TestM (TeamId, Qualified UserId, [Qualified UserId])
createBindingTeamWithQualifiedMembers num = do
  localDomain <- viewFederationDomain
  (tid, owner, users) <- createBindingTeamWithMembers num
  pure (tid, Qualified owner localDomain, map (`Qualified` localDomain) users)

getTeams :: UserId -> [(ByteString, Maybe ByteString)] -> TestM TeamList
getTeams u queryItems = do
  g <- view tsGalley
  r <-
    get
      ( g
          . paths ["teams"]
          . query queryItems
          . zUser u
          . zConn "conn"
          . zType "access"
          . expect2xx
      )
  return $ responseJsonUnsafe r

createBindingTeamWithNMembers :: Int -> TestM (UserId, TeamId, [UserId])
createBindingTeamWithNMembers = createBindingTeamWithNMembersWithHandles False

createBindingTeamWithNMembersWithHandles :: Bool -> Int -> TestM (UserId, TeamId, [UserId])
createBindingTeamWithNMembersWithHandles withHandles n = do
  (owner, tid) <- createBindingTeam
  setHandle owner
  mems <- replicateM n $ do
    member1 <- randomUser
    addTeamMemberInternal tid member1 (rolePermissions RoleMember) Nothing
    setHandle member1
    pure member1
  SQS.ensureQueueEmpty
  pure (owner, tid, mems)
  where
    mkRandomHandle :: MonadIO m => m Text
    mkRandomHandle = liftIO $ do
      nrs <- replicateM 21 (randomRIO (97, 122)) -- a-z
      return (cs (map chr nrs))

    setHandle :: UserId -> TestM ()
    setHandle uid = when withHandles $ do
      b <- view tsBrig
      randomHandle <- mkRandomHandle
      put
        ( b
            . paths ["/i/users", toByteString' uid, "handle"]
            . json (HandleUpdate randomHandle)
        )
        !!! do
          const 200 === statusCode

-- | FUTUREWORK: this is dead code (see 'NonBindingNewTeam').  remove!
createNonBindingTeam :: HasCallStack => Text -> UserId -> [TeamMember] -> TestM TeamId
createNonBindingTeam name owner mems = do
  g <- view tsGalley
  let mm = if null mems then Nothing else Just $ unsafeRange (take 127 mems)
  let nt = NonBindingNewTeam $ newNewTeam (unsafeRange name) DefaultIcon & newTeamMembers .~ mm
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
  let nt = BindingNewTeam $ newNewTeam (unsafeRange name) DefaultIcon
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

-- alternative to 'ResponseLBS': [BodyReader](https://hoogle.zinfra.io/file/root/.stack/snapshots/x86_64-linux/82492d944a85db90f4cd7cec6f4d5215ef9ac1ac8aeffeed4a805fbd6b1232c5/8.8.4/doc/http-client-0.7.0/Network-HTTP-Client.html#t:BodyReader)
getTeamMembersCsv :: HasCallStack => UserId -> TeamId -> TestM ResponseLBS
getTeamMembersCsv usr tid = do
  g <- view tsGalley
  get (g . accept "text/csv" . paths ["teams", toByteString' tid, "members/csv"] . zUser usr) <!! do
    const 200 === statusCode
    const (Just "chunked") === lookup "Transfer-Encoding" . responseHeaders

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
getTeamMember getter tid gettee = do
  g <- view tsGalley
  getTeamMember' g getter tid gettee

getTeamMember' :: (HasCallStack, MonadHttp m, MonadIO m, MonadCatch m) => GalleyR -> UserId -> TeamId -> UserId -> m TeamMember
getTeamMember' g getter tid gettee = do
  r <- get (g . paths ["teams", toByteString' tid, "members", toByteString' gettee] . zUser getter) <!! const 200 === statusCode
  responseJsonError r

getTeamMemberInternal :: HasCallStack => TeamId -> UserId -> TestM TeamMember
getTeamMemberInternal tid mid = do
  g <- view tsGalley
  r <- get (g . paths ["i", "teams", toByteString' tid, "members", toByteString' mid]) <!! const 200 === statusCode
  responseJsonError r

addTeamMember :: HasCallStack => UserId -> TeamId -> UserId -> Permissions -> Maybe (UserId, UTCTimeMillis) -> TestM ()
addTeamMember usr tid muid mperms mmbinv = do
  g <- view tsGalley
  let payload = json (mkNewTeamMember muid mperms mmbinv)
  post (g . paths ["teams", toByteString' tid, "members"] . zUser usr . zConn "conn" . payload)
    !!! const 200 === statusCode

-- | FUTUREWORK: do not use this, it's broken!!  use 'addUserToTeam' instead!  https://wearezeta.atlassian.net/browse/SQSERVICES-471
addTeamMemberInternal :: HasCallStack => TeamId -> UserId -> Permissions -> Maybe (UserId, UTCTimeMillis) -> TestM ()
addTeamMemberInternal tid muid mperms mmbinv = addTeamMemberInternal' tid muid mperms mmbinv !!! const 200 === statusCode

-- | FUTUREWORK: do not use this, it's broken!!  use 'addUserToTeam' instead!  https://wearezeta.atlassian.net/browse/SQSERVICES-471
addTeamMemberInternal' :: HasCallStack => TeamId -> UserId -> Permissions -> Maybe (UserId, UTCTimeMillis) -> TestM ResponseLBS
addTeamMemberInternal' tid muid mperms mmbinv = do
  g <- view tsGalley
  let payload = json (mkNewTeamMember muid mperms mmbinv)
  post (g . paths ["i", "teams", toByteString' tid, "members"] . payload)

addUserToTeam :: HasCallStack => UserId -> TeamId -> TestM TeamMember
addUserToTeam = addUserToTeamWithRole Nothing

addUserToTeam' :: HasCallStack => UserId -> TeamId -> TestM ResponseLBS
addUserToTeam' u t = snd <$> addUserToTeamWithRole' Nothing u t

addUserToTeamWithRole :: HasCallStack => Maybe Role -> UserId -> TeamId -> TestM TeamMember
addUserToTeamWithRole role inviter tid = do
  (inv, rsp2) <- addUserToTeamWithRole' role inviter tid
  let invitee :: User = responseJsonUnsafe rsp2
      inviteeId = Brig.Types.userId invitee
  let invmeta = Just (inviter, inCreatedAt inv)
  mem <- getTeamMember inviter tid inviteeId
  liftIO $ assertEqual "Member has no/wrong invitation metadata" invmeta (mem ^. Team.invitation)
  let zuid = parseSetCookie <$> getHeader "Set-Cookie" rsp2
  liftIO $ assertEqual "Wrong cookie" (Just "zuid") (setCookieName <$> zuid)
  pure mem

addUserToTeamWithRole' :: HasCallStack => Maybe Role -> UserId -> TeamId -> TestM (Invitation, ResponseLBS)
addUserToTeamWithRole' role inviter tid = do
  brig <- view tsBrig
  inviteeEmail <- randomEmail
  let invite = InvitationRequest Nothing role Nothing inviteeEmail Nothing
  invResponse <- postInvitation tid inviter invite
  inv <- responseJsonError invResponse
  inviteeCode <- getInvitationCode tid (inInvitation inv)
  r <-
    post
      ( brig . path "/register"
          . contentJson
          . body (acceptInviteBody inviteeEmail inviteeCode)
      )
  return (inv, r)

addUserToTeamWithSSO :: HasCallStack => Bool -> TeamId -> TestM TeamMember
addUserToTeamWithSSO hasEmail tid = do
  let ssoid = UserSSOId mkSimpleSampleUref
  user <- responseJsonError =<< postSSOUser "SSO User" hasEmail ssoid tid
  let uid = Brig.Types.userId user
  getTeamMember uid tid uid

makeOwner :: HasCallStack => UserId -> TeamMember -> TeamId -> TestM ()
makeOwner owner mem tid = do
  galley <- view tsGalley
  let changeMember = mkNewTeamMember (mem ^. Team.userId) fullPermissions (mem ^. Team.invitation)
  put
    ( galley
        . paths ["teams", toByteString' tid, "members"]
        . zUser owner
        . zConn "conn"
        . json changeMember
    )
    !!! const 200
    === statusCode

acceptInviteBody :: Email -> InvitationCode -> RequestBody
acceptInviteBody email code =
  RequestBodyLBS . encode $
    object
      [ "name" .= Name "bob",
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

getInvitationCode :: HasCallStack => TeamId -> InvitationId -> TestM InvitationCode
getInvitationCode t ref = do
  brig <- view tsBrig

  let getm :: TestM (Maybe InvitationCode)
      getm = do
        r <-
          get
            ( brig
                . path "/i/teams/invitation-code"
                . queryItem "team" (toByteString' t)
                . queryItem "invitation_id" (toByteString' ref)
            )
        let lbs = fromMaybe "" $ responseBody r
        return $ fromByteString . Text.encodeUtf8 =<< lbs ^? key "code" . _String

  fromMaybe (error "No code?")
    <$> retrying
      (constantDelay 800000 <> limitRetries 3)
      (\_ -> pure . isNothing)
      (const getm)

-- Note that here we don't make use of the datatype because NewConv has a default
-- and therefore cannot be unset. However, given that this is to test the legacy
-- API (i.e., no roles) it's fine to hardcode the JSON object in the test since
-- it clearly shows the API that old(er) clients use.
createTeamConvLegacy :: HasCallStack => UserId -> TeamId -> [UserId] -> Maybe Text -> TestM ConvId
createTeamConvLegacy u tid us name = do
  g <- view tsGalley
  let tinfo = ConvTeamInfo tid
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

createTeamConv ::
  HasCallStack =>
  UserId ->
  TeamId ->
  [UserId] ->
  Maybe Text ->
  Maybe (Set Access) ->
  Maybe Milliseconds ->
  TestM ConvId
createTeamConv u tid us name acc mtimer = createTeamConvAccess u tid us name acc Nothing mtimer (Just roleNameWireAdmin)

createTeamConvWithRole ::
  HasCallStack =>
  UserId ->
  TeamId ->
  [UserId] ->
  Maybe Text ->
  Maybe (Set Access) ->
  Maybe Milliseconds ->
  RoleName ->
  TestM ConvId
createTeamConvWithRole u tid us name acc mtimer convRole = createTeamConvAccess u tid us name acc Nothing mtimer (Just convRole)

createTeamConvAccess ::
  HasCallStack =>
  UserId ->
  TeamId ->
  [UserId] ->
  Maybe Text ->
  Maybe (Set Access) ->
  Maybe (Set AccessRoleV2) ->
  Maybe Milliseconds ->
  Maybe RoleName ->
  TestM ConvId
createTeamConvAccess u tid us name acc role mtimer convRole = do
  r <- createTeamConvAccessRaw u tid us name acc role mtimer convRole <!! const 201 === statusCode
  fromBS (getHeader' "Location" r)

createTeamConvAccessRaw ::
  UserId ->
  TeamId ->
  [UserId] ->
  Maybe Text ->
  Maybe (Set Access) ->
  Maybe (Set AccessRoleV2) ->
  Maybe Milliseconds ->
  Maybe RoleName ->
  TestM ResponseLBS
createTeamConvAccessRaw u tid us name acc role mtimer convRole = do
  g <- view tsGalley
  let tinfo = ConvTeamInfo tid
  let conv =
        NewConv us [] (name >>= checked) (fromMaybe (Set.fromList []) acc) role (Just tinfo) mtimer Nothing (fromMaybe roleNameWireAdmin convRole) ProtocolProteusTag
  post
    ( g
        . path "/conversations"
        . zUser u
        . zConn "conn"
        . zType "access"
        . json conv
    )

-- | Create a team MLS conversation
createMLSTeamConv ::
  (HasGalley m, MonadIO m, MonadHttp m, HasCallStack) =>
  Local UserId ->
  TeamId ->
  UserList UserId ->
  Maybe Text ->
  Maybe (Set Access) ->
  Maybe (Set AccessRoleV2) ->
  Maybe Milliseconds ->
  Maybe RoleName ->
  m (Local ConvId)
createMLSTeamConv lusr tid users name access role timer convRole = do
  g <- viewGalley
  let conv =
        NewConv
          { newConvUsers = [],
            newConvQualifiedUsers = ulAll lusr users,
            newConvName = name >>= checked,
            newConvAccess = fromMaybe Set.empty access,
            newConvAccessRoles = role,
            newConvTeam = Just . ConvTeamInfo $ tid,
            newConvMessageTimer = timer,
            newConvUsersRole = fromMaybe roleNameWireAdmin convRole,
            newConvReceiptMode = Nothing,
            newConvProtocol = ProtocolMLSTag
          }
  r <-
    post
      ( g
          . path "/conversations"
          . zUser (tUnqualified lusr)
          . zConn "conn"
          . zType "access"
          . json conv
      )
  convId <- fromBS (getHeader' "Location" r)
  pure $ qualifyAs lusr convId

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

createOne2OneTeamConv :: UserId -> UserId -> Maybe Text -> TeamId -> TestM ResponseLBS
createOne2OneTeamConv u1 u2 n tid = do
  g <- view tsGalley
  let conv =
        NewConv [u2] [] (n >>= checked) mempty Nothing (Just $ ConvTeamInfo tid) Nothing Nothing roleNameWireAdmin ProtocolProteusTag
  post $ g . path "/conversations/one2one" . zUser u1 . zConn "conn" . zType "access" . json conv

postConv ::
  UserId ->
  [UserId] ->
  Maybe Text ->
  [Access] ->
  Maybe (Set AccessRoleV2) ->
  Maybe Milliseconds ->
  TestM ResponseLBS
postConv u us name a r mtimer = postConvWithRole u us name a r mtimer roleNameWireAdmin

defNewProteusConv :: NewConv
defNewProteusConv = NewConv [] [] Nothing mempty Nothing Nothing Nothing Nothing roleNameWireAdmin ProtocolProteusTag

defNewMLSConv :: NewConv
defNewMLSConv = defNewProteusConv {newConvProtocol = ProtocolMLSTag}

postConvQualified ::
  (HasCallStack, HasGalley m, MonadIO m, MonadMask m, MonadHttp m) =>
  UserId ->
  NewConv ->
  m ResponseLBS
postConvQualified u n = do
  g <- viewGalley
  post $
    g
      . path "/conversations"
      . zUser u
      . zConn "conn"
      . zType "access"
      . json n

postConvWithRemoteUsers ::
  HasCallStack =>
  UserId ->
  NewConv ->
  TestM (Response (Maybe LByteString))
postConvWithRemoteUsers u n =
  fmap fst $
    withTempMockFederator (const ()) $
      postConvQualified u n {newConvName = setName (newConvName n)}
        <!! const 201 === statusCode
  where
    setName :: Within Text n m => Maybe (Range n m Text) -> Maybe (Range n m Text)
    setName Nothing = checked "federated gossip"
    setName x = x

postTeamConv :: TeamId -> UserId -> [UserId] -> Maybe Text -> [Access] -> Maybe (Set AccessRoleV2) -> Maybe Milliseconds -> TestM ResponseLBS
postTeamConv tid u us name a r mtimer = do
  g <- view tsGalley
  let conv = NewConv us [] (name >>= checked) (Set.fromList a) r (Just (ConvTeamInfo tid)) mtimer Nothing roleNameWireAdmin ProtocolProteusTag
  post $ g . path "/conversations" . zUser u . zConn "conn" . zType "access" . json conv

deleteTeamConv :: (HasGalley m, MonadIO m, MonadHttp m) => TeamId -> ConvId -> UserId -> m ResponseLBS
deleteTeamConv tid convId zusr = do
  g <- viewGalley
  delete
    ( g
        . paths ["teams", toByteString' tid, "conversations", toByteString' convId]
        . zUser zusr
        . zConn "conn"
    )

postConvWithRole ::
  UserId ->
  [UserId] ->
  Maybe Text ->
  [Access] ->
  Maybe (Set AccessRoleV2) ->
  Maybe Milliseconds ->
  RoleName ->
  TestM ResponseLBS
postConvWithRole u members name access arole timer role =
  postConvQualified
    u
    defNewProteusConv
      { newConvUsers = members,
        newConvName = name >>= checked,
        newConvAccess = Set.fromList access,
        newConvAccessRoles = arole,
        newConvMessageTimer = timer,
        newConvUsersRole = role
      }

postConvWithReceipt :: UserId -> [UserId] -> Maybe Text -> [Access] -> Maybe (Set AccessRoleV2) -> Maybe Milliseconds -> ReceiptMode -> TestM ResponseLBS
postConvWithReceipt u us name a r mtimer rcpt = do
  g <- view tsGalley
  let conv = NewConv us [] (name >>= checked) (Set.fromList a) r Nothing mtimer (Just rcpt) roleNameWireAdmin ProtocolProteusTag
  post $ g . path "/conversations" . zUser u . zConn "conn" . zType "access" . json conv

postSelfConv :: UserId -> TestM ResponseLBS
postSelfConv u = do
  g <- view tsGalley
  post $ g . path "/conversations/self" . zUser u . zConn "conn" . zType "access"

postO2OConv :: UserId -> UserId -> Maybe Text -> TestM ResponseLBS
postO2OConv u1 u2 n = do
  g <- view tsGalley
  let conv = NewConv [u2] [] (n >>= checked) mempty Nothing Nothing Nothing Nothing roleNameWireAdmin ProtocolProteusTag
  post $ g . path "/conversations/one2one" . zUser u1 . zConn "conn" . zType "access" . json conv

postConnectConv :: UserId -> UserId -> Text -> Text -> Maybe Text -> TestM ResponseLBS
postConnectConv a b name msg email = do
  qb <- pure (Qualified b) <*> viewFederationDomain
  g <- view tsGalley
  post $
    g
      . path "/i/conversations/connect"
      . zUser a
      . zConn "conn"
      . zType "access"
      . json (Connect qb (Just msg) (Just name) email)

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
  Maybe [UserId] ->
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
      . json (mkOtrPayload d rec reportMissing "ZXhhbXBsZQ==")

postProteusMessageQualifiedWithMockFederator ::
  UserId ->
  ClientId ->
  Qualified ConvId ->
  [(Qualified UserId, ClientId, ByteString)] ->
  ByteString ->
  ClientMismatchStrategy ->
  (Domain -> ServerT (FedApi 'Brig) Handler) ->
  (Domain -> ServerT (FedApi 'Galley) Handler) ->
  TestM (ResponseLBS, [FederatedRequest])
postProteusMessageQualifiedWithMockFederator senderUser senderClient convId recipients dat strat brigApi galleyApi = do
  localDomain <- viewFederationDomain
  withTempServantMockFederator brigApi galleyApi localDomain $
    postProteusMessageQualified senderUser senderClient convId recipients dat strat

postProteusMessageQualified ::
  (MonadIO m, HasGalley m, MonadHttp m) =>
  UserId ->
  ClientId ->
  Qualified ConvId ->
  [(Qualified UserId, ClientId, ByteString)] ->
  ByteString ->
  ClientMismatchStrategy ->
  m ResponseLBS
postProteusMessageQualified senderUser senderClient (Qualified conv domain) recipients dat strat = do
  g <- viewGalley
  let protoMsg = mkQualifiedOtrPayload senderClient recipients dat strat
  post $
    g
      . paths ["conversations", toByteString' domain, toByteString' conv, "proteus", "messages"]
      . zUser senderUser
      . zConn "conn"
      . zType "access"
      . contentProtobuf
      . bytes (Protolens.encodeMessage protoMsg)

data BroadcastAPI
  = BroadcastLegacyQueryParams
  | BroadcastLegacyBody
  | BroadcastQualified

broadcastAPIName :: BroadcastAPI -> String
broadcastAPIName BroadcastLegacyQueryParams = "legacy API with query parameters only"
broadcastAPIName BroadcastLegacyBody = "legacy API with report_missing in the body"
broadcastAPIName BroadcastQualified = "qualified API"

data BroadcastType = BroadcastJSON | BroadcastProto

broadcastTypeName :: BroadcastType -> String
broadcastTypeName BroadcastJSON = "json"
broadcastTypeName BroadcastProto = "protobuf"

data Broadcast = Broadcast
  { bAPI :: BroadcastAPI,
    bType :: BroadcastType,
    bMessage :: [(UserId, ClientId, Text)],
    bData :: Text,
    bReport :: Maybe [UserId],
    bReq :: Request -> Request
  }

instance Default Broadcast where
  def = Broadcast BroadcastLegacyQueryParams BroadcastJSON mempty "ZXhhbXBsZQ==" mempty id

postBroadcast ::
  (MonadIO m, MonadHttp m, HasGalley m) =>
  Qualified UserId ->
  ClientId ->
  Broadcast ->
  m ResponseLBS
postBroadcast lu c b = do
  let u = qUnqualified lu
  g <- viewGalley
  let (bodyReport, queryReport) = case bAPI b of
        BroadcastLegacyQueryParams -> (Nothing, maybe id mkOtrReportMissing (bReport b))
        _ -> (bReport b, id)
  let bdy = case (bAPI b, bType b) of
        (BroadcastQualified, BroadcastJSON) -> error "JSON not supported for the qualified broadcast API"
        (BroadcastQualified, BroadcastProto) ->
          let m =
                Protolens.encodeMessage $
                  mkQualifiedOtrPayload
                    c
                    (map ((_1 %~ (lu $>)) . (_3 %~ fromBase64TextLenient)) (bMessage b))
                    (fromBase64TextLenient (bData b))
                    ( maybe
                        MismatchReportAll
                        (MismatchReportOnly . Set.fromList . map (lu $>))
                        (bReport b)
                    )
           in contentProtobuf . bytes m
        (_, BroadcastJSON) -> json (mkOtrPayload c (bMessage b) bodyReport (bData b))
        (_, BroadcastProto) ->
          let m =
                runPut . encodeMessage $
                  mkOtrProtoMessage c (otrRecipients (bMessage b)) bodyReport (bData b)
           in contentProtobuf . bytes m
  let name = case bAPI b of BroadcastQualified -> "proteus"; _ -> "otr"
  post $
    g . bReq b
      . paths ["broadcast", name, "messages"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . queryReport
      . bdy

mkOtrReportMissing :: [UserId] -> Request -> Request
mkOtrReportMissing = queryItem "report_missing" . BS.intercalate "," . map toByteString'

mkOtrPayload :: ClientId -> [(UserId, ClientId, Text)] -> Maybe [UserId] -> Text -> Value
mkOtrPayload sender rec reportMissingBody ad =
  object
    [ "sender" .= sender,
      "recipients" .= (HashMap.map toJSON . HashMap.fromListWith HashMap.union $ map mkOtrMessage rec),
      "data" .= Just ad,
      "report_missing" .= reportMissingBody
    ]

mkOtrMessage :: (UserId, ClientId, Text) -> (Text, HashMap.HashMap Text Text)
mkOtrMessage (usr, clt, m) = (fn usr, HashMap.singleton (fn clt) m)
  where
    fn :: (FromByteString a, ToByteString a) => a -> Text
    fn = fromJust . fromByteString . toByteString'

postProtoOtrMessage :: UserId -> ClientId -> ConvId -> OtrRecipients -> TestM ResponseLBS
postProtoOtrMessage = postProtoOtrMessage' Nothing id

postProtoOtrMessage' :: Maybe [UserId] -> (Request -> Request) -> UserId -> ClientId -> ConvId -> OtrRecipients -> TestM ResponseLBS
postProtoOtrMessage' reportMissing modif u d c rec = do
  g <- view tsGalley
  let m = runPut (encodeMessage $ mkOtrProtoMessage d rec reportMissing "ZXhhbXBsZQ==")
   in post $
        g
          . modif
          . paths ["conversations", toByteString' c, "otr", "messages"]
          . zUser u
          . zConn "conn"
          . zType "access"
          . contentProtobuf
          . bytes m

mkOtrProtoMessage :: ClientId -> OtrRecipients -> Maybe [UserId] -> Text -> Proto.NewOtrMessage
mkOtrProtoMessage sender rec reportMissing ad =
  let rcps = protoFromOtrRecipients rec
      sndr = Proto.fromClientId sender
      rmis = Proto.fromUserId <$> fromMaybe [] reportMissing
   in Proto.newOtrMessage sndr rcps
        & Proto.newOtrMessageData ?~ fromBase64TextLenient ad
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

listConvs :: (MonadIO m, MonadHttp m, HasGalley m) => UserId -> ListConversations -> m ResponseLBS
listConvs u req = do
  g <- viewGalley
  post $
    g
      . path "/conversations/list/v2"
      . zUser u
      . zConn "conn"
      . zType "access"
      . json req

getConv :: (MonadIO m, MonadHttp m, HasGalley m, HasCallStack) => UserId -> ConvId -> m ResponseLBS
getConv u c = do
  g <- viewGalley
  get $
    g
      . paths ["conversations", toByteString' c]
      . zUser u
      . zConn "conn"
      . zType "access"

getConvQualified :: (MonadIO m, MonadHttp m, HasGalley m, HasCallStack) => UserId -> Qualified ConvId -> m ResponseLBS
getConvQualified u (Qualified conv domain) = do
  g <- viewGalley
  get $
    g
      . paths ["conversations", toByteString' domain, toByteString' conv]
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

listConvIds :: UserId -> GetPaginatedConversationIds -> TestM ResponseLBS
listConvIds u paginationOpts = do
  g <- view tsGalley
  post $
    g
      . path "/conversations/list-ids"
      . zUser u
      . json paginationOpts

-- | Does not page through conversation list
listRemoteConvs :: Domain -> UserId -> TestM [Qualified ConvId]
listRemoteConvs remoteDomain uid = do
  let paginationOpts = GetPaginatedConversationIds Nothing (toRange (Proxy @100))
  allConvs <- fmap mtpResults . responseJsonError @_ @ConvIdsPage =<< listConvIds uid paginationOpts <!! const 200 === statusCode
  pure $ filter (\qcnv -> qDomain qcnv == remoteDomain) allConvs

postQualifiedMembers ::
  (HasGalley m, MonadIO m, MonadHttp m) =>
  UserId ->
  NonEmpty (Qualified UserId) ->
  ConvId ->
  m ResponseLBS
postQualifiedMembers zusr invitees conv = do
  g <- viewGalley
  let invite = InviteQualified invitees roleNameWireAdmin
  post $
    g
      . paths ["conversations", toByteString' conv, "members", "v2"]
      . zUser zusr
      . zConn "conn"
      . zType "access"
      . json invite

postMembers :: UserId -> List1 UserId -> ConvId -> TestM ResponseLBS
postMembers u us c = do
  g <- view tsGalley
  let i = newInvite us
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
  let i = (newInvite us) {invRoleName = r}
  post $
    g
      . paths ["conversations", toByteString' c, "members"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json i

deleteMemberUnqualified :: HasCallStack => UserId -> UserId -> ConvId -> TestM ResponseLBS
deleteMemberUnqualified u1 u2 c = do
  g <- view tsGalley
  delete $
    g
      . zUser u1
      . paths ["conversations", toByteString' c, "members", toByteString' u2]
      . zConn "conn"
      . zType "access"

deleteMemberQualified ::
  (HasCallStack, MonadIO m, MonadHttp m, HasGalley m) =>
  UserId ->
  Qualified UserId ->
  Qualified ConvId ->
  m ResponseLBS
deleteMemberQualified u1 (Qualified u2 u2Domain) (Qualified conv convDomain) = do
  g <- viewGalley
  delete $
    g
      . zUser u1
      . paths
        [ "conversations",
          toByteString' convDomain,
          toByteString' conv,
          "members",
          toByteString' u2Domain,
          toByteString' u2
        ]
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

putMember :: UserId -> MemberUpdate -> Qualified ConvId -> TestM ResponseLBS
putMember u m (Qualified c dom) = do
  g <- view tsGalley
  put $
    g
      . paths ["conversations", toByteString' dom, toByteString' c, "self"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json m

putOtherMemberQualified ::
  (HasGalley m, MonadIO m, MonadHttp m) =>
  UserId ->
  Qualified UserId ->
  OtherMemberUpdate ->
  Qualified ConvId ->
  m ResponseLBS
putOtherMemberQualified from to m c = do
  g <- viewGalley
  put $
    g
      . paths
        [ "conversations",
          toByteString' (qDomain c),
          toByteString' (qUnqualified c),
          "members",
          toByteString' (qDomain to),
          toByteString' (qUnqualified to)
        ]
      . zUser from
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

putQualifiedConversationName ::
  (HasCallStack, HasGalley m, MonadIO m, MonadHttp m, MonadMask m) =>
  UserId ->
  Qualified ConvId ->
  Text ->
  m ResponseLBS
putQualifiedConversationName u c n = do
  g <- viewGalley
  let update = ConversationRename n
  put
    ( g
        . paths
          [ "conversations",
            toByteString' (qDomain c),
            toByteString' (qUnqualified c),
            "name"
          ]
        . zUser u
        . zConn "conn"
        . zType "access"
        . json update
    )

putConversationName :: UserId -> ConvId -> Text -> TestM ResponseLBS
putConversationName u c n = do
  g <- view tsGalley
  let update = ConversationRename n
  put
    ( g
        . paths ["conversations", toByteString' c, "name"]
        . zUser u
        . zConn "conn"
        . zType "access"
        . json update
    )

putQualifiedReceiptMode ::
  (MonadIO m, MonadHttp m, HasGalley m, HasCallStack) =>
  UserId ->
  Qualified ConvId ->
  ReceiptMode ->
  m ResponseLBS
putQualifiedReceiptMode u (Qualified c dom) r = do
  g <- viewGalley
  let update = ConversationReceiptModeUpdate r
  put
    ( g
        . paths ["conversations", toByteString' dom, toByteString' c, "receipt-mode"]
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

getJoinCodeConv :: UserId -> Code.Key -> Code.Value -> TestM ResponseLBS
getJoinCodeConv u k v = do
  g <- view tsGalley
  get $
    g
      . paths ["/conversations", "join"]
      . zUser u
      . queryItem "key" (toByteString' k)
      . queryItem "code" (toByteString' v)

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

putAccessUpdate :: UserId -> ConvId -> ConversationAccessData -> TestM ResponseLBS
putAccessUpdate u c acc = do
  g <- view tsGalley
  put $
    g
      . paths ["/conversations", toByteString' c, "access"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json acc

putQualifiedAccessUpdate ::
  (MonadHttp m, HasGalley m, MonadIO m) =>
  UserId ->
  Qualified ConvId ->
  ConversationAccessData ->
  m ResponseLBS
putQualifiedAccessUpdate u (Qualified c domain) acc = do
  g <- viewGalley
  put $
    g
      . paths ["/conversations", toByteString' domain, toByteString' c, "access"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json acc

putMessageTimerUpdateQualified ::
  (HasGalley m, MonadIO m, MonadHttp m) =>
  UserId ->
  Qualified ConvId ->
  ConversationMessageTimerUpdate ->
  m ResponseLBS
putMessageTimerUpdateQualified u c acc = do
  g <- viewGalley
  put $
    g
      . paths
        [ "/conversations",
          toByteString' (qDomain c),
          toByteString' (qUnqualified c),
          "message-timer"
        ]
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

deleteUser :: (MonadIO m, MonadCatch m, MonadHttp m, HasGalley m, HasCallStack) => UserId -> m ResponseLBS
deleteUser u = do
  g <- viewGalley
  delete (g . path "/i/user" . zUser u)

getTeamQueue :: HasCallStack => UserId -> Maybe NotificationId -> Maybe (Int, Bool) -> Bool -> TestM [(NotificationId, UserId)]
getTeamQueue zusr msince msize onlyLast =
  parseEventList . responseJsonUnsafe
    <$> ( getTeamQueue' zusr msince (fst <$> msize) onlyLast
            <!! const 200 === statusCode
        )
  where
    parseEventList :: QueuedNotificationList -> [(NotificationId, UserId)]
    parseEventList qnl
      | isJust msize && qnl ^. queuedHasMore /= snd (fromJust msize) =
        error $ "expected has_more: " <> show (snd $ fromJust msize) <> "; but found: " <> show (qnl ^. queuedHasMore)
      | isJust (qnl ^. queuedTime) =
        error $ "expected time: Nothing; but found: " <> show (qnl ^. queuedTime)
      | otherwise =
        fmap (_2 %~ parseEvt) . mconcat . fmap parseEvts . view queuedNotifications $ qnl

    parseEvts :: QueuedNotification -> [(NotificationId, Object)]
    parseEvts qn = (qn ^. queuedNotificationId,) <$> (toList $ qn ^. queuedNotificationPayload)

    parseEvt :: Object -> UserId
    parseEvt o = case fromJSON (Object o) of
      (Error msg) -> error msg
      (Success (e :: TE.Event)) ->
        case e ^. TE.eventData of
          EdMemberJoin uid -> uid
          _ -> error ("bad event type: " <> show (TE.eventType e))

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

asOtherMember :: Qualified UserId -> OtherMember
asOtherMember quid = OtherMember quid Nothing roleNameWireMember

registerRemoteConv :: Qualified ConvId -> UserId -> Maybe Text -> Set OtherMember -> TestM ()
registerRemoteConv convId originUser name othMembers = do
  fedGalleyClient <- view tsFedGalleyClient
  now <- liftIO getCurrentTime
  runFedClient @"on-conversation-created" fedGalleyClient (qDomain convId) $
    NewRemoteConversation
      { rcTime = now,
        rcOrigUserId = originUser,
        rcCnvId = qUnqualified convId,
        rcCnvType = RegularConv,
        rcCnvAccess = [],
        rcCnvAccessRoles = Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole],
        rcCnvName = name,
        rcNonCreatorMembers = othMembers,
        rcMessageTimer = Nothing,
        rcReceiptMode = Nothing
      }

getFeatureStatusMulti :: TeamFeatureName -> Multi.TeamFeatureNoConfigMultiRequest -> TestM ResponseLBS
getFeatureStatusMulti feat req = do
  g <- view tsGalley
  post
    ( g . paths ["i", "features-multi-teams", toByteString' feat]
        . json req
    )

-------------------------------------------------------------------------------
-- Common Assertions

assertConvMemberWithRole :: HasCallStack => RoleName -> ConvId -> Qualified UserId -> TestM ()
assertConvMemberWithRole r c u =
  getSelfMember (qUnqualified u) c !!! do
    const 200 === statusCode
    const (Right u) === (fmap memId <$> responseJsonEither)
    const (Right r) === (fmap memConvRoleName <$> responseJsonEither)

assertConvMember :: HasCallStack => Qualified UserId -> ConvId -> TestM ()
assertConvMember u c =
  getSelfMember (qUnqualified u) c !!! do
    const 200 === statusCode
    const (Right u) === (fmap memId <$> responseJsonEither)

assertNotConvMember :: HasCallStack => UserId -> ConvId -> TestM ()
assertNotConvMember u c =
  getSelfMember u c !!! do
    const 200 === statusCode
    const (Right Null) === responseJsonEither

assertConvEquals :: (HasCallStack, MonadIO m) => Conversation -> Conversation -> m ()
assertConvEquals c1 c2 = liftIO $ do
  assertEqual "id" (cnvQualifiedId c1) (cnvQualifiedId c2)
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
  Qualified UserId ->
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
  Qualified UserId ->
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
    assertEqual "id" (Just cId) (qUnqualified . cnvQualifiedId <$> cnv)
    assertEqual "name" n (cnv >>= cnvName)
    assertEqual "type" (Just t) (cnvType <$> cnv)
    assertEqual "creator" (Just c) (cnvCreator <$> cnv)
    assertEqual "message_timer" (Just mt) (cnvMessageTimer <$> cnv)
    assertEqual "self" (Just s) (memId <$> _self)
    assertEqual "others" (Just . Set.fromList $ us) (Set.fromList . map (qUnqualified . omQualifiedId) . toList <$> others)
    assertEqual "creator is always and admin" (Just roleNameWireAdmin) (memConvRoleName <$> _self)
    assertBool "others role" (all (== role) $ maybe (error "Cannot be null") (map omConvRoleName . toList) others)
    assertBool "otr muted ref not empty" (isNothing (memOtrMutedRef =<< _self))
    assertBool "otr archived not false" (Just False == (memOtrArchived <$> _self))
    assertBool "otr archived ref not empty" (isNothing (memOtrArchivedRef =<< _self))
    case t of
      SelfConv -> assertEqual "access" (Just privateAccess) (cnvAccess <$> cnv)
      ConnectConv -> assertEqual "access" (Just privateAccess) (cnvAccess <$> cnv)
      One2OneConv -> assertEqual "access" (Just privateAccess) (cnvAccess <$> cnv)
      _ -> return ()
  return cId

assertConvQualified ::
  HasCallStack =>
  Response (Maybe Lazy.ByteString) ->
  ConvType ->
  UserId ->
  Qualified UserId ->
  [Qualified UserId] ->
  Maybe Text ->
  Maybe Milliseconds ->
  TestM ConvId
assertConvQualified r t c s us n mt = assertConvQualifiedWithRole r t c s us n mt roleNameWireAdmin

assertConvQualifiedWithRole ::
  HasCallStack =>
  Response (Maybe Lazy.ByteString) ->
  ConvType ->
  UserId ->
  Qualified UserId ->
  [Qualified UserId] ->
  Maybe Text ->
  Maybe Milliseconds ->
  RoleName ->
  TestM ConvId
assertConvQualifiedWithRole r t c s us n mt role = do
  cId <- fromBS $ getHeader' "Location" r
  let cnv = responseJsonMaybe @Conversation r
  let _self = cmSelf . cnvMembers <$> cnv
  let others = cmOthers . cnvMembers <$> cnv
  liftIO $ do
    assertEqual "id" (Just cId) (qUnqualified . cnvQualifiedId <$> cnv)
    assertEqual "name" n (cnv >>= cnvName)
    assertEqual "type" (Just t) (cnvType <$> cnv)
    assertEqual "creator" (Just c) (cnvCreator <$> cnv)
    assertEqual "message_timer" (Just mt) (cnvMessageTimer <$> cnv)
    assertEqual "self" (Just s) (memId <$> _self)
    assertEqual "others" (Just . Set.fromList $ us) (Set.fromList . map omQualifiedId . toList <$> others)
    assertEqual "creator is always and admin" (Just roleNameWireAdmin) (memConvRoleName <$> _self)
    assertBool "others role" (all (== role) $ maybe (error "Cannot be null") (map omConvRoleName . toList) others)
    assertBool "otr muted ref not empty" (isNothing (memOtrMutedRef =<< _self))
    assertBool "otr archived not false" (Just False == (memOtrArchived <$> _self))
    assertBool "otr archived ref not empty" (isNothing (memOtrArchivedRef =<< _self))
    case t of
      SelfConv -> assertEqual "access" (Just privateAccess) (cnvAccess <$> cnv)
      ConnectConv -> assertEqual "access" (Just privateAccess) (cnvAccess <$> cnv)
      One2OneConv -> assertEqual "access" (Just privateAccess) (cnvAccess <$> cnv)
      _ -> return ()
  return cId

wsAssertOtr ::
  HasCallStack =>
  Qualified ConvId ->
  Qualified UserId ->
  ClientId ->
  ClientId ->
  Text ->
  Notification ->
  IO ()
wsAssertOtr = wsAssertOtr' "ZXhhbXBsZQ=="

wsAssertOtr' ::
  HasCallStack =>
  Text ->
  Qualified ConvId ->
  Qualified UserId ->
  ClientId ->
  ClientId ->
  Text ->
  Notification ->
  IO ()
wsAssertOtr' evData conv usr from to txt n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= conv
  evtType e @?= OtrMessageAdd
  evtFrom e @?= usr
  evtData e @?= EdOtrMessage (OtrMessage from to txt (Just evData))

wsAssertMLSWelcome ::
  HasCallStack =>
  Qualified UserId ->
  ByteString ->
  Notification ->
  IO ()
wsAssertMLSWelcome u welcome n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= fmap selfConv u
  evtType e @?= MLSWelcome
  evtFrom e @?= u
  evtData e @?= EdMLSWelcome welcome

wsAssertMLSMessage ::
  HasCallStack =>
  Qualified ConvId ->
  Qualified UserId ->
  ByteString ->
  Notification ->
  IO ()
wsAssertMLSMessage conv u message n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  assertMLSMessageEvent conv u message e

assertMLSMessageEvent ::
  HasCallStack =>
  Qualified ConvId ->
  Qualified UserId ->
  ByteString ->
  Event ->
  IO ()
assertMLSMessageEvent conv u message e = do
  evtConv e @?= conv
  evtType e @?= MLSMessageAdd
  evtFrom e @?= u
  evtData e @?= EdMLSMessage message

-- | This assumes the default role name
wsAssertMemberJoin :: HasCallStack => Qualified ConvId -> Qualified UserId -> [Qualified UserId] -> Notification -> IO ()
wsAssertMemberJoin conv usr new = wsAssertMemberJoinWithRole conv usr new roleNameWireAdmin

wsAssertMemberJoinWithRole :: HasCallStack => Qualified ConvId -> Qualified UserId -> [Qualified UserId] -> RoleName -> Notification -> IO ()
wsAssertMemberJoinWithRole conv usr new role n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  assertJoinEvent conv usr new role e

assertJoinEvent :: Qualified ConvId -> Qualified UserId -> [Qualified UserId] -> RoleName -> Event -> IO ()
assertJoinEvent conv usr new role e = do
  evtConv e @?= conv
  evtType e @?= Conv.MemberJoin
  evtFrom e @?= usr
  fmap (sort . mMembers) (evtData e ^? _EdMembersJoin) @?= Just (sort (fmap (`SimpleMember` role) new))

-- FUTUREWORK: See if this one can be implemented in terms of:
--
-- checkConvMemberLeaveEvent :: HasCallStack => Qualified ConvId -> Qualified UserId -> WS.WebSocket -> TestM ()
--
-- or if they can be combined in general.
wsAssertMembersLeave ::
  HasCallStack =>
  Qualified ConvId ->
  Qualified UserId ->
  [Qualified UserId] ->
  Notification ->
  IO ()
wsAssertMembersLeave conv usr leaving n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  assertLeaveEvent conv usr leaving e

assertLeaveEvent ::
  Qualified ConvId ->
  Qualified UserId ->
  [Qualified UserId] ->
  Event ->
  IO ()
assertLeaveEvent conv usr leaving e = do
  evtConv e @?= conv
  evtType e @?= Conv.MemberLeave
  evtFrom e @?= usr
  fmap (sort . qualifiedUserIdList) (evtData e ^? _EdMembersLeave) @?= Just (sort leaving)

wsAssertMemberUpdateWithRole :: Qualified ConvId -> Qualified UserId -> UserId -> RoleName -> Notification -> IO ()
wsAssertMemberUpdateWithRole conv usr target role n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= conv
  evtType e @?= MemberStateUpdate
  evtFrom e @?= usr
  case evtData e of
    Conv.EdMemberUpdate mis -> do
      assertEqual "target" (Qualified target (qDomain conv)) (misTarget mis)
      assertEqual "conversation_role" (Just role) (misConvRoleName mis)
    x -> assertFailure $ "Unexpected event data: " ++ show x

wsAssertConvAccessUpdate :: Qualified ConvId -> Qualified UserId -> ConversationAccessData -> Notification -> IO ()
wsAssertConvAccessUpdate conv usr new n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= conv
  evtType e @?= ConvAccessUpdate
  evtFrom e @?= usr
  evtData e @?= EdConvAccessUpdate new

wsAssertConvMessageTimerUpdate :: Qualified ConvId -> Qualified UserId -> ConversationMessageTimerUpdate -> Notification -> IO ()
wsAssertConvMessageTimerUpdate conv usr new n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= conv
  evtType e @?= ConvMessageTimerUpdate
  evtFrom e @?= usr
  evtData e @?= EdConvMessageTimerUpdate new

wsAssertMemberLeave :: Qualified ConvId -> Qualified UserId -> [Qualified UserId] -> Notification -> IO ()
wsAssertMemberLeave conv usr old n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= conv
  evtType e @?= Conv.MemberLeave
  evtFrom e @?= usr
  sorted (evtData e) @?= sorted (EdMembersLeave (QualifiedUserIdList old))
  where
    sorted (EdMembersLeave (QualifiedUserIdList m)) = EdMembersLeave (QualifiedUserIdList (sort m))
    sorted x = x

assertNoMsg :: HasCallStack => WS.WebSocket -> (Notification -> Assertion) -> TestM ()
assertNoMsg ws f = do
  x <- WS.awaitMatch (1 # Second) ws f
  liftIO $ case x of
    Left _ -> return () -- expected
    Right _ -> assertFailure "Unexpected message"

assertRemoveUpdate :: (MonadIO m, HasCallStack) => FederatedRequest -> Qualified ConvId -> Qualified UserId -> [UserId] -> Qualified UserId -> m ()
assertRemoveUpdate req qconvId remover alreadyPresentUsers victim = liftIO $ do
  frRPC req @?= "on-conversation-updated"
  frOriginDomain req @?= qDomain qconvId
  let Just cu = decode (frBody req)
  cuOrigUserId cu @?= remover
  cuConvId cu @?= qUnqualified qconvId
  sort (cuAlreadyPresentUsers cu) @?= sort alreadyPresentUsers
  cuAction cu @?= SomeConversationAction (sing @'ConversationRemoveMembersTag) (pure victim)

assertLeaveUpdate :: (MonadIO m, HasCallStack) => FederatedRequest -> Qualified ConvId -> Qualified UserId -> [UserId] -> Qualified UserId -> m ()
assertLeaveUpdate req qconvId remover alreadyPresentUsers victim = liftIO $ do
  frRPC req @?= "on-conversation-updated"
  frOriginDomain req @?= qDomain qconvId
  let Just cu = decode (frBody req)
  cuOrigUserId cu @?= remover
  cuConvId cu @?= qUnqualified qconvId
  sort (cuAlreadyPresentUsers cu) @?= sort alreadyPresentUsers
  cuAction cu @?= SomeConversationAction (sing @'ConversationLeaveTag) (pure victim)

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
  (Event _ _ _ (EdConvCodeUpdate c)) -> c
  _ -> error "Failed to parse ConversationCode from Event"

decodeConvId :: HasCallStack => Response (Maybe Lazy.ByteString) -> ConvId
decodeConvId = qUnqualified . decodeQualifiedConvId

decodeQualifiedConvId :: HasCallStack => Response (Maybe Lazy.ByteString) -> Qualified ConvId
decodeQualifiedConvId = cnvQualifiedId . responseJsonUnsafe

decodeConvList :: Response (Maybe Lazy.ByteString) -> [Conversation]
decodeConvList = convList . responseJsonUnsafeWithMsg "conversations"

decodeConvIdList :: Response (Maybe Lazy.ByteString) -> [ConvId]
decodeConvIdList = convList . responseJsonUnsafeWithMsg "conversation-ids"

decodeQualifiedConvIdList :: Response (Maybe Lazy.ByteString) -> Either String [Qualified ConvId]
decodeQualifiedConvIdList = fmap mtpResults . responseJsonEither @ConvIdsPage

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

-- TODO: it'd be nicer to just take a list here and handle the cases with 0
-- users differently
connectLocalQualifiedUsers :: UserId -> List1 (Qualified UserId) -> TestM ()
connectLocalQualifiedUsers u us = do
  localDomain <- viewFederationDomain
  let partitionMap = indexQualified . toList . toNonEmpty $ us
  -- FUTUREWORK: connect all users, not just those on the same domain as 'u'
  case LMap.lookup localDomain partitionMap of
    Nothing -> err
    Just [] -> err
    Just (x : xs) -> void $ connectUsersWith expect2xx u (list1 x xs)
  where
    err = liftIO . assertFailure $ "No user on the domain with " ++ show u

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
connectUsersWith fn u = mapM connectTo
  where
    connectTo v = do
      b <- view tsBrig
      r1 <-
        post
          ( b
              . zUser u
              . zConn "conn"
              . path "/connections"
              . json (ConnectionRequest v (unsafeRange "chat"))
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

connectWithRemoteUser ::
  (MonadReader TestSetup m, MonadIO m, MonadHttp m, MonadCatch m, HasCallStack) =>
  UserId ->
  Qualified UserId ->
  m ()
connectWithRemoteUser self other = do
  let req = CreateConnectionForTest self other
  b <- view tsBrig
  put
    ( b
        . zUser self
        . contentJson
        . zConn "conn"
        . paths ["i", "connections", "connection-update"]
        . json req
    )
    !!! const 200
    === statusCode

-- | A copy of 'postConnection' from Brig integration tests.
postConnection :: UserId -> UserId -> TestM ResponseLBS
postConnection from to = do
  brig <- view tsBrig
  post $
    brig
      . path "/connections"
      . contentJson
      . body payload
      . zUser from
      . zConn "conn"
  where
    payload =
      RequestBodyLBS . encode $
        ConnectionRequest to (unsafeRange "some conv name")

postConnectionQualified :: UserId -> Qualified UserId -> TestM ResponseLBS
postConnectionQualified from (Qualified toUser toDomain) = do
  brig <- view tsBrig
  post $
    brig
      . paths ["/connections", toByteString' toDomain, toByteString' toUser]
      . contentJson
      . zUser from
      . zConn "conn"

-- | A copy of 'putConnection' from Brig integration tests.
putConnection :: UserId -> UserId -> Relation -> TestM ResponseLBS
putConnection from to r = do
  brig <- view tsBrig
  put $
    brig
      . paths ["/connections", toByteString' to]
      . contentJson
      . body payload
      . zUser from
      . zConn "conn"
  where
    payload = RequestBodyLBS . encode $ object ["status" .= r]

-- | A slightly modified copy of 'putConnection' from above.
putConnectionQualified :: Qualified UserId -> UserId -> Relation -> TestM ResponseLBS
putConnectionQualified fromQualified to r = do
  localDomain <- viewFederationDomain
  let (Qualified from qualifiedDomain) = fromQualified
  liftIO $
    assertEqual
      "The qualified user's domain is not local"
      localDomain
      qualifiedDomain
  brig <- view tsBrig
  put $
    brig
      . paths ["/connections", toByteString' to]
      . contentJson
      . body payload
      . zUser from
      . zConn "conn"
  where
    payload = RequestBodyLBS . encode $ object ["status" .= r]

-- | A copy of `assertConnections from Brig integration tests.
assertConnections :: HasCallStack => UserId -> [ConnectionStatus] -> TestM ()
assertConnections u cstat = do
  brig <- view tsBrig
  resp <- listConnections brig u <!! const 200 === statusCode
  let cstat' :: [ConnectionStatus]
      cstat' = fmap status . clConnections . fromMaybe (error "bad response") . responseJsonMaybe $ resp
  unless (all (`elem` cstat') cstat) $
    error $ "connection check failed: " <> show cstat <> " is not a subset of " <> show cstat'
  where
    status c = ConnectionStatus (ucFrom c) (qUnqualified $ ucTo c) (ucStatus c)
    listConnections brig usr = get $ brig . path "connections" . zUser usr

randomUsers :: Int -> TestM [UserId]
randomUsers n = replicateM n randomUser

randomUserTuple :: HasCallStack => TestM (UserId, Qualified UserId)
randomUserTuple = do
  qUid <- randomQualifiedUser
  pure (qUnqualified qUid, qUid)

randomUser :: HasCallStack => TestM UserId
randomUser = qUnqualified <$> randomUser' False True True

randomQualifiedUser :: HasCallStack => TestM (Qualified UserId)
randomQualifiedUser = randomUser' False True True

randomQualifiedId :: MonadIO m => Domain -> m (Qualified (Id a))
randomQualifiedId domain = Qualified <$> randomId <*> pure domain

randomTeamCreator :: HasCallStack => TestM UserId
randomTeamCreator = qUnqualified <$> randomUser' True True True

randomTeamCreator' :: HasCallStack => TestM User
randomTeamCreator' = randomUser'' True True True

randomUser' :: HasCallStack => Bool -> Bool -> Bool -> TestM (Qualified UserId)
randomUser' isCreator hasPassword hasEmail = userQualifiedId <$> randomUser'' isCreator hasPassword hasEmail

randomUser'' :: HasCallStack => Bool -> Bool -> Bool -> TestM User
randomUser'' isCreator hasPassword hasEmail = selfUser <$> randomUserProfile' isCreator hasPassword hasEmail

randomUserProfile' :: HasCallStack => Bool -> Bool -> Bool -> TestM SelfProfile
randomUserProfile' isCreator hasPassword hasEmail = do
  b <- view tsBrig
  e <- liftIO randomEmail
  let p =
        object $
          ["name" .= fromEmail e]
            <> ["password" .= defPassword | hasPassword]
            <> ["email" .= fromEmail e | hasEmail]
            <> ["team" .= Team.BindingNewTeam (Team.newNewTeam (unsafeRange "teamName") DefaultIcon) | isCreator]
  responseJsonUnsafe <$> (post (b . path "/i/users" . json p) <!! const 201 === statusCode)

ephemeralUser :: HasCallStack => TestM UserId
ephemeralUser = do
  b <- view tsBrig
  name <- UUID.toText <$> liftIO nextRandom
  let p = object ["name" .= name]
  r <- post (b . path "/register" . json p) <!! const 201 === statusCode
  user <- responseJsonError r
  return $ Brig.Types.userId user

randomClient :: HasCallStack => UserId -> LastPrekey -> TestM ClientId
randomClient uid lk = randomClientWithCaps uid lk Nothing

randomClientWithCaps :: HasCallStack => UserId -> LastPrekey -> Maybe (Set Client.ClientCapability) -> TestM ClientId
randomClientWithCaps uid lk caps = do
  b <- view tsBrig
  resp <-
    post
      ( b
          . paths ["i", "clients", toByteString' uid]
          . queryItem "skip_reauth" "true"
          . json newClientBody
      )
      <!! const rStatus === statusCode
  client <- responseJsonError resp
  return (clientId client)
  where
    cType = PermanentClientType
    rStatus = 201
    newClientBody =
      (newClient cType lk)
        { newClientPassword = Just defPassword,
          newClientCapabilities = caps
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

getInternalClientsFull :: UserSet -> TestM UserClientsFull
getInternalClientsFull userSet = do
  b <- view tsBrig
  res <-
    post $
      b
        . paths ["i", "clients", "full"]
        . zConn "conn"
        . json userSet
  responseJsonError res

ensureClientCaps :: HasCallStack => UserId -> ClientId -> Client.ClientCapabilityList -> TestM ()
ensureClientCaps uid cid caps = do
  UserClientsFull (Map.lookup uid -> (Just clnts)) <- getInternalClientsFull (UserSet $ Set.singleton uid)
  let [clnt] = filter ((== cid) . clientId) $ Set.toList clnts
  liftIO $ assertEqual ("ensureClientCaps: " <> show (uid, cid, caps)) (clientCapabilities clnt) caps

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
      let st = maybeFromJSON =<< j ^? key "status"
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
  (u, c) <- randomUserWithClientQualified lk
  return (qUnqualified u, c)

randomUserWithClientQualified :: LastPrekey -> TestM (Qualified UserId, ClientId)
randomUserWithClientQualified lk = do
  u <- randomQualifiedUser
  c <- randomClient (qUnqualified u) lk
  return (u, c)

newNonce :: TestM (Id ())
newNonce = randomId

fromBS :: (HasCallStack, FromByteString a, MonadIO m) => ByteString -> m a
fromBS bs =
  case fromByteString bs of
    Nothing -> liftIO $ assertFailure "fromBS: no parse"
    Just x -> pure x

convRange :: Maybe (Either [ConvId] ConvId) -> Maybe Int32 -> Request -> Request
convRange range size =
  maybe id (queryItem "size" . C.pack . show) size
    . case range of
      Just (Left l) -> queryItem "ids" (C.intercalate "," $ map toByteString' l)
      Just (Right c) -> queryItem "start" (toByteString' c)
      Nothing -> id

privateAccess :: [Access]
privateAccess = [PrivateAccess]

assertExpected :: (Eq a, Show a) => String -> a -> (Response (Maybe LByteString) -> Maybe a) -> Assertions ()
assertExpected msg expected tparser =
  assertResponse $ \res ->
    case tparser res of
      Nothing -> Just (addTitle "Parsing the response failed")
      Just parsed ->
        if parsed == expected
          then Nothing
          else Just (addTitle (unlines ["Expected: ", show expected, "But got:", show parsed]))
  where
    addTitle s = unlines [msg, s]

assertBroadcastMismatch ::
  Domain ->
  BroadcastAPI ->
  [(UserId, Set ClientId)] ->
  [(UserId, Set ClientId)] ->
  [(UserId, Set ClientId)] ->
  Assertions ()
assertBroadcastMismatch localDomain BroadcastQualified =
  \m r d -> assertMismatchQualified mempty (mk m) (mk r) (mk d)
  where
    mk :: [(UserId, Set ClientId)] -> Client.QualifiedUserClients
    mk [] = mempty
    mk uc = Client.QualifiedUserClients . Map.singleton localDomain . Map.fromList $ uc
assertBroadcastMismatch _ _ = assertMismatch

assertMismatchWithMessage ::
  HasCallStack =>
  Maybe String ->
  [(UserId, Set ClientId)] ->
  [(UserId, Set ClientId)] ->
  [(UserId, Set ClientId)] ->
  Assertions ()
assertMismatchWithMessage mmsg missing redundant deleted = do
  assertExpected (formatMessage "missing") (userClients missing) (fmap missingClients . responseJsonMaybe)
  assertExpected (formatMessage "redundant") (userClients redundant) (fmap redundantClients . responseJsonMaybe)
  assertExpected (formatMessage "deleted") (userClients deleted) (fmap deletedClients . responseJsonMaybe)
  where
    userClients :: [(UserId, Set ClientId)] -> UserClients
    userClients = UserClients . Map.fromList

    formatMessage :: String -> String
    formatMessage = maybe id (\msg -> ((msg <> "\n") <>)) mmsg

assertMismatch ::
  HasCallStack =>
  [(UserId, Set ClientId)] ->
  [(UserId, Set ClientId)] ->
  [(UserId, Set ClientId)] ->
  Assertions ()
assertMismatch = assertMismatchWithMessage Nothing

assertMismatchQualified ::
  HasCallStack =>
  Client.QualifiedUserClients ->
  Client.QualifiedUserClients ->
  Client.QualifiedUserClients ->
  Client.QualifiedUserClients ->
  Assertions ()
assertMismatchQualified failedToSend missing redundant deleted = do
  assertExpected "failed to send" failedToSend (fmap mssFailedToSend . responseJsonMaybe)
  assertExpected "missing" missing (fmap mssMissingClients . responseJsonMaybe)
  assertExpected "redundant" redundant (fmap mssRedundantClients . responseJsonMaybe)

  assertExpected "deleted" deleted (fmap mssDeletedClients . responseJsonMaybe)

otrRecipients :: [(UserId, ClientId, Text)] -> OtrRecipients
otrRecipients =
  OtrRecipients
    . UserClientMap
    . fmap Map.fromList
    . foldr (uncurry Map.insert . fmap pure) mempty
    . map (\(a, b, c) -> (a, (b, c)))

genRandom :: (Q.Arbitrary a, MonadIO m) => m a
genRandom = liftIO . Q.generate $ Q.arbitrary

defPassword :: PlainTextPassword
defPassword = PlainTextPassword "secret"

randomEmail :: MonadIO m => m Email
randomEmail = do
  uid <- liftIO nextRandom
  return $ Email ("success+" <> UUID.toText uid) "simulator.amazonses.com"

selfConv :: UserId -> ConvId
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
someClientId = ClientId "550d8c614fd20299"

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

mkProteusConv ::
  ConvId ->
  UserId ->
  RoleName ->
  [OtherMember] ->
  RemoteConversation
mkProteusConv cnvId creator selfRole otherMembers =
  RemoteConversation
    cnvId
    ( ConversationMetadata
        RegularConv
        creator
        []
        (Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole])
        (Just "federated gossip")
        Nothing
        Nothing
        Nothing
    )
    (RemoteConvMembers selfRole otherMembers)
    ProtocolProteus

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
withSettingsOverrides :: (HasGalley m, MonadIO m, MonadMask m) => Opts.Opts -> SessionT m a -> m a
withSettingsOverrides opts action = do
  (galleyApp, _, finalizer) <- liftIO $ Run.mkApp opts
  runSessionT action galleyApp
    `finally` liftIO finalizer

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

deleteTeam :: UserId -> TeamId -> TestM ()
deleteTeam owner tid = do
  g <- view tsGalley
  delete
    ( g
        . paths ["teams", toByteString' tid]
        . zUser owner
        . zConn "conn"
        . json (newTeamMemberDeleteData (Just defPassword))
    )
    !!! do
      const 202 === statusCode

-- (Duplicate of 'Galley.Intra.User.getUsers'.)
getUsersByUid :: [UserId] -> TestM [User]
getUsersByUid = getUsersBy "ids"

getUsersByHandle :: [Handle.Handle] -> TestM [User]
getUsersByHandle = getUsersBy "handles"

getUsersBy :: forall uidsOrHandles. (ToByteString uidsOrHandles) => ByteString -> [uidsOrHandles] -> TestM [User]
getUsersBy keyName = chunkify $ \keys -> do
  brig <- view tsBrig
  let users = BS.intercalate "," $ toByteString' <$> keys
  res <-
    get
      ( brig
          . path "/i/users"
          . queryItem keyName users
          . expect2xx
      )
  let accounts = fromJust $ responseJsonMaybe @[UserAccount] res
  return $ fmap accountUser accounts

getUserProfile :: UserId -> UserId -> TestM UserProfile
getUserProfile zusr uid = do
  brig <- view tsBrig
  res <- get (brig . zUser zusr . paths ["users", toByteString' uid])
  responseJsonError res

upgradeClientToLH :: HasCallStack => UserId -> ClientId -> TestM ()
upgradeClientToLH zusr cid =
  putCapabilities zusr cid [ClientSupportsLegalholdImplicitConsent]

putCapabilities :: HasCallStack => UserId -> ClientId -> [ClientCapability] -> TestM ()
putCapabilities zusr cid caps = do
  brig <- view tsBrig
  void $
    put
      ( brig
          . zUser zusr
          . paths ["clients", toByteString' cid]
          . json defUpdateClient {updateClientCapabilities = Just (Set.fromList caps)}
          . expect2xx
      )

getUsersPrekeysClientUnqualified :: HasCallStack => UserId -> UserId -> ClientId -> TestM ResponseLBS
getUsersPrekeysClientUnqualified zusr uid cid = do
  brig <- view tsBrig
  get
    ( brig
        . zUser zusr
        . paths ["users", toByteString' uid, "prekeys", toByteString' cid]
    )

getUsersPrekeyBundleUnqualified :: HasCallStack => UserId -> UserId -> TestM ResponseLBS
getUsersPrekeyBundleUnqualified zusr uid = do
  brig <- view tsBrig
  get
    ( brig
        . zUser zusr
        . paths ["users", toByteString' uid, "prekeys"]
    )

getMultiUserPrekeyBundleUnqualified :: HasCallStack => UserId -> UserClients -> TestM ResponseLBS
getMultiUserPrekeyBundleUnqualified zusr userClients = do
  brig <- view tsBrig
  post
    ( brig
        . zUser zusr
        . paths ["users", "prekeys"]
        . json userClients
    )

mkProfile :: Qualified UserId -> Name -> UserProfile
mkProfile quid name =
  UserProfile
    { profileQualifiedId = quid,
      profileName = name,
      profilePict = noPict,
      profileAssets = mempty,
      profileAccentId = defaultAccentId,
      profileDeleted = False,
      profileService = Nothing,
      profileHandle = Nothing,
      profileExpire = Nothing,
      profileTeam = Nothing,
      profileEmail = Nothing,
      profileLegalholdStatus = defUserLegalHoldStatus
    }

-- mock federator

-- | Run the given action on a temporary galley instance with access to a mock
-- federator.
--
-- The `resp :: FederatedRequest -> a` argument can be used to provide a fake
-- federator response (of an arbitrary JSON-serialisable type a) for every
-- expected request.
withTempMockFederator ::
  (MonadIO m, ToJSON a, HasGalley m, MonadMask m) =>
  (FederatedRequest -> a) ->
  SessionT m b ->
  m (b, [FederatedRequest])
withTempMockFederator resp = withTempMockFederator' $ pure . encode . resp

withTempMockFederator' ::
  (MonadIO m, HasGalley m, MonadMask m) =>
  (FederatedRequest -> IO LByteString) ->
  SessionT m b ->
  m (b, [FederatedRequest])
withTempMockFederator' resp action = do
  opts <- viewGalleyOpts
  Mock.withTempMockFederator
    [("Content-Type", "application/json")]
    ((\r -> pure ("application" // "json", r)) <=< resp)
    $ \mockPort -> do
      let opts' =
            opts & Opts.optFederator
              ?~ Endpoint "127.0.0.1" (fromIntegral mockPort)
      withSettingsOverrides opts' action

-- Start a mock federator. Use proveded Servant handler for the mocking mocking function.
withTempServantMockFederator ::
  (MonadMask m, MonadIO m, HasGalley m) =>
  (Domain -> ServerT (FedApi 'Brig) Handler) ->
  (Domain -> ServerT (FedApi 'Galley) Handler) ->
  Domain ->
  SessionT m b ->
  m (b, [FederatedRequest])
withTempServantMockFederator brigApi galleyApi originDomain =
  withTempMockFederator' mock
  where
    server :: Domain -> ServerT CombinedBrigAndGalleyAPI Handler
    server d = brigApi d :<|> galleyApi d

    mock :: FederatedRequest -> IO LByteString
    mock req =
      makeFedRequestToServant @CombinedBrigAndGalleyAPI originDomain (server (frTargetDomain req)) req

type CombinedBrigAndGalleyAPI = FedApi 'Brig :<|> FedApi 'Galley

-- Starts a servant Application in Network.Wai.Test session and runs the
-- FederatedRequest against it.
makeFedRequestToServant ::
  forall (api :: *).
  HasServer api '[] =>
  Domain ->
  Server api ->
  FederatedRequest ->
  IO LByteString
makeFedRequestToServant originDomain server fedRequest = do
  sresp <- Wai.runSession session app
  let status = Wai.simpleStatus sresp
      bdy = Wai.simpleBody sresp
  if HTTP.statusIsSuccessful status
    then pure bdy
    else throw (Mock.MockErrorResponse status (T.decodeUtf8 bdy))
  where
    app :: Application
    app = serve (Proxy @api) server

    session :: Wai.Session Wai.SResponse
    session = do
      Wai.srequest
        ( Wai.SRequest
            ( defaultRequest
                { Wai.requestMethod = HTTP.methodPost,
                  Wai.pathInfo = [frRPC fedRequest],
                  Wai.requestHeaders =
                    [ (CI.mk "Content-Type", "application/json"),
                      (CI.mk "Accept", "application/json"),
                      (originDomainHeaderName, cs . domainText $ originDomain)
                    ]
                }
            )
            (frBody fedRequest)
        )

assertRight :: (MonadIO m, Show a, HasCallStack) => Either a b -> m b
assertRight = \case
  Left e -> liftIO $ assertFailure $ "Expected Right, got Left: " <> show e
  Right x -> pure x

assertRightT :: (MonadIO m, Show a, HasCallStack) => ExceptT a m b -> m b
assertRightT = assertRight <=< runExceptT

-- | Run a probe several times, until a "good" value materializes or until patience runs out
-- (after ~2secs).
-- If all retries were unsuccessful, 'aFewTimes' will return the last obtained value, even
-- if it does not satisfy the predicate.
aFewTimes :: TestM a -> (a -> Bool) -> TestM a
aFewTimes action good = do
  env <- ask
  liftIO $
    retrying
      (exponentialBackoff 1000 <> limitRetries 11)
      (\_ -> pure . not . good)
      (\_ -> runReaderT (runTestM action) env)

aFewTimesAssertBool :: HasCallStack => String -> (a -> Bool) -> TestM a -> TestM ()
aFewTimesAssertBool msg good action = do
  result <- aFewTimes action good
  liftIO $ assertBool msg (good result)

checkUserUpdateEvent :: HasCallStack => UserId -> WS.WebSocket -> TestM ()
checkUserUpdateEvent uid w = WS.assertMatch_ checkTimeout w $ \notif -> do
  let j = Object $ List1.head (ntfPayload notif)
  let etype = j ^? key "type" . _String
  let euser = j ^?! key "user" ^? key "id" . _String
  etype @?= Just "user.update"
  euser @?= Just (UUID.toText (toUUID uid))

checkUserDeleteEvent :: HasCallStack => UserId -> WS.WebSocket -> TestM ()
checkUserDeleteEvent uid w = WS.assertMatch_ checkTimeout w $ \notif -> do
  let j = Object $ List1.head (ntfPayload notif)
  let etype = j ^? key "type" . _String
  let euser = j ^? key "id" . _String
  etype @?= Just "user.delete"
  euser @?= Just (UUID.toText (toUUID uid))

checkTeamMemberJoin :: HasCallStack => TeamId -> UserId -> WS.WebSocket -> TestM ()
checkTeamMemberJoin tid uid w = WS.awaitMatch_ checkTimeout w $ \notif -> do
  ntfTransient notif @?= False
  let e = List1.head (WS.unpackPayload notif)
  e ^. eventTeam @?= tid
  e ^. eventData @?= EdMemberJoin uid

checkTeamMemberLeave :: HasCallStack => TeamId -> UserId -> WS.WebSocket -> TestM ()
checkTeamMemberLeave tid usr w = WS.assertMatch_ checkTimeout w $ \notif -> do
  ntfTransient notif @?= False
  let e = List1.head (WS.unpackPayload notif)
  e ^. eventTeam @?= tid
  e ^. eventData @?= EdMemberLeave usr

checkTeamUpdateEvent :: (HasCallStack, MonadIO m, MonadCatch m) => TeamId -> TeamUpdateData -> WS.WebSocket -> m ()
checkTeamUpdateEvent tid upd w = WS.assertMatch_ checkTimeout w $ \notif -> do
  ntfTransient notif @?= False
  let e = List1.head (WS.unpackPayload notif)
  e ^. eventTeam @?= tid
  e ^. eventData @?= EdTeamUpdate upd

checkConvCreateEvent :: (MonadIO m, MonadCatch m) => HasCallStack => ConvId -> WS.WebSocket -> m ()
checkConvCreateEvent cid w = WS.assertMatch_ checkTimeout w $ \notif -> do
  ntfTransient notif @?= False
  let e = List1.head (WS.unpackPayload notif)
  evtType e @?= Conv.ConvCreate
  case evtData e of
    Conv.EdConversation x -> (qUnqualified . cnvQualifiedId) x @?= cid
    other -> assertFailure $ "Unexpected event data: " <> show other

wsAssertConvCreateWithRole ::
  HasCallStack =>
  Qualified ConvId ->
  Qualified UserId ->
  Qualified UserId ->
  [(Qualified UserId, RoleName)] ->
  Notification ->
  IO ()
wsAssertConvCreateWithRole conv eventFrom selfMember otherMembers n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= conv
  evtType e @?= Conv.ConvCreate
  evtFrom e @?= eventFrom
  fmap (memId . cmSelf . cnvMembers) (evtData e ^? _EdConversation) @?= Just selfMember
  fmap (sort . cmOthers . cnvMembers) (evtData e ^? _EdConversation) @?= Just (sort (toOtherMember <$> otherMembers))
  where
    toOtherMember (quid, role) = OtherMember quid Nothing role

checkTeamDeleteEvent :: HasCallStack => TeamId -> WS.WebSocket -> TestM ()
checkTeamDeleteEvent tid w = WS.assertMatch_ checkTimeout w $ \notif -> do
  ntfTransient notif @?= False
  let e = List1.head (WS.unpackPayload notif)
  e ^. eventTeam @?= tid
  e ^. eventData @?= EdTeamDelete

checkConvDeleteEvent :: HasCallStack => Qualified ConvId -> WS.WebSocket -> TestM ()
checkConvDeleteEvent cid w = WS.assertMatch_ checkTimeout w $ \notif -> do
  ntfTransient notif @?= False
  let e = List1.head (WS.unpackPayload notif)
  evtType e @?= Conv.ConvDelete
  evtConv e @?= cid
  evtData e @?= Conv.EdConvDelete

checkConvMemberLeaveEvent :: HasCallStack => Qualified ConvId -> Qualified UserId -> WS.WebSocket -> TestM ()
checkConvMemberLeaveEvent cid usr w = WS.assertMatch_ checkTimeout w $ \notif -> do
  ntfTransient notif @?= False
  let e = List1.head (WS.unpackPayload notif)
  evtConv e @?= cid
  evtType e @?= Conv.MemberLeave
  case evtData e of
    Conv.EdMembersLeave mm -> mm @?= Conv.QualifiedUserIdList [usr]
    other -> assertFailure $ "Unexpected event data: " <> show other

checkTimeout :: WS.Timeout
checkTimeout = 3 # Second

-- | The function is used in conjuction with 'withTempMockFederator' to mock
-- responses by Brig on the mocked side of federation.
mockedFederatedBrigResponse :: [(Qualified UserId, Text)] -> FederatedRequest -> Maybe Value
mockedFederatedBrigResponse users req
  | frComponent req == Brig =
    Just . toJSON $ [mkProfile mem (Name name) | (mem, name) <- users]
  | otherwise = Nothing

-- | Combine two mocked services such that for a given request a JSON response
-- is produced.
joinMockedFederatedResponses ::
  (FederatedRequest -> Maybe Value) ->
  (FederatedRequest -> Maybe Value) ->
  FederatedRequest ->
  Value
joinMockedFederatedResponses service1 service2 req =
  fromMaybe (toJSON ()) (service1 req <|> service2 req)

-- | Only Brig is mocked.
onlyMockedFederatedBrigResponse :: [(Qualified UserId, Text)] -> FederatedRequest -> Value
onlyMockedFederatedBrigResponse users =
  joinMockedFederatedResponses
    (mockedFederatedBrigResponse users)
    (const Nothing)

fedRequestsForDomain :: HasCallStack => Domain -> Component -> [FederatedRequest] -> [FederatedRequest]
fedRequestsForDomain domain component =
  filter $ \req -> frTargetDomain req == domain && frComponent req == component

parseFedRequest :: FromJSON a => FederatedRequest -> Either String a
parseFedRequest fr = eitherDecode (frBody fr)

assertOne :: (HasCallStack, MonadIO m, Show a) => [a] -> m a
assertOne [a] = pure a
assertOne xs = liftIO . assertFailure $ "Expected exactly one element, found " <> show xs

assertNone :: (HasCallStack, MonadIO m, Show a) => [a] -> m ()
assertNone [] = pure ()
assertNone xs = liftIO . assertFailure $ "Expected exactly no elements, found " <> show xs

assertJust :: (HasCallStack, MonadIO m) => Maybe a -> m a
assertJust (Just a) = pure a
assertJust Nothing = liftIO $ assertFailure "Expected Just, got Nothing"

iUpsertOne2OneConversation :: UpsertOne2OneConversationRequest -> TestM ResponseLBS
iUpsertOne2OneConversation req = do
  galley <- view tsGalley
  post (galley . path "/i/conversations/one2one/upsert" . Bilge.json req)

createOne2OneConvWithRemote :: HasCallStack => Local UserId -> Remote UserId -> TestM ()
createOne2OneConvWithRemote localUser remoteUser = do
  let mkRequest actor mConvId =
        UpsertOne2OneConversationRequest
          { uooLocalUser = localUser,
            uooRemoteUser = remoteUser,
            uooActor = actor,
            uooActorDesiredMembership = Included,
            uooConvId = mConvId
          }
  ooConvId <-
    fmap uuorConvId . responseJsonError
      =<< iUpsertOne2OneConversation (mkRequest LocalActor Nothing)
      <!! const 200 === statusCode
  iUpsertOne2OneConversation (mkRequest RemoteActor (Just ooConvId))
    !!! const 200 === statusCode

generateRemoteAndConvId :: Bool -> Local UserId -> TestM (Remote UserId, Qualified ConvId)
generateRemoteAndConvId = generateRemoteAndConvIdWithDomain (Domain "far-away.example.com")

generateRemoteAndConvIdWithDomain :: Domain -> Bool -> Local UserId -> TestM (Remote UserId, Qualified ConvId)
generateRemoteAndConvIdWithDomain remoteDomain shouldBeLocal lUserId = do
  other <- Qualified <$> randomId <*> pure remoteDomain
  let convId = one2OneConvId (qUntagged lUserId) other
      isLocal = tDomain lUserId == qDomain convId
  if shouldBeLocal == isLocal
    then pure (qTagUnsafe other, convId)
    else generateRemoteAndConvIdWithDomain remoteDomain shouldBeLocal lUserId

matchFedRequest :: Domain -> Text -> FederatedRequest -> Bool
matchFedRequest domain reqpath req =
  frTargetDomain req == domain
    && frRPC req == reqpath

spawn :: HasCallStack => CreateProcess -> Maybe ByteString -> IO ByteString
spawn cp minput = do
  (mout, ex) <- withCreateProcess
    cp
      { std_out = CreatePipe,
        std_in = if isJust minput then CreatePipe else Inherit
      }
    $ \minh mouth _ ph ->
      let writeInput = for_ ((,) <$> minput <*> minh) $ \(input, inh) ->
            BS.hPutStr inh input >> hClose inh
          readOutput = (,) <$> traverse BS.hGetContents mouth <*> waitForProcess ph
       in fmap snd $ concurrently writeInput readOutput
  case (mout, ex) of
    (Just out, ExitSuccess) -> pure out
    _ -> assertFailure "Failed spawning process"

decodeMLSError :: ParseMLS a => ByteString -> IO a
decodeMLSError s = case decodeMLS' s of
  Left e -> assertFailure ("Could not parse MLS object: " <> Text.unpack e)
  Right x -> pure x

wsAssertConvReceiptModeUpdate :: Qualified ConvId -> Qualified UserId -> ReceiptMode -> Notification -> IO ()
wsAssertConvReceiptModeUpdate conv usr new n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= conv
  evtType e @?= ConvReceiptModeUpdate
  evtFrom e @?= usr
  evtData e @?= EdConvReceiptModeUpdate (ConversationReceiptModeUpdate new)

newtype WaiTestFedClient a = WaiTestFedClient {unWaiTestFedClient :: ReaderT Domain WaiTest.Session a}
  deriving (Functor, Applicative, Monad, MonadIO)

instance Servant.RunClient WaiTestFedClient where
  runRequestAcceptStatus expectedStatuses servantRequest = WaiTestFedClient $ do
    domain <- ask
    let req' = fromServantRequest domain servantRequest
    res <- lift $ WaiTest.srequest req'
    let servantResponse = toServantResponse res
    let status = Servant.responseStatusCode servantResponse
    let statusIsSuccess =
          case expectedStatuses of
            Nothing -> HTTP.statusIsSuccessful status
            Just ex -> status `elem` ex
    unless statusIsSuccess $
      unWaiTestFedClient $ throwClientError (FailureResponse (bimap (const ()) (\x -> (Servant.BaseUrl Servant.Http "" 80 "", cs (toLazyByteString x))) servantRequest) servantResponse)
    pure servantResponse
  throwClientError = liftIO . throw

fromServantRequest :: Domain -> Servant.Request -> WaiTest.SRequest
fromServantRequest domain r =
  let pathBS = "/federation" <> Data.String.Conversions.cs (toLazyByteString (Servant.requestPath r))
      bodyBS = case Servant.requestBody r of
        Nothing -> ""
        Just (bdy, _) -> case bdy of
          Servant.RequestBodyLBS lbs -> Data.String.Conversions.cs lbs
          Servant.RequestBodyBS bs -> bs
          Servant.RequestBodySource _ -> error "fromServantRequest: not implemented for RequestBodySource"

      -- Content-Type and Accept are specified by requestBody and requestAccept
      headers =
        filter (\(h, _) -> h /= "Accept" && h /= "Content-Type") $
          toList $ Servant.requestHeaders r
      acceptHdr
        | null hs = Nothing
        | otherwise = Just ("Accept", renderHeader hs)
        where
          hs = toList $ ServantRequest.requestAccept r
      contentTypeHdr = case ServantRequest.requestBody r of
        Nothing -> Nothing
        Just (_', typ) -> Just (HTTP.hContentType, renderHeader typ)
      req =
        Wai.defaultRequest
          { Wai.requestMethod = Servant.requestMethod r,
            Wai.rawPathInfo = pathBS,
            Wai.rawQueryString = renderQuery True (toList (Servant.requestQueryString r)),
            Wai.requestHeaders =
              -- Inspired by 'Servant.Client.Internal.HttpClient.defaultMakeClientRequest',
              -- the Servant function that maps @Request@ to @Client.Request@.
              -- This solution is a bit sophisticated due to two constraints:
              --   - Accept header may contain a list of accepted media types.
              --   - Accept and Content-Type headers should only appear once in the result.
              maybeToList acceptHdr
                <> maybeToList contentTypeHdr
                <> headers
                <> [(originDomainHeaderName, Text.encodeUtf8 (domainText domain))],
            Wai.isSecure = True,
            Wai.pathInfo = filter (not . Text.null) (map Data.String.Conversions.cs (C8.split '/' pathBS)),
            Wai.queryString = toList (Servant.requestQueryString r)
          }
   in WaiTest.SRequest req (cs bodyBS)

toServantResponse :: WaiTest.SResponse -> Servant.Response
toServantResponse res =
  Servant.Response
    { Servant.responseStatusCode = WaiTest.simpleStatus res,
      Servant.responseHeaders = Seq.fromList (WaiTest.simpleHeaders res),
      Servant.responseBody = WaiTest.simpleBody res,
      Servant.responseHttpVersion = http11
    }

createWaiTestFedClient ::
  forall (name :: Symbol) comp api.
  ( HasFedEndpoint comp api name,
    Servant.HasClient WaiTestFedClient api
  ) =>
  Servant.Client WaiTestFedClient api
createWaiTestFedClient =
  Servant.clientIn (Proxy @api) (Proxy @WaiTestFedClient)

runWaiTestFedClient ::
  Domain ->
  WaiTestFedClient a ->
  WaiTest.Session a
runWaiTestFedClient domain action =
  runReaderT (unWaiTestFedClient action) domain
