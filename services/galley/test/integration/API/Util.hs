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
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module API.Util where

import API.Federation.Util
import API.SQS qualified as SQS
import Bilge hiding (timeout)
import Bilge.Assert
import Bilge.TestSession
import Control.Applicative
import Control.Concurrent.Async
import Control.Exception (throw)
import Control.Lens hiding (from, to, uncons, (#), (.=))
import Control.Monad.Catch (MonadCatch, MonadMask)
import Control.Monad.Codensity (lowerCodensity)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Retry (constantDelay, exponentialBackoff, limitRetries, retrying)
import Data.Aeson hiding (json)
import Data.Aeson qualified as A
import Data.Aeson.Lens (key, _String)
import Data.ByteString qualified as BS
import Data.ByteString.Base64.URL qualified as B64U
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Char8 qualified as C
import Data.ByteString.Conversion
import Data.ByteString.Lazy qualified as Lazy
import Data.CaseInsensitive qualified as CI
import Data.Code qualified as Code
import Data.Currency qualified as Currency
import Data.Default
import Data.Domain
import Data.Handle qualified as Handle
import Data.HashMap.Strict qualified as HashMap
import Data.Id
import Data.Json.Util hiding ((#))
import Data.Kind
import Data.LegalHold (defUserLegalHoldStatus)
import Data.List.NonEmpty (NonEmpty)
import Data.List1 as List1
import Data.Map qualified as LMap
import Data.Map.Strict qualified as Map
import Data.Misc
import Data.ProtoLens qualified as Protolens
import Data.ProtocolBuffers (encodeMessage)
import Data.Qualified hiding (isLocal)
import Data.Range
import Data.Serialize (runPut)
import Data.Set qualified as Set
import Data.Singletons
import Data.Text qualified as Text
import Data.Text.Encoding qualified as T
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy.Encoding qualified as LT
import Data.Time (getCurrentTime)
import Data.Tuple.Extra
import Data.UUID qualified as UUID
import Data.UUID.V4
import Federator.MockServer
import Federator.MockServer qualified as Mock
import GHC.TypeLits (KnownSymbol)
import GHC.TypeNats
import Galley.Intra.User (chunkify)
import Galley.Options qualified as Opts
import Galley.Run qualified as Run
import Galley.Types.Conversations.One2One
import Galley.Types.Teams qualified as Team
import Galley.Types.UserList
import Imports
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Media.MediaType
import Network.HTTP.Types qualified as HTTP
import Network.URI (pathSegments)
import Network.Wai (defaultRequest)
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as Wai
import Network.Wai.Utilities.MockServer (withMockServer)
import Servant
import System.Exit
import System.Process
import System.Random
import Test.QuickCheck qualified as Q
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import Test.Tasty.Cannon qualified as WS
import Test.Tasty.HUnit
import TestHelpers (viewFederationDomain)
import TestSetup
import UnliftIO.Timeout
import Util.Options
import Web.Cookie
import Wire.API.Connection
import Wire.API.Conversation
import Wire.API.Conversation qualified as Conv
import Wire.API.Conversation.Action
import Wire.API.Conversation.Code hiding (Value)
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role
import Wire.API.Conversation.Typing
import Wire.API.Event.Conversation
import Wire.API.Event.Conversation qualified as Conv
import Wire.API.Event.Federation qualified as Fed
import Wire.API.Event.LeaveReason
import Wire.API.Event.Team
import Wire.API.Event.Team qualified as TE
import Wire.API.Federation.API
import Wire.API.Federation.API.Brig
import Wire.API.Federation.API.Common
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Domain (originDomainHeaderName)
import Wire.API.Internal.Notification hiding (target)
import Wire.API.MLS.LeafNode
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.Message
import Wire.API.Message.Proto qualified as Proto
import Wire.API.Routes.Internal.Brig.Connection
import Wire.API.Routes.Internal.Galley.ConversationsIntra
import Wire.API.Routes.Internal.Galley.TeamFeatureNoConfigMulti qualified as Multi
import Wire.API.Routes.Internal.Galley.TeamsIntra
import Wire.API.Routes.MultiTablePaging
import Wire.API.Routes.Version
import Wire.API.Team
import Wire.API.Team.Feature
import Wire.API.Team.Invitation
import Wire.API.Team.Member hiding (userId)
import Wire.API.Team.Member qualified as Team
import Wire.API.Team.Permission hiding (self)
import Wire.API.Team.Role
import Wire.API.User hiding (AccountStatus (..))
import Wire.API.User.Auth hiding (Access)
import Wire.API.User.Client
import Wire.API.User.Client qualified as Client
import Wire.API.User.Client.Prekey

-------------------------------------------------------------------------------
-- API Operations

addPrefix :: Request -> Request
addPrefix = maybeAddPrefixAtVersion maxBound

maybeAddPrefixAtVersion :: Version -> Request -> Request
maybeAddPrefixAtVersion vers r = case pathSegments $ getUri r of
  ("i" : _) -> r
  ("api-internal" : _) -> r
  _ -> addPrefixAtVersion vers r

addPrefixAtVersion :: Version -> Request -> Request
addPrefixAtVersion v r = r {HTTP.path = toHeader v <> "/" <> removeSlash (HTTP.path r)}
  where
    removeSlash s = case B8.uncons s of
      Just ('/', s') -> s'
      _ -> s

-- | A class for monads with access to a Sem r instance
class HasGalley m where
  viewGalley :: m GalleyR
  viewGalleyOpts :: m Opts.Opts

instance HasGalley TestM where
  viewGalley = fmap (addPrefix .) (view tsUnversionedGalley)
  viewGalleyOpts = view tsGConf

instance (HasGalley m, Monad m) => HasGalley (SessionT m) where
  viewGalley = lift viewGalley
  viewGalleyOpts = lift viewGalleyOpts

class HasBrig m where
  viewBrig :: m BrigR

instance HasBrig TestM where
  viewBrig = fmap (addPrefix .) (view tsUnversionedBrig)

symmPermissions :: [Perm] -> Permissions
symmPermissions p = let s = Set.fromList p in fromJust (newPermissions s s)

createBindingTeam :: HasCallStack => TestM (UserId, TeamId)
createBindingTeam = do
  first Wire.API.User.userId <$> createBindingTeam'

createBindingTeam' :: HasCallStack => TestM (User, TeamId)
createBindingTeam' = do
  owner <- randomTeamCreator'
  teams <- getTeams owner.userId []
  team <- assertOne $ view teamListTeams teams
  let tid = view teamId team
  SQS.assertTeamActivate "create team" tid
  refreshIndex
  pure (owner, tid)

createBindingTeamWithMembers :: HasCallStack => Int -> TestM (TeamId, UserId, [UserId])
createBindingTeamWithMembers numUsers = do
  (owner, tid) <- createBindingTeam
  members <- forM [2 .. numUsers] $ \n -> do
    mem <- addUserToTeam owner tid
    SQS.assertTeamUpdate "add member" tid (fromIntegral n) [owner]
    -- 'refreshIndex' needs to happen here to make tests more realistic.  one effect of
    -- refreshing the index once at the end would be that the hard member limit wouldn't hold
    -- any more.
    refreshIndex
    pure $ view Team.userId mem

  pure (tid, owner, members)

createBindingTeamWithQualifiedMembers :: HasCallStack => Int -> TestM (TeamId, Qualified UserId, [Qualified UserId])
createBindingTeamWithQualifiedMembers num = do
  localDomain <- viewFederationDomain
  (tid, owner, users) <- createBindingTeamWithMembers num
  pure (tid, Qualified owner localDomain, map (`Qualified` localDomain) users)

getTeams :: UserId -> [(ByteString, Maybe ByteString)] -> TestM TeamList
getTeams u queryItems = do
  -- This endpoint is removed from version v4 onwards
  g <- fmap (addPrefixAtVersion V3 .) (view tsUnversionedGalley)
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
  pure $ responseJsonUnsafe r

createBindingTeamWithNMembers :: Int -> TestM (UserId, TeamId, [UserId])
createBindingTeamWithNMembers = createBindingTeamWithNMembersWithHandles False

createBindingTeamWithNMembersWithHandles :: Bool -> Int -> TestM (UserId, TeamId, [UserId])
createBindingTeamWithNMembersWithHandles withHandles n = do
  (owner, tid) <- createBindingTeam
  setHandle owner
  mems <- replicateM n $ do
    member1 <- randomUser
    addTeamMemberInternal tid member1 (Team.rolePermissions RoleMember) Nothing
    setHandle member1
    pure member1
  pure (owner, tid, mems)
  where
    mkRandomHandle :: MonadIO m => m Text
    mkRandomHandle = liftIO $ do
      nrs <- replicateM 21 (randomRIO (97, 122)) -- a-z
      pure (cs (map chr nrs))

    setHandle :: UserId -> TestM ()
    setHandle uid = when withHandles $ do
      b <- viewBrig
      randomHandle <- mkRandomHandle
      put
        ( b
            . paths ["/i/users", toByteString' uid, "handle"]
            . json (HandleUpdate randomHandle)
        )
        !!! do
          const 200 === statusCode

changeTeamStatus :: HasCallStack => TeamId -> TeamStatus -> TestM ()
changeTeamStatus tid s = do
  g <- viewGalley
  put
    ( g
        . paths ["i", "teams", toByteString' tid, "status"]
        . json (TeamStatusUpdate s Nothing)
    )
    !!! const 200
      === statusCode

createBindingTeamInternal :: HasCallStack => Text -> UserId -> TestM TeamId
createBindingTeamInternal name owner = do
  tid <- createBindingTeamInternalNoActivate name owner
  changeTeamStatus tid Active
  pure tid

createBindingTeamInternalNoActivate :: HasCallStack => Text -> UserId -> TestM TeamId
createBindingTeamInternalNoActivate name owner = do
  g <- viewGalley
  tid <- randomId
  let nt = BindingNewTeam $ newNewTeam (unsafeRange name) DefaultIcon
  _ <-
    put (g . paths ["/i/teams", toByteString' tid] . zUser owner . zConn "conn" . zType "access" . json nt) <!! do
      const 201 === statusCode
      const True === isJust . getHeader "Location"
  pure tid

createBindingTeamInternalWithCurrency :: HasCallStack => Text -> UserId -> Currency.Alpha -> TestM TeamId
createBindingTeamInternalWithCurrency name owner cur = do
  g <- viewGalley
  tid <- createBindingTeamInternalNoActivate name owner
  _ <-
    put (g . paths ["i", "teams", toByteString' tid, "status"] . json (TeamStatusUpdate Active $ Just cur))
      !!! const 200
        === statusCode
  pure tid

getTeamInternal :: HasCallStack => TeamId -> TestM TeamData
getTeamInternal tid = do
  g <- viewGalley
  r <- get (g . paths ["i/teams", toByteString' tid]) <!! const 200 === statusCode
  responseJsonError r

getTeam :: HasCallStack => UserId -> TeamId -> TestM Team
getTeam usr tid = do
  g <- viewGalley
  r <- get (g . paths ["teams", toByteString' tid] . zUser usr) <!! const 200 === statusCode
  responseJsonError r

getTeamMembers :: HasCallStack => UserId -> TeamId -> TestM TeamMemberList
getTeamMembers usr tid = do
  g <- viewGalley
  r <- get (g . paths ["teams", toByteString' tid, "members"] . zUser usr) <!! const 200 === statusCode
  responseJsonError r

-- alternative to 'ResponseLBS': [BodyReader](https://hoogle.zinfra.io/file/root/.stack/snapshots/x86_64-linux/82492d944a85db90f4cd7cec6f4d5215ef9ac1ac8aeffeed4a805fbd6b1232c5/8.8.4/doc/http-client-0.7.0/Network-HTTP-Client.html#t:BodyReader)
getTeamMembersCsv :: HasCallStack => UserId -> TeamId -> TestM ResponseLBS
getTeamMembersCsv usr tid = do
  g <- viewGalley
  get (g . accept "text/csv" . paths ["teams", toByteString' tid, "members/csv"] . zUser usr) <!! do
    const 200 === statusCode
    const (Just "chunked") === lookup "Transfer-Encoding" . responseHeaders

getTeamMembersTruncated :: HasCallStack => UserId -> TeamId -> Int -> TestM TeamMemberList
getTeamMembersTruncated usr tid n = do
  g <- viewGalley
  r <- get (g . paths ["teams", toByteString' tid, "members"] . zUser usr . queryItem "maxResults" (C.pack $ show n)) <!! const 200 === statusCode
  responseJsonError r

data ResultPage = ResultPage
  { rpResults :: [A.Value],
    rpHasMore :: Bool,
    rpPagingState :: Text
  }

instance FromJSON ResultPage where
  parseJSON = withObject "ResultPage" $ \o ->
    ResultPage
      <$> o
        .: "members"
      <*> o
        .: "hasMore"
      <*> o
        .: "pagingState"

getTeamMembersPaginated :: HasCallStack => UserId -> TeamId -> Int -> Maybe Text -> TestM ResultPage
getTeamMembersPaginated usr tid n mPs = do
  g <- viewGalley
  r <-
    get
      ( g
          . paths ["teams", toByteString' tid, "members"]
          . zUser usr
          . queryItem "maxResults" (C.pack $ show n)
          . maybe Imports.id (queryItem "pagingState" . cs) mPs
      )
      <!! const 200
        === statusCode
  responseJsonError r

getTeamMembersInternalTruncated :: HasCallStack => TeamId -> Int -> TestM TeamMemberList
getTeamMembersInternalTruncated tid n = do
  g <- viewGalley
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
  g <- viewGalley
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
  g <- viewGalley
  post
    ( g
        . paths ["teams", toByteString' tid, "get-members-by-ids-using-post"]
        . zUser usr
        . queryItem "maxResults" (C.pack $ show trnc)
        . json (UserIdList uids)
    )

getTeamMember :: HasCallStack => UserId -> TeamId -> UserId -> TestM TeamMember
getTeamMember getter tid gettee = do
  g <- viewGalley
  getTeamMember' g getter tid gettee

getTeamMember' :: (HasCallStack, MonadHttp m, MonadIO m, MonadCatch m) => GalleyR -> UserId -> TeamId -> UserId -> m TeamMember
getTeamMember' g getter tid gettee = do
  r <- get (g . paths ["teams", toByteString' tid, "members", toByteString' gettee] . zUser getter) <!! const 200 === statusCode
  responseJsonError r

getTeamMemberInternal :: HasCallStack => TeamId -> UserId -> TestM TeamMember
getTeamMemberInternal tid mid = do
  g <- viewGalley
  r <- get (g . paths ["i", "teams", toByteString' tid, "members", toByteString' mid]) <!! const 200 === statusCode
  responseJsonError r

addTeamMember :: HasCallStack => UserId -> TeamId -> UserId -> Permissions -> Maybe (UserId, UTCTimeMillis) -> TestM ()
addTeamMember usr tid muid mperms mmbinv = do
  g <- viewGalley
  let payload = json (mkNewTeamMember muid mperms mmbinv)
  post (g . paths ["teams", toByteString' tid, "members"] . zUser usr . zConn "conn" . payload)
    !!! const 200
      === statusCode

-- | FUTUREWORK: do not use this, it's broken!!  use 'addUserToTeam' instead!  https://wearezeta.atlassian.net/browse/SQSERVICES-471
addTeamMemberInternal :: HasCallStack => TeamId -> UserId -> Permissions -> Maybe (UserId, UTCTimeMillis) -> TestM ()
addTeamMemberInternal tid muid mperms mmbinv = addTeamMemberInternal' tid muid mperms mmbinv !!! const 200 === statusCode

-- | FUTUREWORK: do not use this, it's broken!!  use 'addUserToTeam' instead!  https://wearezeta.atlassian.net/browse/SQSERVICES-471
addTeamMemberInternal' :: HasCallStack => TeamId -> UserId -> Permissions -> Maybe (UserId, UTCTimeMillis) -> TestM ResponseLBS
addTeamMemberInternal' tid muid mperms mmbinv = do
  g <- viewGalley
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
      inviteeId = invitee.userId
  let invmeta = Just (inviter, inCreatedAt inv)
  mem <- getTeamMember inviter tid inviteeId
  liftIO $ assertEqual "Member has no/wrong invitation metadata" invmeta (mem ^. Team.invitation)
  let zuid = parseSetCookie <$> getHeader "Set-Cookie" rsp2
  liftIO $ assertEqual "Wrong cookie" (Just "zuid") (setCookieName <$> zuid)
  pure mem

addUserToTeamWithRole' :: HasCallStack => Maybe Role -> UserId -> TeamId -> TestM (Invitation, ResponseLBS)
addUserToTeamWithRole' role inviter tid = do
  brig <- viewBrig
  inviteeEmail <- randomEmail
  let invite = InvitationRequest Nothing role Nothing inviteeEmail Nothing
  invResponse <- postInvitation tid inviter invite
  inv <- responseJsonError invResponse
  inviteeCode <- getInvitationCode tid (inInvitation inv)
  r <-
    post
      ( brig
          . path "/register"
          . contentJson
          . body (acceptInviteBody inviteeEmail inviteeCode)
      )
  pure (inv, r)

addUserToTeamWithSSO :: HasCallStack => Bool -> TeamId -> TestM TeamMember
addUserToTeamWithSSO hasEmail tid = do
  let ssoid = UserSSOId mkSimpleSampleUref
  uid <- fmap (\(u :: User) -> u.userId) $ responseJsonError =<< postSSOUser "SSO User" hasEmail ssoid tid
  getTeamMember uid tid uid

makeOwner :: HasCallStack => UserId -> TeamMember -> TeamId -> TestM ()
makeOwner owner mem tid = do
  galley <- viewGalley
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
  brig <- viewBrig
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
  brig <- viewBrig

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
        pure $ fromByteString . Text.encodeUtf8 =<< lbs ^? key "code" . _String

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
  g <- viewGalley
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
  Maybe (Set AccessRole) ->
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
  Maybe (Set AccessRole) ->
  Maybe Milliseconds ->
  Maybe RoleName ->
  TestM ResponseLBS
createTeamConvAccessRaw u tid us name acc role mtimer convRole = do
  g <- viewGalley
  let tinfo = ConvTeamInfo tid
  let conv =
        NewConv us [] (name >>= checked) (fromMaybe (Set.fromList []) acc) role (Just tinfo) mtimer Nothing (fromMaybe roleNameWireAdmin convRole) BaseProtocolProteusTag
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
  ClientId ->
  TeamId ->
  UserList UserId ->
  Maybe Text ->
  Maybe (Set Access) ->
  Maybe (Set AccessRole) ->
  Maybe Milliseconds ->
  Maybe RoleName ->
  m (Local ConvId)
createMLSTeamConv lusr c tid users name access role timer convRole = do
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
            newConvProtocol = BaseProtocolMLSTag
          }
  r <-
    post
      ( g
          . path "/conversations"
          . zUser (tUnqualified lusr)
          . zClient c
          . zConn "conn"
          . zType "access"
          . json conv
      )
  convId <- fromBS (getHeader' "Location" r)
  pure $ qualifyAs lusr convId

updateTeamConv :: UserId -> ConvId -> ConversationRename -> TestM ResponseLBS
updateTeamConv zusr convid upd = do
  g <- viewGalley
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
  g <- viewGalley
  let conv =
        NewConv [u2] [] (n >>= checked) mempty Nothing (Just $ ConvTeamInfo tid) Nothing Nothing roleNameWireAdmin BaseProtocolProteusTag
  post $ g . path "/conversations/one2one" . zUser u1 . zConn "conn" . zType "access" . json conv

postConv ::
  UserId ->
  [UserId] ->
  Maybe Text ->
  [Access] ->
  Maybe (Set AccessRole) ->
  Maybe Milliseconds ->
  TestM ResponseLBS
postConv u us name a r mtimer = postConvWithRole u us name a r mtimer roleNameWireAdmin

defNewProteusConv :: NewConv
defNewProteusConv = NewConv [] [] Nothing mempty Nothing Nothing Nothing Nothing roleNameWireAdmin BaseProtocolProteusTag

defNewMLSConv :: NewConv
defNewMLSConv =
  defNewProteusConv
    { newConvProtocol = BaseProtocolMLSTag
    }

postConvQualified ::
  UserId ->
  Maybe ClientId ->
  NewConv ->
  TestM ResponseLBS
postConvQualified u c n = do
  g <- viewGalley
  post $
    g
      . path "/conversations"
      . zUser u
      . maybe Imports.id zClient c
      . zConn "conn"
      . zType "access"
      . json n

postConvWithRemoteUsersGeneric ::
  HasCallStack =>
  Mock LByteString ->
  UserId ->
  Maybe ClientId ->
  NewConv ->
  TestM (Response (Maybe LByteString))
postConvWithRemoteUsersGeneric m u c n = do
  let mock =
        ("get-not-fully-connected-backends" ~> NonConnectedBackends mempty)
          <|> m
  fmap fst $
    withTempMockFederator' mock $
      postConvQualified u c n {newConvName = setName (newConvName n)}
        <!! const 201 === statusCode
  where
    setName :: (KnownNat n, KnownNat m, Within Text n m) => Maybe (Range n m Text) -> Maybe (Range n m Text)
    setName Nothing = checked "federated gossip"
    setName x = x

postConvWithRemoteUsers ::
  HasCallStack =>
  UserId ->
  Maybe ClientId ->
  NewConv ->
  TestM (Response (Maybe LByteString))
postConvWithRemoteUsers u c n =
  fmap fst $
    withTempMockFederator' (("get-not-fully-connected-backends" ~> NonConnectedBackends mempty) <|> mockReply EmptyResponse) $
      postConvQualified u c n {newConvName = setName (newConvName n)}
        <!! const 201
          === statusCode
  where
    setName :: (KnownNat n, KnownNat m, Within Text n m) => Maybe (Range n m Text) -> Maybe (Range n m Text)
    setName Nothing = checked "federated gossip"
    setName x = x

postTeamConv :: TeamId -> UserId -> [UserId] -> Maybe Text -> [Access] -> Maybe (Set AccessRole) -> Maybe Milliseconds -> TestM ResponseLBS
postTeamConv tid u us name a r mtimer = do
  g <- viewGalley
  let conv = NewConv us [] (name >>= checked) (Set.fromList a) r (Just (ConvTeamInfo tid)) mtimer Nothing roleNameWireAdmin BaseProtocolProteusTag
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
  Maybe (Set AccessRole) ->
  Maybe Milliseconds ->
  RoleName ->
  TestM ResponseLBS
postConvWithRole u members name access arole timer role =
  postConvQualified
    u
    Nothing
    defNewProteusConv
      { newConvUsers = members,
        newConvName = name >>= checked,
        newConvAccess = Set.fromList access,
        newConvAccessRoles = arole,
        newConvMessageTimer = timer,
        newConvUsersRole = role
      }

postConvWithReceipt :: UserId -> [UserId] -> Maybe Text -> [Access] -> Maybe (Set AccessRole) -> Maybe Milliseconds -> ReceiptMode -> TestM ResponseLBS
postConvWithReceipt u us name a r mtimer rcpt = do
  g <- viewGalley
  let conv = NewConv us [] (name >>= checked) (Set.fromList a) r Nothing mtimer (Just rcpt) roleNameWireAdmin BaseProtocolProteusTag
  post $ g . path "/conversations" . zUser u . zConn "conn" . zType "access" . json conv

postSelfConv :: UserId -> TestM ResponseLBS
postSelfConv u = do
  g <- viewGalley
  post $ g . path "/conversations/self" . zUser u . zConn "conn" . zType "access"

postO2OConv :: UserId -> UserId -> Maybe Text -> TestM ResponseLBS
postO2OConv u1 u2 n = do
  g <- viewGalley
  let conv = NewConv [u2] [] (n >>= checked) mempty Nothing Nothing Nothing Nothing roleNameWireAdmin BaseProtocolProteusTag
  post $ g . path "/conversations/one2one" . zUser u1 . zConn "conn" . zType "access" . json conv

postConnectConv :: UserId -> UserId -> Text -> Text -> Maybe Text -> TestM ResponseLBS
postConnectConv a b name msg email = do
  qb <- Qualified b <$> viewFederationDomain
  g <- viewGalley
  post $
    g
      . path "/i/conversations/connect"
      . zUser a
      . zConn "conn"
      . zType "access"
      . json (Connect qb (Just msg) (Just name) email)

putConvAccept :: UserId -> ConvId -> TestM ResponseLBS
putConvAccept invited cid = do
  g <- viewGalley
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
  g <- viewGalley
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
  Mock LByteString ->
  TestM (ResponseLBS, [FederatedRequest])
postProteusMessageQualifiedWithMockFederator senderUser senderClient convId recipients dat strat mock =
  withTempMockFederator' mock $
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
  def = Broadcast BroadcastLegacyQueryParams BroadcastJSON mempty "ZXhhbXBsZQ==" mempty Imports.id

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
        BroadcastLegacyQueryParams -> (Nothing, maybe Imports.id mkOtrReportMissing (bReport b))
        _ -> (bReport b, Imports.id)
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
    g
      . bReq b
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
    fn :: ToByteString a => a -> Text
    fn = fromJust . fromByteString . toByteString'

postProtoOtrMessage :: UserId -> ClientId -> ConvId -> OtrRecipients -> TestM ResponseLBS
postProtoOtrMessage = postProtoOtrMessage' Nothing Imports.id

postProtoOtrMessage' :: Maybe [UserId] -> (Request -> Request) -> UserId -> ClientId -> ConvId -> OtrRecipients -> TestM ResponseLBS
postProtoOtrMessage' reportMissing modif u d c rec = do
  g <- viewGalley
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

getConvs :: HasCallStack => UserId -> [Qualified ConvId] -> TestM ResponseLBS
getConvs u cids = do
  g <- viewGalley
  post $
    g
      . path "/conversations/list"
      . zUser u
      . zConn "conn"
      . json (ListConversations (unsafeRange cids))

getConvClients :: HasCallStack => GroupId -> TestM ClientList
getConvClients gid = do
  g <- viewGalley
  responseJsonError
    =<< get
      ( g
          . paths ["i", "group", B64U.encode $ unGroupId gid]
      )

getAllConvs :: HasCallStack => UserId -> TestM [Conversation]
getAllConvs u = do
  g <- viewGalley
  cids <- do
    r :: ConvIdsPage <-
      responseJsonError
        =<< post
          ( g
              . path "/conversations/list-ids"
              . zUser u
              . zConn "conn"
              . json
                ( GetPaginatedConversationIds Nothing maxBound ::
                    GetPaginatedConversationIds
                )
          )
          <!! const 200 === statusCode
    pure (mtpResults r)
  r <-
    responseJsonError
      =<< getConvs u cids
        <!! const 200 === statusCode
  pure (crFound r)

listConvs ::
  (MonadIO m, MonadHttp m, MonadReader TestSetup m) =>
  UserId ->
  ListConversations ->
  m ResponseLBS
listConvs u req = do
  g <- view tsUnversionedGalley
  post $
    g
      . path "/v1/conversations/list/v2"
      . zUser u
      . zConn "conn"
      . zType "access"
      . json req

getConv ::
  ( MonadIO m,
    MonadHttp m,
    MonadReader TestSetup m,
    HasCallStack
  ) =>
  UserId ->
  ConvId ->
  m ResponseLBS
getConv u c = do
  g <- view tsUnversionedGalley
  get $
    g
      . paths ["v2", "conversations", toByteString' c]
      . zUser u
      . zConn "conn"
      . zType "access"

getConvQualifiedV2 ::
  ( MonadReader TestSetup m,
    MonadHttp m
  ) =>
  UserId ->
  Qualified ConvId ->
  m ResponseLBS
getConvQualifiedV2 u qcnv = do
  g <- view tsUnversionedGalley
  get $
    g
      . paths
        [ "v2",
          "conversations",
          toByteString' (qDomain qcnv),
          toByteString' (qUnqualified qcnv)
        ]
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

getConvIdsV2 :: UserId -> Maybe (Either [ConvId] ConvId) -> Maybe Int32 -> TestM ResponseLBS
getConvIdsV2 u r s = do
  -- The endpoint is removed starting V3
  g <- fmap (addPrefixAtVersion V2 .) (view tsUnversionedGalley)
  get $
    g
      . path "/conversations/ids"
      . zUser u
      . zConn "conn"
      . zType "access"
      . convRange r s

getConvPage :: UserId -> Maybe ConversationPagingState -> Maybe Int32 -> TestM ResponseLBS
getConvPage u state count = do
  g <- viewGalley
  getConvPageWithGalley g u state count

getConvPageWithGalley ::
  (Request -> Request) ->
  UserId ->
  Maybe ConversationPagingState ->
  Maybe Int32 ->
  TestM ResponseLBS
getConvPageWithGalley g u state count =
  post $
    g
      . path "/conversations/list-ids"
      . zUser u
      . zConn "conn"
      -- generate JSON by hand here, so we can bypass the static range check
      . json
        ( object
            ( map (\n -> "size" .= toJSON n) (toList count)
                <> map (\s -> "paging_state" .= toJSON s) (toList state)
            )
        )

-- | Does not page through conversation list
listRemoteConvs :: Domain -> UserId -> TestM [Qualified ConvId]
listRemoteConvs remoteDomain uid = do
  allConvs <-
    fmap mtpResults . responseJsonError @_ @ConvIdsPage
      =<< getConvPage uid Nothing (Just 100) <!! const 200 === statusCode
  pure $ filter (\qcnv -> qDomain qcnv == remoteDomain) allConvs

postQualifiedMembers ::
  (MonadReader TestSetup m, MonadHttp m, HasGalley m) =>
  UserId ->
  NonEmpty (Qualified UserId) ->
  Qualified ConvId ->
  m ResponseLBS
postQualifiedMembers zusr invitees conv = do
  g <- viewGalley
  let invite = InviteQualified invitees roleNameWireAdmin
  post $
    g
      . paths
        [ "conversations",
          toByteString' . qDomain $ conv,
          toByteString' . qUnqualified $ conv,
          "members"
        ]
      . zUser zusr
      . zConn "conn"
      . zType "access"
      . json invite

postMembers ::
  (MonadIO m, MonadHttp m, HasGalley m) =>
  UserId ->
  NonEmpty (Qualified UserId) ->
  Qualified ConvId ->
  m ResponseLBS
postMembers u us c = postMembersWithRole u us c roleNameWireAdmin

postMembersWithRole ::
  (MonadIO m, MonadHttp m, HasGalley m) =>
  UserId ->
  NonEmpty (Qualified UserId) ->
  Qualified ConvId ->
  RoleName ->
  m ResponseLBS
postMembersWithRole u us c r = do
  g <- viewGalley
  let i = InviteQualified us r
  post $
    g
      . paths
        [ "conversations",
          toByteString' (qDomain c),
          toByteString' (qUnqualified c),
          "members"
        ]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json i

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
  g <- viewGalley
  get $
    g
      . paths ["conversations", toByteString' c, "self"]
      . zUser u
      . zConn "conn"
      . zType "access"

putMember :: UserId -> MemberUpdate -> Qualified ConvId -> TestM ResponseLBS
putMember u m (Qualified c dom) = do
  g <- viewGalley
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
  g <- viewGalley
  put $
    g
      . paths ["conversations", toByteString' c, "members", toByteString' to]
      . zUser from
      . zConn "conn"
      . zType "access"
      . json m

putQualifiedConversationName ::
  (HasCallStack, HasGalley m, MonadIO m, MonadHttp m) =>
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
  g <- viewGalley
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
  g <- viewGalley
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
  g <- viewGalley
  get $
    g
      . paths ["/conversations", "join"]
      . zUser u
      . queryItem "key" (toByteString' k)
      . queryItem "code" (toByteString' v)

postJoinConv :: UserId -> ConvId -> TestM ResponseLBS
postJoinConv u c = do
  -- This endpoint is removed from version v5 onwards
  g <- fmap (addPrefixAtVersion V4 .) (view tsUnversionedGalley)
  post $
    g
      . paths ["/conversations", toByteString' c, "join"]
      . zUser u
      . zConn "conn"
      . zType "access"

postJoinCodeConv :: UserId -> ConversationCode -> TestM ResponseLBS
postJoinCodeConv = postJoinCodeConv' Nothing

postJoinCodeConv' :: Maybe PlainTextPassword8 -> UserId -> ConversationCode -> TestM ResponseLBS
postJoinCodeConv' mPw u j = do
  g <- viewGalley
  post $
    g
      . paths ["conversations", "join"]
      . zUser u
      . zConn "conn"
      . zType "access"
      -- `json (JoinConversationByCode j Nothing)` and `json j` are equivalent, using the latter to test backwards compatibility
      . (if isJust mPw then json (JoinConversationByCode j mPw) else json j)

deleteFederation ::
  (MonadHttp m, HasGalley m, MonadIO m) =>
  Domain ->
  m ResponseLBS
deleteFederation dom = do
  g <- viewGalley
  delete $
    g . paths ["/i/federation", toByteString' dom]

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
  g <- viewGalley
  put $
    g
      . paths ["/conversations", toByteString' c, "message-timer"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json acc

postConvCode :: UserId -> ConvId -> TestM ResponseLBS
postConvCode = postConvCode' Nothing

postConvCode' :: Maybe PlainTextPassword8 -> UserId -> ConvId -> TestM ResponseLBS
postConvCode' mPw u c = do
  g <- viewGalley
  post $
    g
      . paths ["/conversations", toByteString' c, "code"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json (CreateConversationCodeRequest mPw)

postConvCodeCheck :: ConversationCode -> TestM ResponseLBS
postConvCodeCheck code = do
  g <- viewGalley
  post $
    g
      . path "/conversations/code-check"
      . json code

getConvCode :: UserId -> ConvId -> TestM ResponseLBS
getConvCode u c = do
  g <- viewGalley
  get $
    g
      . paths ["/conversations", toByteString' c, "code"]
      . zUser u
      . zConn "conn"
      . zType "access"

deleteConvCode :: UserId -> ConvId -> TestM ResponseLBS
deleteConvCode u c = do
  g <- viewGalley
  delete $
    g
      . paths ["/conversations", toByteString' c, "code"]
      . zUser u
      . zConn "conn"
      . zType "access"

deleteUser :: (MonadIO m, MonadHttp m, HasGalley m, HasCallStack) => UserId -> m ResponseLBS
deleteUser u = do
  g <- viewGalley
  delete (g . path "/i/user" . zUser u)

getTeamQueue :: HasCallStack => UserId -> Maybe NotificationId -> Maybe (Int, Bool) -> Bool -> TestM [(NotificationId, UserId)]
getTeamQueue zusr msince msize onlyLast =
  parseEventList . responseJsonUnsafe
    <$> ( getTeamQueue' zusr msince (fst <$> msize) onlyLast
            <!! const 200
              === statusCode
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
    parseEvts qn = (qn ^. queuedNotificationId,) <$> toList (qn ^. queuedNotificationPayload)

    parseEvt :: Object -> UserId
    parseEvt o = case fromJSON (Object o) of
      (Error msg) -> error msg
      (Success (e :: TE.Event)) ->
        case e ^. TE.eventData of
          EdMemberJoin uid -> uid
          _ -> error ("bad event type: " <> show (TE.eventType e))

getTeamQueue' :: HasCallStack => UserId -> Maybe NotificationId -> Maybe Int -> Bool -> TestM ResponseLBS
getTeamQueue' zusr msince msize onlyLast = do
  g <- viewGalley
  get
    ( g
        . path "/teams/notifications"
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
  void $
    runFedClient @"on-conversation-created" fedGalleyClient (qDomain convId) $
      ConversationCreated
        { time = now,
          origUserId = originUser,
          cnvId = qUnqualified convId,
          cnvType = RegularConv,
          cnvAccess = [],
          cnvAccessRoles = Set.fromList [TeamMemberAccessRole, NonTeamMemberAccessRole],
          cnvName = name,
          nonCreatorMembers = othMembers,
          messageTimer = Nothing,
          receiptMode = Nothing,
          protocol = ProtocolProteus
        }

getFeatureStatusMulti :: forall cfg. KnownSymbol (FeatureSymbol cfg) => Multi.TeamFeatureNoConfigMultiRequest -> TestM ResponseLBS
getFeatureStatusMulti req = do
  g <- viewGalley
  post
    ( g
        . paths ["i", "features-multi-teams", featureNameBS @cfg]
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
  assertEqual "id" c1.cnvQualifiedId c2.cnvQualifiedId
  assertEqual "type" (Conv.cnvType c1) (Conv.cnvType c2)
  assertEqual "creator" (Conv.cnvCreator c1) (Conv.cnvCreator c2)
  assertEqual "access" (accessSet c1) (accessSet c2)
  assertEqual "name" (Conv.cnvName c1) (Conv.cnvName c2)
  assertEqual "self member" (selfMember c1) (selfMember c2)
  assertEqual "other members" (otherMembers c1) (otherMembers c2)
  where
    accessSet = Set.fromList . toList . Conv.cnvAccess
    selfMember = cmSelf . cnvMembers
    otherMembers = Set.fromList . cmOthers . cnvMembers

assertConv ::
  HasCallStack =>
  Response (Maybe Lazy.ByteString) ->
  ConvType ->
  Maybe UserId ->
  Qualified UserId ->
  [Qualified UserId] ->
  Maybe Text ->
  Maybe Milliseconds ->
  TestM (Qualified ConvId)
assertConv r t c s us n mt = assertConvWithRole r t c s us n mt roleNameWireAdmin

assertConvWithRole ::
  HasCallStack =>
  Response (Maybe Lazy.ByteString) ->
  ConvType ->
  Maybe UserId ->
  Qualified UserId ->
  [Qualified UserId] ->
  Maybe Text ->
  Maybe Milliseconds ->
  RoleName ->
  TestM (Qualified ConvId)
assertConvWithRole r t c s us n mt role = do
  cId <- fromBS $ getHeader' "Location" r
  cnv <- responseJsonError r
  let _self = cmSelf (cnvMembers cnv)
  let others = cmOthers (cnvMembers cnv)
  liftIO $ do
    assertEqual "id" cId (qUnqualified cnv.cnvQualifiedId)
    assertEqual "name" n (Conv.cnvName cnv)
    assertEqual "type" t (Conv.cnvType cnv)
    assertEqual "creator" c (cnvCreator cnv)
    assertEqual "message_timer" mt (cnvMessageTimer cnv)
    assertEqual "self" s (memId _self)
    assertEqual "others" (Set.fromList $ us) (Set.fromList . map omQualifiedId . toList $ others)
    assertEqual "creator is always and admin" roleNameWireAdmin (memConvRoleName _self)
    assertBool "others role" (all ((== role) . omConvRoleName) (toList others))
    assertBool "otr muted ref not empty" (isNothing (memOtrMutedRef _self))
    assertBool "otr archived not false" (not (memOtrArchived _self))
    assertBool "otr archived ref not empty" (isNothing (memOtrArchivedRef _self))
    case t of
      SelfConv -> assertEqual "access" privateAccess (Conv.cnvAccess cnv)
      ConnectConv -> assertEqual "access" privateAccess (Conv.cnvAccess cnv)
      One2OneConv -> assertEqual "access" privateAccess (Conv.cnvAccess cnv)
      _ -> pure ()
  pure (cnvQualifiedId cnv)

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
  Qualified ConvId ->
  ByteString ->
  Notification ->
  IO ()
wsAssertMLSWelcome u cid welcome n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= cid
  evtType e @?= MLSWelcome
  evtFrom e @?= u
  evtData e @?= EdMLSWelcome welcome

wsAssertMLSMessage ::
  HasCallStack =>
  Qualified ConvOrSubConvId ->
  Qualified UserId ->
  ByteString ->
  Notification ->
  IO ()
wsAssertMLSMessage qcs u message n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  assertMLSMessageEvent qcs u message e

wsAssertClientRemoved ::
  HasCallStack =>
  ClientId ->
  Notification ->
  IO ()
wsAssertClientRemoved cid n = do
  let j = Object $ List1.head (ntfPayload n)
  let etype = j ^? key "type" . _String
  let eclient = j ^? key "client" . key "id" . _String
  etype @?= Just "user.client-remove"
  (fromByteString . T.encodeUtf8 =<< eclient) @?= Just cid

wsAssertClientAdded ::
  HasCallStack =>
  ClientId ->
  Notification ->
  IO ()
wsAssertClientAdded cid n = do
  let j = Object $ List1.head (ntfPayload n)
  let etype = j ^? key "type" . _String
  let eclient = j ^? key "client" . key "id" . _String
  etype @?= Just "user.client-add"
  (fromByteString . T.encodeUtf8 =<< eclient) @?= Just cid

assertMLSMessageEvent ::
  HasCallStack =>
  Qualified ConvOrSubConvId ->
  Qualified UserId ->
  ByteString ->
  Conv.Event ->
  IO ()
assertMLSMessageEvent qcs u message e = do
  evtConv e @?= (.conv) <$> qcs
  case qUnqualified qcs of
    Conv _ -> pure ()
    SubConv _ subconvId ->
      evtSubConv e @?= Just subconvId
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

assertJoinEvent :: Qualified ConvId -> Qualified UserId -> [Qualified UserId] -> RoleName -> Conv.Event -> IO ()
assertJoinEvent conv usr new role e = do
  evtConv e @?= conv
  evtType e @?= Conv.MemberJoin
  evtFrom e @?= usr
  fmap (sort . mMembers) (evtData e ^? _EdMembersJoin) @?= Just (sort (fmap (`SimpleMember` role) new))

wsAssertFederationDeleted ::
  HasCallStack =>
  Domain ->
  Notification ->
  IO ()
wsAssertFederationDeleted dom n = do
  ntfTransient n @?= False
  assertFederationDeletedEvent dom $ List1.head (WS.unpackPayload n)

assertFederationDeletedEvent ::
  Domain ->
  Fed.Event ->
  IO ()
assertFederationDeletedEvent dom e = do
  Fed._eventType e @?= Fed.FederationDelete
  Fed._eventDomain e @?= dom

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
  Conv.Event ->
  IO ()
assertLeaveEvent conv usr leaving e = do
  evtConv e @?= conv
  evtType e @?= Conv.MemberLeave
  evtFrom e @?= usr
  fmap (sort . qualifiedUserIdList) (evtData e ^? _EdMembersLeave . _2) @?= Just (sort leaving)

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

wsAssertMemberLeave :: Qualified ConvId -> Qualified UserId -> [Qualified UserId] -> EdMemberLeftReason -> Notification -> IO ()
wsAssertMemberLeave conv usr old reason n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= conv
  evtType e @?= Conv.MemberLeave
  evtFrom e @?= usr
  sorted (evtData e) @?= sorted (EdMembersLeave reason (QualifiedUserIdList old))
  where
    sorted (EdMembersLeave _ (QualifiedUserIdList m)) = EdMembersLeave reason (QualifiedUserIdList (sort m))
    sorted x = x

wsAssertTyping :: HasCallStack => Qualified ConvId -> Qualified UserId -> TypingStatus -> Notification -> IO ()
wsAssertTyping conv usr ts n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= True
  evtConv e @?= conv
  evtType e @?= Conv.Typing
  evtFrom e @?= usr
  evtData e @?= EdTyping ts

assertNoMsg :: HasCallStack => WS.WebSocket -> (Notification -> Assertion) -> TestM ()
assertNoMsg ws f = do
  x <- WS.awaitMatch (1 # Second) ws f
  liftIO $ case x of
    Left _ -> pure () -- expected
    Right _ -> assertFailure "Unexpected message"

assertRemoveUpdate :: (MonadIO m, HasCallStack) => FederatedRequest -> Qualified ConvId -> Qualified UserId -> [UserId] -> Qualified UserId -> m ()
assertRemoveUpdate req qconvId remover alreadyPresentUsers victim = liftIO $ do
  frRPC req @?= "on-conversation-updated"
  frOriginDomain req @?= qDomain qconvId
  cu <- assertJust $ decode (frBody req)
  cuOrigUserId cu @?= remover
  cuConvId cu @?= qUnqualified qconvId
  sort (cuAlreadyPresentUsers cu) @?= sort alreadyPresentUsers
  cuAction cu
    @?= SomeConversationAction
      (sing @'ConversationRemoveMembersTag)
      (ConversationRemoveMembers (pure victim) EdReasonRemoved)

assertLeaveUpdate :: (MonadIO m, HasCallStack) => FederatedRequest -> Qualified ConvId -> Qualified UserId -> [UserId] -> m ()
assertLeaveUpdate req qconvId remover alreadyPresentUsers = liftIO $ do
  frRPC req @?= "on-conversation-updated"
  frOriginDomain req @?= qDomain qconvId
  cu <- assertJust $ decode (frBody req)
  cuOrigUserId cu @?= remover
  cuConvId cu @?= qUnqualified qconvId
  sort (cuAlreadyPresentUsers cu) @?= sort alreadyPresentUsers
  cuAction cu @?= SomeConversationAction (sing @'ConversationLeaveTag) ()

-------------------------------------------------------------------------------
-- Helpers

testResponse :: HasCallStack => Int -> Maybe TestErrorLabel -> Assertions ()
testResponse status mlabel = do
  const status === statusCode
  case mlabel of
    Just label -> responseJsonEither === const (Right label)
    Nothing -> (isLeft <$> responseJsonEither @TestErrorLabel) === const True

newtype TestErrorLabel = TestErrorLabel {fromTestErrorLabel :: Text}
  deriving (Eq, Show)

instance IsString TestErrorLabel where
  fromString = TestErrorLabel . cs

instance FromJSON TestErrorLabel where
  parseJSON = fmap TestErrorLabel . withObject "TestErrorLabel" (.: "label")

decodeConvCode :: Response (Maybe Lazy.ByteString) -> ConversationCodeInfo
decodeConvCode = responseJsonUnsafe

decodeConvCodeEvent :: Response (Maybe Lazy.ByteString) -> ConversationCodeInfo
decodeConvCodeEvent r = case responseJsonUnsafe r of
  (Event _ _ _ _ (EdConvCodeUpdate c)) -> c
  _ -> error "Failed to parse ConversationCode from Event"

decodeConvId :: HasCallStack => Response (Maybe Lazy.ByteString) -> ConvId
decodeConvId = qUnqualified . decodeQualifiedConvId

decodeQualifiedConvId :: HasCallStack => Response (Maybe Lazy.ByteString) -> Qualified ConvId
decodeQualifiedConvId = cnvQualifiedId . responseJsonUnsafe

decodeConvList :: HasCallStack => Response (Maybe Lazy.ByteString) -> [Conversation]
decodeConvList = convList . responseJsonUnsafeWithMsg "conversations"

decodeConvIdList :: HasCallStack => Response (Maybe Lazy.ByteString) -> [ConvId]
decodeConvIdList = convList . responseJsonUnsafeWithMsg "conversation-ids"

decodeQualifiedConvIdList :: Response (Maybe Lazy.ByteString) -> Either String [Qualified ConvId]
decodeQualifiedConvIdList = fmap mtpResults . responseJsonEither @ConvIdsPage

zUser :: UserId -> Request -> Request
zUser = header "Z-User" . toByteString'

zBot :: UserId -> Request -> Request
zBot = header "Z-Bot" . toByteString'

zClient :: ClientId -> Request -> Request
zClient = header "Z-Client" . toByteString'

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
connectUsersUnchecked = connectUsersWith Imports.id

connectUsersWith ::
  (Request -> Request) ->
  UserId ->
  List1 UserId ->
  TestM (List1 (Response (Maybe Lazy.ByteString), Response (Maybe Lazy.ByteString)))
connectUsersWith fn u = mapM connectTo
  where
    connectTo v = do
      b <- view tsUnversionedBrig
      r1 <-
        post
          ( b
              . zUser u
              . zConn "conn"
              . paths ["v1", "connections"]
              . json (ConnectionRequest v (unsafeRange "chat"))
              . fn
          )
      r2 <-
        put
          ( b
              . zUser v
              . zConn "conn"
              . paths ["v1", "connections", toByteString' u]
              . json (ConnectionUpdate Accepted)
              . fn
          )
      pure (r1, r2)

connectWithRemoteUser ::
  (HasBrig m, MonadIO m, MonadHttp m, MonadCatch m, HasCallStack) =>
  UserId ->
  Qualified UserId ->
  m ()
connectWithRemoteUser self other = do
  let req = CreateConnectionForTest self other
  b <- viewBrig
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
  brig <- view tsUnversionedBrig
  post $
    brig
      . paths ["v1", "connections"]
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
  brig <- viewBrig
  post $
    brig
      . paths ["connections", toByteString' toDomain, toByteString' toUser]
      . contentJson
      . zUser from
      . zConn "conn"

-- | A copy of 'putConnection' from Brig integration tests.
putConnection :: UserId -> UserId -> Relation -> TestM ResponseLBS
putConnection from to r = do
  brig <- view tsUnversionedBrig
  put $
    brig
      . paths ["v1", "connections", toByteString' to]
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
  brig <- view tsUnversionedBrig
  put $
    brig
      . paths ["v1", "connections", toByteString' to]
      . contentJson
      . body payload
      . zUser from
      . zConn "conn"
  where
    payload = RequestBodyLBS . encode $ object ["status" .= r]

-- | A copy of `assertConnections from Brig integration tests.
assertConnections :: HasCallStack => UserId -> [ConnectionStatus] -> TestM ()
assertConnections u cstat = do
  brig <- view tsUnversionedBrig
  resp <- listConnections brig u <!! const 200 === statusCode
  let cstat' :: [ConnectionStatus]
      cstat' = fmap status . clConnections . fromMaybe (error "bad response") . responseJsonMaybe $ resp
  unless (all (`elem` cstat') cstat) $
    error $
      "connection check failed: " <> show cstat <> " is not a subset of " <> show cstat'
  where
    status c = ConnectionStatus (ucFrom c) (qUnqualified $ ucTo c) (ucStatus c)
    listConnections brig usr = get $ brig . paths ["v1", "connections"] . zUser usr

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
  b <- viewBrig
  e <- liftIO randomEmail
  let p =
        object $
          ["name" .= fromEmail e]
            <> ["password" .= defPassword | hasPassword]
            <> ["email" .= fromEmail e | hasEmail]
            <> ["team" .= BindingNewTeam (newNewTeam (unsafeRange "teamName") DefaultIcon) | isCreator]
  responseJsonUnsafe <$> (post (b . path "/i/users" . json p) <!! const 201 === statusCode)

ephemeralUser :: HasCallStack => TestM UserId
ephemeralUser = do
  b <- viewBrig
  name <- UUID.toText <$> liftIO nextRandom
  let p = object ["name" .= name]
  r <- post (b . path "/register" . json p) <!! const 201 === statusCode
  user <- responseJsonError r
  pure $ Wire.API.User.userId user

randomClient :: HasCallStack => UserId -> LastPrekey -> TestM ClientId
randomClient uid lk = randomClientWithCaps uid lk Nothing

randomClientWithCaps :: HasCallStack => UserId -> LastPrekey -> Maybe (Set Client.ClientCapability) -> TestM ClientId
randomClientWithCaps uid lk caps = do
  b <- viewBrig
  resp <-
    post
      ( b
          . paths ["i", "clients", toByteString' uid]
          . queryItem "skip_reauth" "true"
          . json newClientBody
      )
      <!! const rStatus
        === statusCode
  client <- responseJsonError resp
  pure (clientId client)
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
  b <- view tsUnversionedBrig
  fmap profileDeleted . responseJsonMaybe
    <$> get
      ( b
          . paths ["v1", "users", toByteString' u]
          . zUser from
          . zConn "conn"
      )

getClients :: UserId -> TestM ResponseLBS
getClients u = do
  b <- viewBrig
  get $
    b
      . paths ["clients"]
      . zUser u
      . zConn "conn"

getInternalClientsFull :: UserSet -> TestM UserClientsFull
getInternalClientsFull userSet = do
  b <- viewBrig
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
  clnt <- assertOne . filter ((== cid) . clientId) $ Set.toList clnts
  liftIO $ assertEqual ("ensureClientCaps: " <> show (uid, cid, caps)) (clientCapabilities clnt) caps

-- TODO: Refactor, as used also in brig
deleteClient :: UserId -> ClientId -> Maybe PlainTextPassword6 -> TestM ResponseLBS
deleteClient u c pw = do
  b <- viewBrig
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
  b <- viewBrig
  r <-
    get (b . paths ["i", "users", toByteString' u, "status"])
      <!! const 200
        === statusCode
  case responseBody r of
    Nothing -> error $ "getStatus: failed to parse response: " ++ show r
    Just j -> do
      let st = maybeFromJSON =<< j ^? key "status"
      let decoded = fromMaybe (error $ "getStatus: failed to decode status" ++ show j) st
      pure $ decoded == Deleted
  where
    maybeFromJSON :: FromJSON a => Value -> Maybe a
    maybeFromJSON v = case fromJSON v of
      Success a -> Just a
      _ -> Nothing

isMember :: UserId -> ConvId -> TestM Bool
isMember usr cnv = do
  g <- viewGalley
  res <-
    get $
      g
        . paths ["i", "conversations", toByteString' cnv, "members", toByteString' usr]
        . expect2xx
  pure $ isJust (responseJsonMaybe @Member res)

randomUserWithClient :: LastPrekey -> TestM (UserId, ClientId)
randomUserWithClient lk = do
  (u, c) <- randomUserWithClientQualified lk
  pure (qUnqualified u, c)

randomUserWithClientQualified :: LastPrekey -> TestM (Qualified UserId, ClientId)
randomUserWithClientQualified lk = do
  u <- randomQualifiedUser
  c <- randomClient (qUnqualified u) lk
  pure (u, c)

newNonce :: TestM (Id ())
newNonce = randomId

fromBS :: (HasCallStack, FromByteString a, MonadIO m) => ByteString -> m a
fromBS bs =
  case fromByteString bs of
    Nothing -> liftIO $ assertFailure "fromBS: no parse"
    Just x -> pure x

convRange :: Maybe (Either [ConvId] ConvId) -> Maybe Int32 -> Request -> Request
convRange range size =
  maybe Imports.id (queryItem "size" . C.pack . show) size
    . case range of
      Just (Left l) -> queryItem "ids" (C.intercalate "," $ map toByteString' l)
      Just (Right c) -> queryItem "start" (toByteString' c)
      Nothing -> Imports.id

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
  \m r d -> assertMismatchQualified mempty (mk m) (mk r) (mk d) mempty
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
    formatMessage = maybe Imports.id (\msg -> ((msg <> "\n") <>)) mmsg

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
  Client.QualifiedUserClients ->
  Assertions ()
assertMismatchQualified failureToSend missing redundant deleted failedToConfirm = do
  assertExpected "failed to send" failureToSend (fmap mssFailedToSend . responseJsonMaybe)
  assertExpected "missing" missing (fmap mssMissingClients . responseJsonMaybe)
  assertExpected "redundant" redundant (fmap mssRedundantClients . responseJsonMaybe)
  assertExpected "deleted" deleted (fmap mssDeletedClients . responseJsonMaybe)
  assertExpected "failed to confirm clients" failedToConfirm (fmap mssFailedToConfirmClients . responseJsonMaybe)

otrRecipients :: [(UserId, ClientId, Text)] -> OtrRecipients
otrRecipients =
  OtrRecipients
    . UserClientMap
    . fmap Map.fromList
    . foldr ((uncurry Map.insert . fmap pure) . (\(a, b, c) -> (a, (b, c)))) mempty

genRandom :: (Q.Arbitrary a, MonadIO m) => m a
genRandom = liftIO . Q.generate $ Q.arbitrary

defPassword :: PlainTextPassword6
defPassword = plainTextPassword6Unsafe "topsecretdefaultpassword"

randomEmail :: MonadIO m => m Email
randomEmail = do
  uid <- liftIO nextRandom
  pure $ Email ("success+" <> UUID.toText uid) "simulator.amazonses.com"

selfConv :: UserId -> ConvId
selfConv u = Id (toUUID u)

-- TODO: Refactor, as used also in other services
retryWhileN :: (MonadIO m) => Int -> (a -> Bool) -> m a -> m a
retryWhileN n f m =
  retrying
    (constantDelay 1000000 <> limitRetries n)
    (const (pure . f))
    (const m)

-- | Changing this will break tests; all prekeys and client Id must match the same
-- fingerprint
someClientId :: ClientId
someClientId = ClientId 0xcc6e640e296e8bba

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
        (Just creator)
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
  brig <- viewBrig
  post (brig . path "/i/index/refresh") !!! const 200 === statusCode

postSSOUser :: Text -> Bool -> UserSSOId -> TeamId -> TestM ResponseLBS
postSSOUser name hasEmail ssoid teamid = do
  brig <- viewBrig
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

class HasSettingsOverrides m where
  withSettingsOverrides :: (Opts.Opts -> Opts.Opts) -> m a -> m a

instance HasSettingsOverrides TestM where
  withSettingsOverrides f action = do
    ts :: TestSetup <- ask
    let opts = f (ts ^. tsGConf)
    liftIO . lowerCodensity $ do
      (galleyApp, _env) <- Run.mkApp opts -- FUTUREWORK: always call Run.closeApp at the end.
      port' <- withMockServer galleyApp
      liftIO $
        runReaderT
          (runTestM action)
          ( ts
              & tsUnversionedGalley .~ Bilge.host "127.0.0.1" . Bilge.port port'
              & tsFedGalleyClient .~ FedClient (ts ^. tsManager) (Endpoint "127.0.0.1" port')
          )

waitForMemberDeletion :: UserId -> TeamId -> UserId -> TestM ()
waitForMemberDeletion zusr tid uid = do
  maybeTimedOut <- timeout 2000000 loop
  liftIO $
    when (isNothing maybeTimedOut) $
      assertFailure "Timed out waiting for member deletion"
  where
    loop = do
      galley <- viewGalley
      res <- get (galley . paths ["teams", toByteString' tid, "members", toByteString' uid] . zUser zusr)
      case statusCode res of
        404 -> pure ()
        _ -> threadDelay 1000 >> loop

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
  g <- viewGalley
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
  brig <- viewBrig
  let users = BS.intercalate "," $ toByteString' <$> keys
  res <-
    get
      ( brig
          . path "/i/users"
          . queryItem keyName users
          . expect2xx
      )
  let accounts = fromJust $ responseJsonMaybe @[UserAccount] res
  pure $ fmap accountUser accounts

getUserProfile :: UserId -> UserId -> TestM UserProfile
getUserProfile zusr uid = do
  brig <- view tsUnversionedBrig
  res <- get (brig . zUser zusr . paths ["v1", "users", toByteString' uid])
  responseJsonError res

upgradeClientToLH :: HasCallStack => UserId -> ClientId -> TestM ()
upgradeClientToLH zusr cid =
  putCapabilities zusr cid [ClientSupportsLegalholdImplicitConsent]

putCapabilities :: HasCallStack => UserId -> ClientId -> [ClientCapability] -> TestM ()
putCapabilities zusr cid caps = do
  brig <- viewBrig
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
  brig <- view tsUnversionedBrig
  get
    ( brig
        . zUser zusr
        . paths ["v1", "users", toByteString' uid, "prekeys", toByteString' cid]
    )

getUsersPrekeyBundleUnqualified :: HasCallStack => UserId -> UserId -> TestM ResponseLBS
getUsersPrekeyBundleUnqualified zusr uid = do
  brig <- view tsUnversionedBrig
  get
    ( brig
        . zUser zusr
        . paths ["v1", "users", toByteString' uid, "prekeys"]
    )

getMultiUserPrekeyBundleUnqualified :: HasCallStack => UserId -> UserClients -> TestM ResponseLBS
getMultiUserPrekeyBundleUnqualified zusr userClients = do
  brig <- view tsUnversionedBrig
  post
    ( brig
        . zUser zusr
        . paths ["v1", "users", "prekeys"]
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
      profileLegalholdStatus = defUserLegalHoldStatus,
      profileSupportedProtocols = defSupportedProtocols
    }

-- mock federator

-- | Run the given action on a temporary galley instance with access to a mock
-- federator.
withTempMockFederator' ::
  (MonadIO m, MonadMask m, HasSettingsOverrides m) =>
  Mock LByteString ->
  m b ->
  m (b, [FederatedRequest])
withTempMockFederator' resp action = do
  let mock = runMock (assertFailure . Text.unpack) $ do
        r <- resp
        pure ("application" // "json", r)
  Mock.withTempMockFederator
    [("Content-Type", "application/json")]
    mock
    $ \mockPort -> do
      withSettingsOverrides (\opts -> opts & Opts.federator ?~ Endpoint "127.0.0.1" (fromIntegral mockPort)) action

-- Starts a servant Application in Network.Wai.Test session and runs the
-- FederatedRequest against it.
makeFedRequestToServant ::
  forall (api :: Type).
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
    else throw (Mock.MockErrorResponse status (LT.decodeUtf8 bdy))
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

checkUserDeleteEvent :: HasCallStack => UserId -> WS.Timeout -> WS.WebSocket -> TestM ()
checkUserDeleteEvent uid timeout_ w = WS.assertMatch_ timeout_ w $ \notif -> do
  let j = Object $ List1.head (ntfPayload notif)
  let etype = j ^? key "type" . _String
  let euser = j ^? key "id" . _String
  etype @?= Just "user.delete"
  euser @?= Just (UUID.toText (toUUID uid))

checkTeamMemberJoin :: HasCallStack => TeamId -> UserId -> WS.WebSocket -> TestM ()
checkTeamMemberJoin tid uid w = WS.assertMatch_ checkTimeout w $ \notif -> do
  ntfTransient notif @?= True
  let e = List1.head (WS.unpackPayload notif)
  e ^. eventTeam @?= tid
  e ^. eventData @?= EdMemberJoin uid

checkTeamMemberLeave :: HasCallStack => TeamId -> UserId -> WS.WebSocket -> TestM ()
checkTeamMemberLeave tid usr w = WS.assertMatch_ checkTimeout w $ \notif -> do
  ntfTransient notif @?= True
  let e = List1.head (WS.unpackPayload notif)
  e ^. eventTeam @?= tid
  e ^. eventData @?= EdMemberLeave usr

checkTeamUpdateEvent :: (HasCallStack, MonadIO m, MonadCatch m) => TeamId -> TeamUpdateData -> WS.WebSocket -> m ()
checkTeamUpdateEvent tid upd w = WS.assertMatch_ checkTimeout w $ \notif -> do
  ntfTransient notif @?= True
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

wsAssertConvCreate ::
  HasCallStack =>
  Qualified ConvId ->
  Qualified UserId ->
  Notification ->
  IO ()
wsAssertConvCreate conv eventFrom n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= conv
  evtType e @?= Conv.ConvCreate
  evtFrom e @?= eventFrom

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
    Conv.EdMembersLeave _ mm -> mm @?= Conv.QualifiedUserIdList [usr]
    other -> assertFailure $ "Unexpected event data: " <> show other

checkTimeout :: WS.Timeout
checkTimeout = 60 # Second

-- | The function is used in conjuction with 'withTempMockFederator' to mock
-- responses by Brig on the mocked side of federation.
mockedFederatedBrigResponse :: [(Qualified UserId, Text)] -> Mock LByteString
mockedFederatedBrigResponse users = do
  guardComponent Brig
  mockReply [mkProfile mem (Name name) | (mem, name) <- users]

fedRequestsForDomain :: HasCallStack => Domain -> Component -> [FederatedRequest] -> [FederatedRequest]
fedRequestsForDomain domain component =
  filter $ \req -> frTargetDomain req == domain && frComponent req == component

parseFedRequest :: FromJSON a => FederatedRequest -> Either String a
parseFedRequest fr = eitherDecode (frBody fr)

assertOne :: (HasCallStack, MonadIO m, Show a) => [a] -> m a
assertOne [a] = pure a
assertOne xs = liftIO . error $ "Expected exactly one element, found " <> show xs

assertTwo :: (HasCallStack, Show a) => [a] -> (a, a)
assertTwo [a, b] = (a, b)
assertTwo xs = error $ "Expected two elements, found " <> show xs

assertThree :: (HasCallStack, Show a) => [a] -> (a, a, a)
assertThree [a1, a2, a3] = (a1, a2, a3)
assertThree xs = error $ "Expected three elements, found " <> show xs

assertNone :: (HasCallStack, MonadIO m, Show a) => [a] -> m ()
assertNone [] = pure ()
assertNone xs = liftIO . error $ "Expected exactly no elements, found " <> show xs

assertJust :: (HasCallStack, MonadIO m) => Maybe a -> m a
assertJust (Just a) = pure a
assertJust Nothing = liftIO $ error "Expected Just, got Nothing"

iUpsertOne2OneConversation :: UpsertOne2OneConversationRequest -> TestM ResponseLBS
iUpsertOne2OneConversation req = do
  galley <- viewGalley
  post (galley . path "/i/conversations/one2one/upsert" . Bilge.json req)

createOne2OneConvWithRemote :: HasCallStack => Local UserId -> Remote UserId -> TestM ()
createOne2OneConvWithRemote localUser remoteUser = do
  let convId = one2OneConvId BaseProtocolProteusTag (tUntagged localUser) (tUntagged remoteUser)
      mkRequest actor =
        UpsertOne2OneConversationRequest
          { uooLocalUser = localUser,
            uooRemoteUser = remoteUser,
            uooActor = actor,
            uooActorDesiredMembership = Included,
            uooConvId = convId
          }

  iUpsertOne2OneConversation (mkRequest LocalActor) !!! const 200 === statusCode
  iUpsertOne2OneConversation (mkRequest RemoteActor) !!! const 200 === statusCode

generateRemoteAndConvId :: Bool -> Local UserId -> TestM (Remote UserId, Qualified ConvId)
generateRemoteAndConvId = generateRemoteAndConvIdWithDomain (Domain "far-away.example.com")

generateRemoteAndConvIdWithDomain :: Domain -> Bool -> Local UserId -> TestM (Remote UserId, Qualified ConvId)
generateRemoteAndConvIdWithDomain remoteDomain shouldBeLocal lUserId = do
  other <- Qualified <$> randomId <*> pure remoteDomain
  let convId = one2OneConvId BaseProtocolProteusTag (tUntagged lUserId) other
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
       in snd <$> concurrently writeInput readOutput
  case (mout, ex) of
    (Just out, ExitSuccess) -> pure out
    _ -> assertFailure "Process didn't finish successfully"

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

wsAssertBackendRemoveProposalWithEpoch :: HasCallStack => Qualified UserId -> Qualified ConvId -> LeafIndex -> Epoch -> Notification -> IO ByteString
wsAssertBackendRemoveProposalWithEpoch fromUser convId idx epoch n = do
  bs <- wsAssertBackendRemoveProposal fromUser (Conv <$> convId) idx n
  let msg = fromRight (error "Failed to parse Message") $ decodeMLS' @Message bs
  case msg.content of
    MessagePublic pmsg -> liftIO $ pmsg.content.value.epoch @?= epoch
    _ -> assertFailure "unexpected message content"
  pure bs

wsAssertBackendRemoveProposal :: HasCallStack => Qualified UserId -> Qualified ConvOrSubConvId -> LeafIndex -> Notification -> IO ByteString
wsAssertBackendRemoveProposal fromUser cnvOrSubCnv idx n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= (.conv) <$> cnvOrSubCnv
  evtType e @?= MLSMessageAdd
  evtFrom e @?= fromUser
  let bs = getMLSMessageData (evtData e)
  let msg = fromRight (error "Failed to parse Message") $ decodeMLS' @Message bs
  liftIO $ case msg.content of
    MessagePublic pmsg -> do
      pmsg.content.value.sender @?= SenderExternal 0
      case pmsg.content.value.content of
        FramedContentProposal prop -> case prop.value of
          RemoveProposal removedIdx -> removedIdx @?= idx
          otherProp -> assertFailure $ "Expected RemoveProposal but got " <> show otherProp
        otherPayload -> assertFailure $ "Expected ProposalMessage but got " <> show otherPayload
    _ -> assertFailure $ "Expected PublicMessage"
  pure bs
  where
    getMLSMessageData :: Conv.EventData -> ByteString
    getMLSMessageData (EdMLSMessage bs) = bs
    getMLSMessageData d = error ("Expected EdMLSMessage, but got " <> show d)

wsAssertAddProposal ::
  HasCallStack =>
  Qualified UserId ->
  Qualified ConvId ->
  Notification ->
  IO ByteString
wsAssertAddProposal fromUser convId n = do
  let e = List1.head (WS.unpackPayload n)
  ntfTransient n @?= False
  evtConv e @?= convId
  evtType e @?= MLSMessageAdd
  evtFrom e @?= fromUser
  let bs = getMLSMessageData (evtData e)
  let msg = fromRight (error "Failed to parse Message 'MLSPlaintext") $ decodeMLS' @Message bs
  liftIO $ case msg.content of
    MessagePublic pmsg -> do
      pmsg.content.value.sender @?= SenderNewMemberProposal
      case pmsg.content.value.content of
        FramedContentProposal prop -> case prop.value of
          AddProposal _ -> pure ()
          otherProp -> assertFailure $ "Expected AddProposal but got " <> show otherProp
        otherPayload -> assertFailure $ "Expected ProposalMessage but got " <> show otherPayload
    _ -> assertFailure $ "Expected PublicMessage"
  pure bs
  where
    getMLSMessageData :: Conv.EventData -> ByteString
    getMLSMessageData (EdMLSMessage bs) = bs
    getMLSMessageData d = error ("Excepected EdMLSMessage, but got " <> show d)

createAndConnectUsers :: [Maybe Text] -> TestM [Qualified UserId]
createAndConnectUsers domains = do
  localDomain <- viewFederationDomain
  users <- for domains $ maybe randomQualifiedUser (randomQualifiedId . Domain)
  let userPairs = do
        t <- tails users
        (a, others) <- maybeToList (uncons t)
        b <- others
        pure (a, b)
  for_ userPairs $ \(a, b) ->
    case (qDomain a == localDomain, qDomain b == localDomain) of
      (True, True) ->
        connectUsers (qUnqualified a) (pure (qUnqualified b))
      (True, False) -> connectWithRemoteUser (qUnqualified a) b
      (False, True) -> connectWithRemoteUser (qUnqualified b) a
      (False, False) -> pure ()
  pure users

putConversationProtocol :: (MonadIO m, MonadHttp m, HasGalley m, HasCallStack) => UserId -> ClientId -> Qualified ConvId -> ProtocolTag -> m ResponseLBS
putConversationProtocol uid client (Qualified conv domain) protocol = do
  galley <- viewGalley
  put
    ( galley
        . paths ["conversations", toByteString' domain, toByteString' conv, "protocol"]
        . zUser uid
        . zConn "conn"
        . zClient client
        . Bilge.json (object ["protocol" .= protocol])
    )

assertMixedProtocol :: (MonadIO m, HasCallStack) => Conversation -> m ConversationMLSData
assertMixedProtocol conv = do
  case cnvProtocol conv of
    ProtocolMixed mlsData -> pure mlsData
    _ -> liftIO $ assertFailure "Unexpected protocol"

connectBackend :: UserId -> Remote Backend -> TestM [Qualified UserId]
connectBackend usr (tDomain &&& bUsers . tUnqualified -> (d, c)) = do
  users <- replicateM (fromIntegral c) (randomQualifiedId d)
  mapM_ (connectWithRemoteUser usr) users
  pure users
