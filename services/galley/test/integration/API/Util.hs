{-# LANGUAGE OverloadedStrings #-}

module API.Util where

import Bilge hiding (timeout)
import Bilge.Assert
import Brig.Types
import Control.Applicative hiding (empty)
import Control.Error
import Control.Lens hiding ((.=), from, to, (#))
import Control.Monad hiding (mapM_)
import Control.Monad.IO.Class
import Control.Retry
import Data.Aeson hiding (json)
import Data.Aeson.Lens (key)
import Data.ByteString.Char8 (pack, ByteString, intercalate)
import Data.ByteString.Conversion
import Data.Foldable (toList)
import Data.Id
import Data.Int
import Data.List (sort)
import Data.List1 as List1
import Data.Maybe
import Data.Misc
import Data.Monoid
import Data.ProtocolBuffers (encodeMessage)
import Data.Range
import Data.Set (Set)
import Data.Serialize (runPut)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.UUID.V4
import Data.Word
import Galley.Types
import Galley.Types.Teams hiding (EventType (..))
import Galley.Types.Teams.Intra
import GHC.Stack (HasCallStack)
import Gundeck.Types.Notification
import Gundeck.Types.Push
import Prelude hiding (head, mapM_)
import Test.Tasty.Cannon (Cannon, TimeoutUnit (..), (#))
import Test.Tasty.HUnit

import Debug.Trace (traceShow)

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString.Lazy   as Lazy
import qualified Data.Currency          as Currency
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import qualified Data.UUID              as UUID
import qualified Galley.Aws             as Aws
import qualified Galley.Types.Proto     as Proto
import qualified Test.QuickCheck        as Q
import qualified Test.Tasty.Cannon      as WS

type Galley      = Request -> Request
type Brig        = Request -> Request
type ResponseLBS = Response (Maybe Lazy.ByteString)

data TestSetup = TestSetup
  { manager         :: Manager
  , galley          :: Galley
  , brig            :: Brig
  , cannon          :: Cannon
  , awsEnv          :: Maybe Aws.Env
  , maxConvTeamSize :: Word16
  }

-------------------------------------------------------------------------------
-- API Operations

symmPermissions :: [Perm] -> Permissions
symmPermissions p = let s = Set.fromList p in fromJust (newPermissions s s)

createTeam :: HasCallStack => Galley -> Text -> UserId -> [TeamMember] -> Http TeamId
createTeam g name owner mems = do
    let mm = if null mems then Nothing else Just $ unsafeRange (take 127 mems)
    let nt = NonBindingNewTeam $ newNewTeam (unsafeRange name) (unsafeRange "icon") & newTeamMembers .~ mm
    resp <- post (g . path "/teams" . zUser owner . zConn "conn" . zType "access" . json nt) <!! do
        const 201  === statusCode
        const True === isJust . getHeader "Location"
    fromBS (getHeader' "Location" resp)

changeTeamStatus :: HasCallStack => Galley -> TeamId -> TeamStatus -> Http ()
changeTeamStatus g tid s = put
        ( g . paths ["i", "teams", toByteString' tid, "status"]
        . json (TeamStatusUpdate s Nothing)
        ) !!! const 200 === statusCode

createTeamInternal :: HasCallStack => Galley -> Text -> UserId -> Http TeamId
createTeamInternal g name owner = do
    tid <- createTeamInternalNoActivate g name owner
    changeTeamStatus g tid Active
    return tid

createTeamInternalNoActivate :: HasCallStack => Galley -> Text -> UserId -> Http TeamId
createTeamInternalNoActivate g name owner = do
    tid <- randomId
    let nt = BindingNewTeam $ newNewTeam (unsafeRange name) (unsafeRange "icon")
    _ <- put (g . paths ["/i/teams", toByteString' tid] . zUser owner . zConn "conn" . zType "access" . json nt) <!! do
        const 201  === statusCode
        const True === isJust . getHeader "Location"
    return tid

createTeamInternalWithCurrency :: HasCallStack => Galley -> Text -> UserId -> Currency.Alpha -> Http TeamId
createTeamInternalWithCurrency g name owner cur = do
    tid <- createTeamInternalNoActivate g name owner
    _ <- put (g . paths ["i", "teams", toByteString' tid, "status"] . json (TeamStatusUpdate Active $ Just cur)) !!!
        const 200 === statusCode
    return tid

getTeam :: HasCallStack => Galley -> UserId -> TeamId -> Http Team
getTeam g usr tid = do
    r <- get (g . paths ["teams", toByteString' tid] . zUser usr) <!! const 200 === statusCode
    pure (fromJust (decodeBody r))

getTeamMembers :: HasCallStack => Galley -> UserId -> TeamId -> Http TeamMemberList
getTeamMembers g usr tid = do
    r <- get (g . paths ["teams", toByteString' tid, "members"] . zUser usr) <!! const 200 === statusCode
    pure (fromJust (decodeBody r))

getTeamMember :: HasCallStack => Galley -> UserId -> TeamId -> UserId -> Http TeamMember
getTeamMember g usr tid mid = do
    r <- get (g . paths ["teams", toByteString' tid, "members", toByteString' mid] . zUser usr) <!! const 200 === statusCode
    pure (fromJust (decodeBody r))

getTeamMemberInternal :: HasCallStack => Galley -> TeamId -> UserId -> Http TeamMember
getTeamMemberInternal g tid mid = do
    r <- get (g . paths ["i", "teams", toByteString' tid, "members", toByteString' mid]) <!! const 200 === statusCode
    pure (fromJust (decodeBody r))

addTeamMember :: HasCallStack => Galley -> UserId -> TeamId -> TeamMember -> Http ()
addTeamMember g usr tid mem = do
    let payload = json (newNewTeamMember mem)
    post (g . paths ["teams", toByteString' tid, "members"] . zUser usr . zConn "conn" .payload) !!!
        const 200 === statusCode

addTeamMemberInternal :: HasCallStack => Galley -> TeamId -> TeamMember -> Http ()
addTeamMemberInternal g tid mem = do
    let payload = json (newNewTeamMember mem)
    post (g . paths ["i", "teams", toByteString' tid, "members"] . payload) !!!
        const 200 === statusCode

createTeamConv :: HasCallStack => Galley -> UserId -> ConvTeamInfo -> [UserId] -> Maybe Text -> Maybe (Set Access) -> Maybe Milliseconds -> Http ConvId
createTeamConv g u tinfo us name acc mtimer = createTeamConvAccess g u tinfo us name acc Nothing mtimer

createTeamConvAccess :: HasCallStack => Galley -> UserId -> ConvTeamInfo -> [UserId] -> Maybe Text -> Maybe (Set Access) -> Maybe AccessRole -> Maybe Milliseconds -> Http ConvId
createTeamConvAccess g u tinfo us name acc role mtimer = do
    r <- createTeamConvAccessRaw g u tinfo us name acc role mtimer <!! const 201 === statusCode
    fromBS (getHeader' "Location" r)

createTeamConvAccessRaw :: Galley -> UserId -> ConvTeamInfo -> [UserId] -> Maybe Text -> Maybe (Set Access) -> Maybe AccessRole -> Maybe Milliseconds -> Http ResponseLBS
createTeamConvAccessRaw g u tinfo us name acc role mtimer = do
    let conv = NewConv us name (fromMaybe (Set.fromList []) acc) role (Just tinfo) mtimer
    post ( g
          . path "/conversations"
          . zUser u
          . zConn "conn"
          . zType "access"
          . json conv
          )

createManagedConv :: HasCallStack => Galley -> UserId -> ConvTeamInfo -> [UserId] -> Maybe Text -> Maybe (Set Access) -> Maybe Milliseconds -> Http ConvId
createManagedConv g u tinfo us name acc mtimer = do
    let conv = NewConv us name (fromMaybe (Set.fromList []) acc) Nothing (Just tinfo) mtimer
    r <- post ( g
              . path "i/conversations/managed"
              . zUser u
              . zConn "conn"
              . zType "access"
              . json conv
              )
         <!! const 201 === statusCode
    fromBS (getHeader' "Location" r)

createOne2OneTeamConv :: Galley -> UserId -> UserId -> Maybe Text -> TeamId -> Http ResponseLBS
createOne2OneTeamConv g u1 u2 n tid = do
    let conv = NewConv [u2] n mempty Nothing (Just $ ConvTeamInfo tid False) Nothing
    post $ g . path "/conversations/one2one" . zUser u1 . zConn "conn" . zType "access" . json conv

postConv :: Galley -> UserId -> [UserId] -> Maybe Text -> [Access] -> Maybe AccessRole -> Maybe Milliseconds -> Http ResponseLBS
postConv g u us name a r mtimer = do
    let conv = NewConv us name (Set.fromList a) r Nothing mtimer
    post $ g . path "/conversations" . zUser u . zConn "conn" . zType "access" . json conv

postSelfConv :: Galley -> UserId -> Http ResponseLBS
postSelfConv g u = post $ g . path "/conversations/self" . zUser u . zConn "conn" . zType "access"

postO2OConv :: Galley -> UserId -> UserId -> Maybe Text -> Http ResponseLBS
postO2OConv g u1 u2 n = do
    let conv = NewConv [u2] n mempty Nothing Nothing Nothing
    post $ g . path "/conversations/one2one" . zUser u1 . zConn "conn" . zType "access" . json conv

postConnectConv :: Galley -> UserId -> UserId -> Text -> Text -> Maybe Text -> Http ResponseLBS
postConnectConv g a b name msg email = post $ g
    . path "/i/conversations/connect"
    . zUser a
    . zConn "conn"
    . zType "access"
    . json (Connect b (Just msg) (Just name) email)

putConvAccept :: Galley -> UserId -> ConvId -> Http ResponseLBS
putConvAccept g invited cid = put $ g
    . paths ["/i/conversations", pack $ show cid, "accept", "v2"]
    . zUser invited
    . zType "access"
    . zConn "conn"

postOtrMessage :: (Request -> Request) -> Galley -> UserId -> ClientId -> ConvId -> [(UserId, ClientId, Text)] -> Http ResponseLBS
postOtrMessage f g u d c rec = post $ g
    . f
    . paths ["conversations", toByteString' c, "otr", "messages"]
    . zUser u . zConn "conn"
    . zType "access"
    . json (mkOtrPayload d rec)

postOtrBroadcastMessage :: (Request -> Request) -> Galley -> UserId -> ClientId -> [(UserId, ClientId, Text)] -> Http ResponseLBS
postOtrBroadcastMessage f g u d rec = post $ g
    . f
    . paths ["broadcast", "otr", "messages"]
    . zUser u . zConn "conn"
    . zType "access"
    . json (mkOtrPayload d rec)

mkOtrPayload :: ClientId -> [(UserId, ClientId, Text)] -> Value
mkOtrPayload sender rec = object
        [ "sender"     .= sender
        , "recipients" .= (HashMap.map toJSON . HashMap.fromListWith HashMap.union $ map mkOtrMessage rec)
        , "data"       .= Just ("data" :: Text)
        ]

mkOtrMessage :: (UserId, ClientId, Text) -> (Text, HashMap.HashMap Text Text)
mkOtrMessage (usr, clt, m) = (fn usr, HashMap.singleton (fn clt) m)
  where
    fn :: (FromByteString a, ToByteString a) => a -> Text
    fn = fromJust . fromByteString . toByteString'

postProtoOtrMessage :: Galley -> UserId -> ClientId -> ConvId -> OtrRecipients -> Http ResponseLBS
postProtoOtrMessage g u d c rec = let m = runPut (encodeMessage $ mkOtrProtoMessage d rec) in post $ g
    . paths ["conversations", toByteString' c, "otr", "messages"]
    . zUser u . zConn "conn"
    . zType "access"
    . contentProtobuf
    . bytes m

postProtoOtrBroadcast :: Galley -> UserId -> ClientId -> OtrRecipients -> Http ResponseLBS
postProtoOtrBroadcast g u d rec = let m = runPut (encodeMessage $ mkOtrProtoMessage d rec) in post $ g
    . paths ["broadcast", "otr", "messages"]
    . zUser u . zConn "conn"
    . zType "access"
    . contentProtobuf
    . bytes m

mkOtrProtoMessage :: ClientId -> OtrRecipients -> Proto.NewOtrMessage
mkOtrProtoMessage sender rec =
    let rcps = Proto.fromOtrRecipients rec
        sndr = Proto.fromClientId sender
    in Proto.newOtrMessage sndr rcps & Proto.newOtrMessageData ?~ "data"

getConvs :: Galley -> UserId -> Maybe (Either [ConvId] ConvId) -> Maybe Int32 -> Http ResponseLBS
getConvs g u r s = get $ g
    . path "/conversations"
    . zUser u
    . zConn "conn"
    . zType "access"
    . convRange r s

getConv :: Galley -> UserId -> ConvId -> Http ResponseLBS
getConv g u c = get $ g
    . paths ["conversations", toByteString' c]
    . zUser u
    . zConn "conn"
    . zType "access"

getConvIds :: Galley -> UserId -> Maybe (Either [ConvId] ConvId) -> Maybe Int32 -> Http ResponseLBS
getConvIds g u r s = get $ g
    . path "/conversations/ids"
    . zUser u
    . zConn "conn"
    . zType "access"
    . convRange r s

postMembers :: Galley -> UserId -> List1 UserId -> ConvId -> Http ResponseLBS
postMembers g u us c = do
    let i = Invite us
    post $ g
         . paths ["conversations", toByteString' c, "members"]
         . zUser u
         . zConn "conn"
         . zType "access"
         . json i

deleteMember :: Galley -> UserId -> UserId -> ConvId -> Http ResponseLBS
deleteMember g u1 u2 c = delete $ g
    . zUser u1
    . paths ["conversations", toByteString' c, "members", toByteString' u2]
    . zConn "conn"
    . zType "access"

getSelfMember :: Galley -> UserId -> ConvId -> Http ResponseLBS
getSelfMember g u c = get $ g
    . paths ["conversations", toByteString' c, "self"]
    . zUser u
    . zConn "conn"
    . zType "access"

putMember :: Galley -> UserId -> MemberUpdate -> ConvId -> Http ResponseLBS
putMember g u m c = put $ g
    . paths ["conversations", toByteString' c, "self"]
    . zUser u
    . zConn "conn"
    . zType "access"
    . json m

postJoinConv :: Galley -> UserId -> ConvId -> Http ResponseLBS
postJoinConv g u c = post $ g
    . paths ["/conversations", toByteString' c, "join"]
    . zUser u
    . zConn "conn"
    . zType "access"

postJoinCodeConv :: Galley -> UserId -> ConversationCode -> Http ResponseLBS
postJoinCodeConv g u j = post $ g
    . paths ["/conversations", "join"]
    . zUser u
    . zConn "conn"
    . zType "access"
    . json j

putAccessUpdate :: Galley -> UserId -> ConvId -> ConversationAccessUpdate -> Http ResponseLBS
putAccessUpdate g u c acc = put $ g
    . paths ["/conversations", toByteString' c, "access"]
    . zUser u
    . zConn "conn"
    . zType "access"
    . json acc

putMessageTimerUpdate
    :: Galley -> UserId -> ConvId -> ConversationMessageTimerUpdate -> Http ResponseLBS
putMessageTimerUpdate g u c acc = put $ g
    . paths ["/conversations", toByteString' c, "message-timer"]
    . zUser u
    . zConn "conn"
    . zType "access"
    . json acc

postConvCode :: Galley -> UserId -> ConvId -> Http ResponseLBS
postConvCode g u c = post $ g
    . paths ["/conversations", toByteString' c, "code"]
    . zUser u
    . zConn "conn"
    . zType "access"

postConvCodeCheck :: Galley -> ConversationCode -> Http ResponseLBS
postConvCodeCheck g code = post $ g
    . path "/conversations/code-check"
    . json code

getConvCode :: Galley -> UserId -> ConvId -> Http ResponseLBS
getConvCode g u c = get $ g
    . paths ["/conversations", toByteString' c, "code"]
    . zUser u
    . zConn "conn"
    . zType "access"

deleteConvCode :: Galley -> UserId -> ConvId -> Http ResponseLBS
deleteConvCode g u c = delete $ g
    . paths ["/conversations", toByteString' c, "code"]
    . zUser u
    . zConn "conn"
    . zType "access"

deleteClientInternal :: Galley -> UserId -> ClientId -> Http ResponseLBS
deleteClientInternal g u c = delete $ g
    . zUser u
    . zConn "conn"
    . paths ["i", "clients", toByteString' c]

deleteUser :: HasCallStack => Galley -> UserId -> Http ()
deleteUser g u = delete (g . path "/i/user" . zUser u) !!! const 200 === statusCode

assertConvMember :: HasCallStack => Galley -> UserId -> ConvId -> Http ()
assertConvMember g u c =
    getSelfMember g u c !!! do
        const 200      === statusCode
        const (Just u) === (fmap memId <$> decodeBody)

assertNotConvMember :: HasCallStack => Galley -> UserId -> ConvId -> Http ()
assertNotConvMember g u c =
    getSelfMember g u c !!! do
        const 200         === statusCode
        const (Just Null) === decodeBody

-------------------------------------------------------------------------------
-- Common Assertions

assertConvEquals :: (HasCallStack, MonadIO m) => Conversation -> Conversation -> m ()
assertConvEquals c1 c2 = liftIO $ do
    assertEqual "id"              (cnvId        c1) (cnvId        c2)
    assertEqual "type"            (cnvType      c1) (cnvType      c2)
    assertEqual "creator"         (cnvCreator   c1) (cnvCreator   c2)
    assertEqual "access"          (accessSet    c1) (accessSet    c2)
    assertEqual "name"            (cnvName      c1) (cnvName      c2)
    assertEqual "self member"     (selfMember   c1) (selfMember   c2)
    assertEqual "other members"   (otherMembers c1) (otherMembers c2)
  where
    accessSet    = Set.fromList . toList . cnvAccess
    selfMember   = cmSelf . cnvMembers
    otherMembers = Set.fromList . cmOthers . cnvMembers

assertConv :: HasCallStack
           => Response (Maybe Lazy.ByteString)
           -> ConvType
           -> UserId
           -> UserId
           -> [UserId]
           -> Maybe Text
           -> Maybe Milliseconds
           -> Http ConvId
assertConv r t c s us n mt = do
    cId <- fromBS $ getHeader' "Location" r
    let cnv = decodeBody r :: Maybe Conversation
    let _self = cmSelf . cnvMembers <$> cnv
    let others = cmOthers . cnvMembers <$> cnv
    liftIO $ do
        assertEqual "id" (Just cId) (cnvId <$> cnv)
        assertEqual "name" n (cnv >>= cnvName)
        assertEqual "type" (Just t) (cnvType <$> cnv)
        assertEqual "creator" (Just c) (cnvCreator <$> cnv)
        assertEqual "message_timer" (Just mt) (cnvMessageTimer <$> cnv)
        assertEqual "self" (Just s) (memId <$> _self)
        assertEqual "others" (Just $ Set.fromList us) (Set.fromList . map omId . toList <$> others)
        assertBool  "otr muted not false" (Just False == (memOtrMuted <$> _self))
        assertBool  "otr muted ref not empty" (isNothing (memOtrMutedRef =<< _self))
        assertBool  "otr archived not false" (Just False == (memOtrArchived <$> _self))
        assertBool  "otr archived ref not empty" (isNothing (memOtrArchivedRef =<< _self))
        case t of
            SelfConv    -> assertEqual "access" (Just privateAccess) (cnvAccess <$> cnv)
            ConnectConv -> assertEqual "access" (Just privateAccess) (cnvAccess <$> cnv)
            One2OneConv -> assertEqual "access" (Just privateAccess) (cnvAccess <$> cnv)
            _           -> return ()
    return cId


wsAssertOtr :: ConvId -> UserId -> ClientId -> ClientId -> Text -> Notification -> IO ()
wsAssertOtr = wsAssertOtr' "data"

wsAssertOtr' :: Text -> ConvId -> UserId -> ClientId -> ClientId -> Text -> Notification -> IO ()
wsAssertOtr' evData conv usr from to txt n = do
    let e = List1.head (WS.unpackPayload n)
    ntfTransient n @?= False
    evtConv      e @?= conv
    evtType      e @?= OtrMessageAdd
    evtFrom      e @?= usr
    evtData      e @?= Just (EdOtrMessage (OtrMessage from to txt (Just evData)))

wsAssertMemberJoin :: ConvId -> UserId -> [UserId] -> Notification -> IO ()
wsAssertMemberJoin conv usr new n = do
    let e = List1.head (WS.unpackPayload n)
    ntfTransient n @?= False
    evtConv      e @?= conv
    evtType      e @?= MemberJoin
    evtFrom      e @?= usr
    evtData      e @?= Just (EdMembers (Members new))

wsAssertConvAccessUpdate :: ConvId -> UserId -> ConversationAccessUpdate -> Notification -> IO ()
wsAssertConvAccessUpdate conv usr new n = do
    let e = List1.head (WS.unpackPayload n)
    ntfTransient n @?= False
    evtConv      e @?= conv
    evtType      e @?= ConvAccessUpdate
    evtFrom      e @?= usr
    evtData      e @?= Just (EdConvAccessUpdate new)

wsAssertConvMessageTimerUpdate :: ConvId -> UserId -> ConversationMessageTimerUpdate -> Notification -> IO ()
wsAssertConvMessageTimerUpdate conv usr new n = do
    let e = List1.head (WS.unpackPayload n)
    ntfTransient n @?= False
    evtConv      e @?= conv
    evtType      e @?= ConvMessageTimerUpdate
    evtFrom      e @?= usr
    evtData      e @?= Just (EdConvMessageTimerUpdate new)

wsAssertMemberLeave :: ConvId -> UserId -> [UserId] -> Notification -> IO ()
wsAssertMemberLeave conv usr old n = do
    let e = List1.head (WS.unpackPayload n)
    ntfTransient n      @?= False
    evtConv      e      @?= conv
    evtType      e      @?= MemberLeave
    evtFrom      e      @?= usr
    sorted (evtData e)  @?= sorted (Just (EdMembers (Members old)))
  where
    sorted (Just (EdMembers (Members m))) = Just (EdMembers (Members (sort m)))
    sorted x = x

assertNoMsg :: HasCallStack => WS.WebSocket -> (Notification -> Assertion) -> Http ()
assertNoMsg ws f = do
    x <- WS.awaitMatch (1 # Second) ws f
    liftIO $ case x of
        Left  _ -> return () -- expected
        Right _ -> assertFailure "Unexpected message"

-------------------------------------------------------------------------------
-- Helpers

decodeConvCode :: Response (Maybe Lazy.ByteString) -> ConversationCode
decodeConvCode r = fromMaybe (error "Failed to parse ConversationCode response") $
    decodeBody r

decodeConvCodeEvent :: Response (Maybe Lazy.ByteString) -> ConversationCode
decodeConvCodeEvent r = case fromMaybe (error "Failed to parse Event") $ decodeBody r of
    (Event ConvCodeUpdate _ _ _ (Just (EdConvCodeUpdate c))) -> c
    _ -> error "Failed to parse ConversationCode from Event"

decodeConvId :: Response (Maybe Lazy.ByteString) -> ConvId
decodeConvId r = fromMaybe (error "Failed to parse conversation") $
    cnvId <$> decodeBody r

decodeConvList :: Response (Maybe Lazy.ByteString) -> [Conversation]
decodeConvList = convList . decodeBody' "conversations"

decodeConvIdList :: Response (Maybe Lazy.ByteString) -> [ConvId]
decodeConvIdList = convList . decodeBody' "conversation-ids"

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
connectUsers :: Brig -> UserId -> List1 UserId -> Http ()
connectUsers b u us = void $ connectUsersWith expect2xx b u us

connectUsersUnchecked :: Brig
                      -> UserId
                      -> List1 UserId
                      -> Http (List1 (Response (Maybe Lazy.ByteString), Response (Maybe Lazy.ByteString)))
connectUsersUnchecked = connectUsersWith id

connectUsersWith :: (Request -> Request)
                 -> Brig
                 -> UserId
                 -> List1 UserId
                 -> Http (List1 (Response (Maybe Lazy.ByteString), Response (Maybe Lazy.ByteString)))
connectUsersWith fn b u us = mapM connectTo us
  where
    connectTo v = do
        r1 <- post ( b
             . zUser u
             . zConn "conn"
             . path "/connections"
             . json (ConnectionRequest v "chat" (Message "Y"))
             . fn
             )
        r2 <- put ( b
            . zUser v
            . zConn "conn"
            . paths ["connections", toByteString' u]
            . json (ConnectionUpdate Accepted)
            . fn
            )
        return (r1, r2)

-- | A copy of 'putConnection' from Brig integration tests.
putConnection :: Brig -> UserId -> UserId -> Relation -> Http ResponseLBS
putConnection b from to r = put $ b
    . paths ["/connections", toByteString' to]
    . contentJson
    . body payload
    . zUser from
    . zConn "conn"
  where
    payload = RequestBodyLBS . encode $ object [ "status" .= r ]

randomUsers :: Brig -> Int -> Http [UserId]
randomUsers b n = replicateM n (randomUser b)

randomUser :: HasCallStack => Brig -> Http UserId
randomUser b = do
    e <- liftIO randomEmail
    let p = object [ "name" .= fromEmail e, "email" .= fromEmail e, "password" .= defPassword ]
    r <- post (b . path "/i/users" . json p) <!! const 201 === statusCode
    fromBS (getHeader' "Location" r)

ephemeralUser :: HasCallStack => Brig -> Http UserId
ephemeralUser b = do
    name <- UUID.toText <$> liftIO nextRandom
    let p = object [ "name" .= name ]
    r <- post (b . path "/register" . json p) <!! const 201 === statusCode
    let user = fromMaybe (error "createEphemeralUser: failed to parse response") (decodeBody r)
    return $ Brig.Types.userId user

randomClient :: HasCallStack => Brig -> UserId -> LastPrekey -> Http ClientId
randomClient b usr lk = do
    q <- post (b . path "/clients" . zUser usr . zConn "conn" . json newClientBody)
            <!! const 201 === statusCode
    fromBS $ getHeader' "Location" q
  where
    newClientBody =
        let sig = SignalingKeys (EncKey (C.replicate 32 'a')) (MacKey (C.replicate 32 'b'))
        in (newClient PermanentClient lk sig)
            { newClientPassword = Just (PlainTextPassword defPassword)
            }

ensureDeletedState :: HasCallStack => Brig -> Bool -> UserId -> UserId -> Http ()
ensureDeletedState b check from u =
    get ( b
        . paths ["users", toByteString' u]
        . zUser from
        . zConn "conn"
        ) !!! const (Just check) === fmap profileDeleted . decodeBody

-- TODO: Refactor, as used also in brig
deleteClient :: Brig -> UserId -> ClientId -> Maybe PlainTextPassword -> Http ResponseLBS
deleteClient b u c pw = delete $ b
    . paths ["clients", toByteString' c]
    . zUser u
    . zConn "conn"
    . contentJson
    . body payload
  where
    payload = RequestBodyLBS . encode $ object
        [ "password" .= pw
        ]

-- TODO: Refactor, as used also in brig
isUserDeleted :: HasCallStack => Brig -> UserId -> Http Bool
isUserDeleted b u = do
    r <- get (b . paths ["i", "users", toByteString' u, "status"]) <!!
        const 200 === statusCode

    case responseBody r of
        Nothing -> error $ "getStatus: failed to parse response: " ++ show r
        Just  j -> do
            let st = maybeFromJSON =<< (j ^? key "status")
            let decoded = fromMaybe (error $ "getStatus: failed to decode status" ++ show j) st
            return $ decoded == Deleted
  where
    maybeFromJSON :: FromJSON a => Value -> Maybe a
    maybeFromJSON v = case fromJSON v of
        Success a -> Just a
        _         -> Nothing

isMember :: Galley -> UserId -> ConvId -> Http Bool
isMember g usr cnv = do
    res <- get $ g
               . paths ["i", "conversations", toByteString' cnv, "members", toByteString' usr]
               . expect2xx
    return $ isJust (decodeBody res :: Maybe Member)

someLastPrekeys :: [LastPrekey]
someLastPrekeys =
    [ lastPrekey "pQABARn//wKhAFggnCcZIK1pbtlJf4wRQ44h4w7/sfSgj5oWXMQaUGYAJ/sDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g=="
    , lastPrekey "pQABARn//wKhAFggwO2any+CjiGP8XFYrY67zHPvLgp+ysY5k7vci57aaLwDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g=="
    , lastPrekey "pQABARn//wKhAFggoChErA5oTI5JT769hJV+VINmU8kougGdYqGd2U7hPa8DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g=="
    , lastPrekey "pQABARn//wKhAFggPLk4BBJ8THVLGm7r0K7EJITRlJnt6bpNzM9GTNRYcCcDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g=="
    , lastPrekey "pQABARn//wKhAFggqHASsRlZ1i8dESXRXBL2OvR+0yGUtqK9vJfzol1E+osDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g=="
    , lastPrekey "pQABARn//wKhAFggx/N1YhKXSJYJQxhWgHSA4ASaJKIHDJfmEnojfnp9VQ8DoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g=="
    , lastPrekey "pQABARn//wKhAFggVL6QIpoqmtKxmB8HToiAPxfjSDEzJEUAoFKfhXou06YDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g=="
    , lastPrekey "pQABARn//wKhAFggRs74/ViOrHN+aS2RbGCwC0sJv1Sp/Q0pmRB15s9DCBMDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g=="
    , lastPrekey "pQABARn//wKhAFggtNO/hrwzt9M/1X6eK2sG6YFmA7BDqlFMEipbZOsg0vcDoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g=="
    , lastPrekey "pQABARn//wKhAFgg1rZEY6vbAnEz+Ern5kRny/uKiIrXTb/usQxGnceV2HADoQChAFgglacihnqg/YQJHkuHNFU7QD6Pb3KN4FnubaCF2EVOgRkE9g=="
    ]

randomUserWithClient :: Brig -> LastPrekey -> Http (UserId, ClientId)
randomUserWithClient b lk = do
    u <- randomUser b
    c <- randomClient b u lk
    return (u, c)

newNonce :: Http (Id ())
newNonce = randomId

decodeBody :: (HasCallStack, FromJSON a) => Response (Maybe Lazy.ByteString) -> Maybe a
decodeBody r = do
    b <- responseBody r
    case decode b of
        Nothing -> traceShow b Nothing
        Just  a -> Just a

decodeBody' :: (HasCallStack, FromJSON a) => String -> Response (Maybe Lazy.ByteString) -> a
decodeBody' s = fromMaybe (error $ "decodeBody: " ++ s) . decodeBody

fromBS :: (HasCallStack, FromByteString a, Monad m) => ByteString -> m a
fromBS = maybe (fail "fromBS: no parse") return . fromByteString

convRange :: Maybe (Either [ConvId] ConvId) -> Maybe Int32 -> Request -> Request
convRange range size =
      maybe id (queryItem "size" . pack . show) size
    . case range of
        Just (Left  l) -> queryItem "ids" (intercalate "," $ map toByteString' l)
        Just (Right c) -> queryItem "start" (toByteString' c)
        Nothing        -> id

privateAccess :: [Access]
privateAccess = [PrivateAccess]

eqMismatch :: [(UserId, Set ClientId)]
           -> [(UserId, Set ClientId)]
           -> [(UserId, Set ClientId)]
           -> Maybe ClientMismatch
           -> Bool
eqMismatch _    _    _    Nothing      = False
eqMismatch mssd rdnt dltd (Just other) =
    UserClients (Map.fromList mssd) == missingClients other   &&
    UserClients (Map.fromList rdnt) == redundantClients other &&
    UserClients (Map.fromList dltd) == deletedClients other

otrRecipients :: [(UserId, [(ClientId, Text)])] -> OtrRecipients
otrRecipients = OtrRecipients . UserClientMap . Map.fromList . map toUserClientMap
  where
    toUserClientMap (u, cs) = (u, Map.fromList cs)

encodeCiphertext :: ByteString -> Text
encodeCiphertext = decodeUtf8 . B64.encode

memberUpdate :: MemberUpdate
memberUpdate = MemberUpdate Nothing Nothing Nothing Nothing Nothing Nothing

genRandom :: (Q.Arbitrary a, MonadIO m) => m a
genRandom = liftIO . Q.generate $ Q.arbitrary

defPassword :: Text
defPassword = "secret"

randomEmail :: MonadIO m => m Email
randomEmail = do
    uid <- liftIO nextRandom
    return $ Email ("success+" <> UUID.toText uid) "simulator.amazonses.com"

selfConv :: UserId -> Id C
selfConv u = Id (toUUID u)

-- TODO: Refactor, as used also in other services
retryWhileN :: (MonadIO m) => Int -> (a -> Bool) -> m a -> m a
retryWhileN n f m = retrying (constantDelay 1000000 <> limitRetries n)
                             (const (return . f))
                             (const m)
