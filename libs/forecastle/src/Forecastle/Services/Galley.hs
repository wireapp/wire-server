module Forecastle.Services.Galley where

import Imports
import Bilge hiding (timeout)
import Bilge.Assert
import Brig.Types
import Control.Lens hiding ((.=), from, to, (#))
import Control.Retry (retrying, constantDelay, limitRetries)
import Data.Aeson hiding (json)
import Data.Aeson.Lens (key)
import Data.ByteString.Conversion
import Data.Id
import Data.List1 as List1
import Data.Misc
import Data.ProtocolBuffers (encodeMessage)
import Data.Range
import Data.Serialize (runPut)
import Data.Text.Encoding (decodeUtf8)
import Data.UUID.V4
import Galley.Types
import Galley.Types.Teams hiding (EventType (..))
import Galley.Types.Teams.Intra
import Gundeck.Types.Notification
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import Test.Tasty.HUnit
import Forecastle

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8  as C
import qualified Data.ByteString.Lazy   as Lazy
import qualified Data.Currency          as Currency
import qualified Data.HashMap.Strict    as HashMap
import qualified Data.Map.Strict        as Map
import qualified Data.Set               as Set
import qualified Data.UUID              as UUID
import qualified Galley.Types.Proto     as Proto
import qualified Test.QuickCheck        as Q
import qualified Test.Tasty.Cannon      as WS

type ResponseLBS = Response (Maybe LByteString)
-------------------------------------------------------------------------------
-- API Operations

type TestConstraints ts e = (ContainsTypes e ts, HasCallStack)

symmPermissions :: [Perm] -> Permissions
symmPermissions p = let s = Set.fromList p in fromJust (newPermissions s s)

fromBS :: ((TestConstraints '[GalleyR, Manager] e), FromByteString a) => ByteString -> TestM e a
fromBS = maybe (fail "fromBS: no parse") return . fromByteString

createTeam :: (TestConstraints '[Manager, GalleyR] e) => Text -> UserId -> [TeamMember] -> TestM e TeamId
createTeam name owner mems = do
    g <- tGalley
    let mm = if null mems then Nothing else Just $ unsafeRange (take 127 mems)
    let nt = NonBindingNewTeam $ newNewTeam (unsafeRange name) (unsafeRange "icon") & newTeamMembers .~ mm
    resp <- post (g . path "/teams" . zUser owner . zConn "conn" . zType "access" . json nt) <!! do
        const 201  === statusCode
        const True === isJust . getHeader "Location"
    fromBS (getHeader' "Location" resp)

changeTeamStatus :: (TestConstraints '[GalleyR, Manager] e) => TeamId -> TeamStatus -> TestM e ()
changeTeamStatus tid s = do
      g <- tGalley
      put
        ( g . paths ["i", "teams", toByteString' tid, "status"]
        . json (TeamStatusUpdate s Nothing)
        ) !!! const 200 === statusCode

createTeamInternal :: (TestConstraints '[GalleyR, Manager] e) => Text -> UserId -> TestM e TeamId
createTeamInternal name owner = do
    tid <- createTeamInternalNoActivate name owner
    changeTeamStatus tid Active
    return tid

createTeamInternalNoActivate :: (TestConstraints '[GalleyR, Manager] e) => Text -> UserId -> TestM e TeamId
createTeamInternalNoActivate name owner = do
    g <- tGalley
    tid <- randomId
    let nt = BindingNewTeam $ newNewTeam (unsafeRange name) (unsafeRange "icon")
    _ <- put (g . paths ["/i/teams", toByteString' tid] . zUser owner . zConn "conn" . zType "access" . json nt) <!! do
        const 201  === statusCode
        const True === isJust . getHeader "Location"
    return tid

createTeamInternalWithCurrency :: (TestConstraints '[GalleyR, Manager] e) => Text -> UserId -> Currency.Alpha -> TestM e TeamId
createTeamInternalWithCurrency name owner cur = do
    g <- tGalley
    tid <- createTeamInternalNoActivate name owner
    _ <- put (g . paths ["i", "teams", toByteString' tid, "status"] . json (TeamStatusUpdate Active $ Just cur)) !!!
        const 200 === statusCode
    return tid

getTeam :: (TestConstraints '[GalleyR, Manager] e) => UserId -> TeamId -> TestM e Team
getTeam usr tid = do
    g <- tGalley
    r <- get (g . paths ["teams", toByteString' tid] . zUser usr) <!! const 200 === statusCode
    pure (fromJust (decodeBody r))

getTeamMembers :: (TestConstraints '[GalleyR, Manager] e) => UserId -> TeamId -> TestM e TeamMemberList
getTeamMembers usr tid = do
    g <- tGalley
    r <- get (g . paths ["teams", toByteString' tid, "members"] . zUser usr) <!! const 200 === statusCode
    pure (fromJust (decodeBody r))

getTeamMember :: (TestConstraints '[GalleyR, Manager] e) => UserId -> TeamId -> UserId -> TestM e TeamMember
getTeamMember usr tid mid = do
    g <- tGalley
    r <- get (g . paths ["teams", toByteString' tid, "members", toByteString' mid] . zUser usr) <!! const 200 === statusCode
    pure (fromJust (decodeBody r))

getTeamMemberInternal :: (TestConstraints '[GalleyR, Manager] e) => TeamId -> UserId -> TestM e TeamMember
getTeamMemberInternal tid mid = do
    g <- tGalley
    r <- get (g . paths ["i", "teams", toByteString' tid, "members", toByteString' mid]) <!! const 200 === statusCode
    pure (fromJust (decodeBody r))

addTeamMember :: (TestConstraints '[GalleyR, Manager] e) => UserId -> TeamId -> TeamMember -> TestM e ()
addTeamMember usr tid mem = do
    g <- tGalley
    let payload = json (newNewTeamMember mem)
    post (g . paths ["teams", toByteString' tid, "members"] . zUser usr . zConn "conn" .payload) !!!
        const 200 === statusCode

addTeamMemberInternal :: (TestConstraints '[GalleyR, Manager] e) => TeamId -> TeamMember -> TestM e ()
addTeamMemberInternal tid mem = do
    g <- tGalley
    let payload = json (newNewTeamMember mem)
    post (g . paths ["i", "teams", toByteString' tid, "members"] . payload) !!!
        const 200 === statusCode

createTeamConv :: (TestConstraints '[GalleyR, Manager] e) => UserId -> TeamId -> [UserId] -> Maybe Text -> Maybe (Set Access) -> Maybe Milliseconds -> TestM e ConvId
createTeamConv u tid us name acc mtimer = createTeamConvAccess u tid us name acc Nothing mtimer

createTeamConvAccess :: (TestConstraints '[GalleyR, Manager] e) => UserId -> TeamId -> [UserId] -> Maybe Text -> Maybe (Set Access) -> Maybe AccessRole -> Maybe Milliseconds -> TestM e ConvId
createTeamConvAccess u tid us name acc role mtimer = do
    r <- createTeamConvAccessRaw u tid us name acc role mtimer <!! const 201 === statusCode
    fromBS (getHeader' "Location" r)

createTeamConvAccessRaw
    :: (TestConstraints '[GalleyR, Manager] e)
    => UserId
    -> TeamId
    -> [UserId]
    -> Maybe Text
    -> Maybe (Set Access)
    -> Maybe AccessRole
    -> Maybe Milliseconds
    -> TestM e ResponseLBS
createTeamConvAccessRaw u tid us name acc role mtimer = do
    g <- tGalley
    let tinfo = ConvTeamInfo tid False
    let conv = NewConvUnmanaged $
               NewConv us name (fromMaybe (Set.fromList []) acc) role (Just tinfo) mtimer Nothing
    post ( g
          . path "/conversations"
          . zUser u
          . zConn "conn"
          . zType "access"
          . json conv
          )

updateTeamConv :: (TestConstraints '[GalleyR, Manager] e)
               => UserId
               -> ConvId
               -> ConversationRename
               -> TestM e ResponseLBS
updateTeamConv zusr convid upd = do
    g <- tGalley
    put ( g
         . paths ["/conversations", toByteString' convid]
         . zUser zusr
         . zConn "conn"
         . zType "access"
         . json upd
         )

-- | See Note [managed conversations]
createManagedConv :: (TestConstraints '[GalleyR, Manager] e) => UserId -> TeamId -> [UserId] -> Maybe Text -> Maybe (Set Access) -> Maybe Milliseconds -> TestM e ConvId
createManagedConv u tid us name acc mtimer = do
    g <- tGalley
    let tinfo = ConvTeamInfo tid True
    let conv = NewConvManaged $
               NewConv us name (fromMaybe (Set.fromList []) acc) Nothing (Just tinfo) mtimer Nothing
    r <- post ( g
              . path "i/conversations/managed"
              . zUser u
              . zConn "conn"
              . zType "access"
              . json conv
              )
         <!! const 201 === statusCode
    fromBS (getHeader' "Location" r)

createOne2OneTeamConv
    :: (TestConstraints '[GalleyR, Manager] e)
    => UserId
    -> UserId
    -> Maybe Text
    -> TeamId
    -> TestM e ResponseLBS
createOne2OneTeamConv u1 u2 n tid = do
    g <- tGalley
    let conv = NewConvUnmanaged $
               NewConv [u2] n mempty Nothing (Just $ ConvTeamInfo tid False) Nothing Nothing
    post $ g . path "/conversations/one2one" . zUser u1 . zConn "conn" . zType "access" . json conv

postConv :: (TestConstraints '[GalleyR, Manager] e)
         => UserId
         -> [UserId]
         -> Maybe Text
         -> [Access]
         -> Maybe AccessRole
         -> Maybe Milliseconds
         -> TestM e ResponseLBS
postConv u us name a r mtimer = do
    g <- tGalley
    let conv = NewConvUnmanaged $ NewConv us name (Set.fromList a) r Nothing mtimer Nothing
    post $ g . path "/conversations" . zUser u . zConn "conn" . zType "access" . json conv

postConvWithReceipt
    :: (TestConstraints '[GalleyR, Manager] e)
    => UserId
    -> [UserId]
    -> Maybe Text
    -> [Access]
    -> Maybe AccessRole
    -> Maybe Milliseconds
    -> ReceiptMode
    -> TestM e ResponseLBS
postConvWithReceipt u us name a r mtimer rcpt = do
    g <- tGalley
    let conv = NewConvUnmanaged $ NewConv us name (Set.fromList a) r Nothing mtimer (Just rcpt)
    post $ g . path "/conversations" . zUser u . zConn "conn" . zType "access" . json conv

postSelfConv :: (TestConstraints '[GalleyR, Manager] e) => UserId -> TestM e ResponseLBS
postSelfConv u = do
    g <- tGalley
    post $ g . path "/conversations/self" . zUser u . zConn "conn" . zType "access"

postO2OConv :: (TestConstraints '[GalleyR, Manager] e)
            => UserId
            -> UserId
            -> Maybe Text
            -> TestM e ResponseLBS
postO2OConv u1 u2 n = do
    g <- tGalley
    let conv = NewConvUnmanaged $ NewConv [u2] n mempty Nothing Nothing Nothing Nothing
    post $ g . path "/conversations/one2one" . zUser u1 . zConn "conn" . zType "access" . json conv

postConnectConv :: (TestConstraints '[GalleyR, Manager] e)
                => UserId
                -> UserId
                -> Text
                -> Text
                -> Maybe Text
                -> TestM e ResponseLBS
postConnectConv a b name msg email = do
    g <- tGalley
    post $ g
      . path "/i/conversations/connect"
      . zUser a
      . zConn "conn"
      . zType "access"
      . json (Connect b (Just msg) (Just name) email)

putConvAccept :: (TestConstraints '[GalleyR, Manager] e)
              => UserId
              -> ConvId
              -> TestM e ResponseLBS
putConvAccept invited cid = do
    g <- tGalley
    put $ g
      . paths ["/i/conversations", C.pack $ show cid, "accept", "v2"]
      . zUser invited
      . zType "access"
      . zConn "conn"

postOtrMessage :: (TestConstraints '[GalleyR, Manager] e)
               => (Request -> Request)
               -> UserId
               -> ClientId
               -> ConvId
               -> [(UserId, ClientId, Text)]
               -> TestM e ResponseLBS
postOtrMessage f u d c rec = do
    g <- tGalley
    post $ g
      . f
      . paths ["conversations", toByteString' c, "otr", "messages"]
      . zUser u . zConn "conn"
      . zType "access"
      . json (mkOtrPayload d rec)

postOtrBroadcastMessage
    :: (TestConstraints '[GalleyR, Manager] e)
    => (Request -> Request)
    -> UserId
    -> ClientId
    -> [(UserId, ClientId, Text)]
    -> TestM e ResponseLBS
postOtrBroadcastMessage f u d rec = do
    g <- tGalley
    post $ g
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

postProtoOtrMessage :: (TestConstraints '[GalleyR, Manager] e)
                    => UserId
                    -> ClientId
                    -> ConvId
                    -> OtrRecipients
                    -> TestM e ResponseLBS
postProtoOtrMessage u d c rec = do
  g <- tGalley
  let m = runPut (encodeMessage $ mkOtrProtoMessage d rec) in post $ g
    . paths ["conversations", toByteString' c, "otr", "messages"]
    . zUser u . zConn "conn"
    . zType "access"
    . contentProtobuf
    . bytes m

postProtoOtrBroadcast :: (TestConstraints '[GalleyR, Manager] e)
                      => UserId
                      -> ClientId
                      -> OtrRecipients
                      -> TestM e ResponseLBS
postProtoOtrBroadcast u d rec = do
  g <- tGalley
  let m = runPut (encodeMessage $ mkOtrProtoMessage d rec) in post $ g
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

getConvs :: (TestConstraints '[GalleyR, Manager] e)
         => UserId
         -> Maybe (Either [ConvId] ConvId)
         -> Maybe Int32
         -> TestM e ResponseLBS
getConvs u r s = do
    g <- tGalley
    get $ g
      . path "/conversations"
      . zUser u
      . zConn "conn"
      . zType "access"
      . convRange r s

getConv :: (TestConstraints '[GalleyR, Manager] e) => UserId -> ConvId -> TestM e ResponseLBS
getConv u c = do
    g <- tGalley
    get $ g
      . paths ["conversations", toByteString' c]
      . zUser u
      . zConn "conn"
      . zType "access"

getConvIds :: (TestConstraints '[GalleyR, Manager] e)
           => UserId
           -> Maybe (Either [ConvId] ConvId)
           -> Maybe Int32
           -> TestM e ResponseLBS
getConvIds u r s = do
    g <- tGalley
    get $ g
      . path "/conversations/ids"
      . zUser u
      . zConn "conn"
      . zType "access"
      . convRange r s

postMembers :: (TestConstraints '[GalleyR, Manager] e)
            => UserId
            -> List1 UserId
            -> ConvId
            -> TestM e ResponseLBS
postMembers u us c = do
    g <- tGalley
    let i = Invite us
    post $ g
         . paths ["conversations", toByteString' c, "members"]
         . zUser u
         . zConn "conn"
         . zType "access"
         . json i

deleteMember :: (TestConstraints '[GalleyR, Manager] e)
             => UserId
             -> UserId
             -> ConvId
             -> TestM e ResponseLBS
deleteMember u1 u2 c = do
    g <- tGalley
    delete $ g
      . zUser u1
      . paths ["conversations", toByteString' c, "members", toByteString' u2]
      . zConn "conn"
      . zType "access"

getSelfMember :: (TestConstraints '[GalleyR, Manager] e)
              => UserId
              -> ConvId
              -> TestM e ResponseLBS
getSelfMember u c = do
    g <- tGalley
    get $ g
      . paths ["conversations", toByteString' c, "self"]
      . zUser u
      . zConn "conn"
      . zType "access"

putMember :: (TestConstraints '[GalleyR, Manager] e)
          => UserId
          -> MemberUpdate
          -> ConvId
          -> TestM e ResponseLBS
putMember u m c = do
    g <- tGalley
    put $ g
      . paths ["conversations", toByteString' c, "self"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json m

postJoinConv :: (TestConstraints '[GalleyR, Manager] e)
             => UserId
             -> ConvId
             -> TestM e ResponseLBS
postJoinConv u c = do
    g <- tGalley
    post $ g
      . paths ["/conversations", toByteString' c, "join"]
      . zUser u
      . zConn "conn"
      . zType "access"

postJoinCodeConv :: (TestConstraints '[GalleyR, Manager] e)
                 => UserId
                 -> ConversationCode
                 -> TestM e ResponseLBS
postJoinCodeConv u j = do
    g <- tGalley
    post $ g
      . paths ["/conversations", "join"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json j

putAccessUpdate :: (TestConstraints '[GalleyR, Manager] e)
                => UserId
                -> ConvId
                -> ConversationAccessUpdate
                -> TestM e ResponseLBS
putAccessUpdate u c acc = do
    g <- tGalley
    put $ g
      . paths ["/conversations", toByteString' c, "access"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json acc

putMessageTimerUpdate :: (TestConstraints '[GalleyR, Manager] e)
                      => UserId
                      -> ConvId
                      -> ConversationMessageTimerUpdate
                      -> TestM e ResponseLBS
putMessageTimerUpdate u c acc = do
    g <- tGalley
    put $ g
      . paths ["/conversations", toByteString' c, "message-timer"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json acc

postConvCode :: (TestConstraints '[GalleyR, Manager] e)
             => UserId
             -> ConvId
             -> TestM e ResponseLBS
postConvCode u c = do
    g <- tGalley
    post $ g
      . paths ["/conversations", toByteString' c, "code"]
      . zUser u
      . zConn "conn"
      . zType "access"

postConvCodeCheck :: (TestConstraints '[GalleyR, Manager] e)
                  => ConversationCode
                  -> TestM e ResponseLBS
postConvCodeCheck code = do
    g <- tGalley
    post $ g
      . path "/conversations/code-check"
      . json code

getConvCode :: (TestConstraints '[GalleyR, Manager] e)
            => UserId
            -> ConvId
            -> TestM e ResponseLBS
getConvCode u c = do
    g <- tGalley
    get $ g
      . paths ["/conversations", toByteString' c, "code"]
      . zUser u
      . zConn "conn"
      . zType "access"

deleteConvCode :: (TestConstraints '[GalleyR, Manager] e)
               => UserId
               -> ConvId
               -> TestM e ResponseLBS
deleteConvCode u c = do
    g <- tGalley
    delete $ g
      . paths ["/conversations", toByteString' c, "code"]
      . zUser u
      . zConn "conn"
      . zType "access"

deleteClientInternal :: (TestConstraints '[GalleyR, Manager] e)
                     => UserId
                     -> ClientId
                     -> TestM e ResponseLBS
deleteClientInternal u c = do
    g <- tGalley
    delete $ g
      . zUser u
      . zConn "conn"
      . paths ["i", "clients", toByteString' c]

deleteUser :: (TestConstraints '[GalleyR, Manager] e) => UserId -> TestM e ()
deleteUser u = do
    g <- tGalley
    delete (g . path "/i/user" . zUser u) !!! const 200 === statusCode

assertConvMember :: (TestConstraints '[GalleyR, Manager] e) => UserId -> ConvId -> TestM e ()
assertConvMember u c =
    getSelfMember u c !!! do
        const 200      === statusCode
        const (Just u) === (fmap memId <$> decodeBody)

assertNotConvMember :: (TestConstraints '[GalleyR, Manager] e) => UserId -> ConvId -> TestM e ()
assertNotConvMember u c =
    getSelfMember u c !!! do
        const 200         === statusCode
        const (Just Null) === decodeBody

-------------------------------------------------------------------------------
-- Common Assertions

assertConvEquals :: ((TestConstraints '[GalleyR, Manager] e), MonadIO m) => Conversation -> Conversation -> TestM e ()
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

assertConv :: (TestConstraints '[GalleyR, Manager] e)
           => Response (Maybe Lazy.ByteString)
           -> ConvType
           -> UserId
           -> UserId
           -> [UserId]
           -> Maybe Text
           -> Maybe Milliseconds
           -> TestM e ConvId
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

assertNoMsg :: (TestConstraints '[GalleyR, Manager] e) => WS.WebSocket -> (Notification -> Assertion) -> TestM e ()
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
connectUsers :: (TestConstraints '[BrigR, Manager] e) => UserId -> List1 UserId -> TestM e ()
connectUsers u us = void $ connectUsersWith expect2xx u us

connectUsersUnchecked
    :: (TestConstraints '[BrigR, Manager] e)
    => UserId
    -> List1 UserId
    -> TestM e (List1 (Response (Maybe Lazy.ByteString), Response (Maybe Lazy.ByteString)))
connectUsersUnchecked = connectUsersWith id

connectUsersWith
    :: (TestConstraints '[BrigR, Manager] e)
    => (Request -> Request)
    -> UserId
    -> List1 UserId
    -> TestM e (List1 (Response (Maybe Lazy.ByteString), Response (Maybe Lazy.ByteString)))
connectUsersWith fn u us = mapM connectTo us
  where
    connectTo v = do
        b <- tBrig
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
putConnection :: (TestConstraints '[BrigR, Manager] e)
              => UserId
              -> UserId
              -> Relation
              -> TestM e ResponseLBS
putConnection from to r = do
    b <- tBrig
    put $ b
      . paths ["/connections", toByteString' to]
      . contentJson
      . body payload
      . zUser from
      . zConn "conn"
    where
      payload = RequestBodyLBS . encode $ object [ "status" .= r ]

randomUsers :: (TestConstraints '[GalleyR, BrigR, Manager] e) => Int -> TestM e [UserId]
randomUsers n = replicateM n randomUser

randomUser :: (TestConstraints '[GalleyR, BrigR, Manager] e) => TestM e UserId
randomUser = randomUser' True

randomUser' :: (TestConstraints '[GalleyR, BrigR, Manager] e) => Bool -> TestM e UserId
randomUser' hasPassword = do
    b <- tBrig
    e <- randomEmail
    let p = object $ [ "name" .= fromEmail e, "email" .= fromEmail e]
                  <> [ "password" .= defPassword | hasPassword]
    r <- post (b . path "/i/users" . json p) <!! const 201 === statusCode
    fromBS (getHeader' "Location" r)

ephemeralUser :: (TestConstraints '[GalleyR, BrigR, Manager] e) => TestM e UserId
ephemeralUser = do
    b <- tBrig
    name <- UUID.toText <$> liftIO nextRandom
    let p = object [ "name" .= name ]
    r <- post (b . path "/register" . json p) <!! const 201 === statusCode
    let user = fromMaybe (error "createEphemeralUser: failed to parse response") (decodeBody r)
    return $ Brig.Types.userId user

randomClient :: (TestConstraints '[GalleyR, BrigR, Manager] e) => UserId -> LastPrekey -> TestM e ClientId
randomClient usr lk = do
    b <- tBrig
    q <- post (b . path "/clients" . zUser usr . zConn "conn" . json newClientBody)
            <!! const 201 === statusCode
    fromBS $ getHeader' "Location" q
  where
    newClientBody = (newClient PermanentClient lk)
        { newClientPassword = Just (PlainTextPassword defPassword)
        }

ensureDeletedState :: (TestConstraints '[Manager, BrigR] e) => Bool -> UserId -> UserId -> TestM e ()
ensureDeletedState check from u = do
    b <- tBrig
    get ( b
        . paths ["users", toByteString' u]
        . zUser from
        . zConn "conn"
        ) !!! const (Just check) === fmap profileDeleted . decodeBody

-- TODO: Refactor, as used also in brig
deleteClient :: (TestConstraints '[Manager, BrigR] e)
             => UserId
             -> ClientId
             -> Maybe PlainTextPassword
             -> TestM e ResponseLBS
deleteClient u c pw = do
    b <- tBrig
    delete $ b
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
isUserDeleted :: (TestConstraints '[GalleyR, Manager, BrigR] e) => UserId -> TestM e Bool
isUserDeleted u = do
    b <- tBrig
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

isMember :: (TestConstraints '[GalleyR, Manager] e) => UserId -> ConvId -> TestM e Bool
isMember usr cnv = do
    g <- tGalley
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

randomUserWithClient :: (TestConstraints '[GalleyR, BrigR, Manager] e) => LastPrekey -> TestM e (UserId, ClientId)
randomUserWithClient lk = do
    u <- randomUser
    c <- randomClient u lk
    return (u, c)

newNonce :: TestM e (Id ())
newNonce = randomId

decodeBody :: (HasCallStack, FromJSON a) => Response (Maybe Lazy.ByteString) -> Maybe a
decodeBody r = do
    b <- responseBody r
    case decode b of
        Nothing -> traceShow b Nothing
        Just  a -> Just a

decodeBody' :: (FromJSON a) => String -> Response (Maybe Lazy.ByteString) -> a
decodeBody' s = fromMaybe (error $ "decodeBody: " ++ s) . decodeBody

convRange :: Maybe (Either [ConvId] ConvId) -> Maybe Int32 -> Request -> Request
convRange range size =
      maybe id (queryItem "size" . C.pack . show) size
    . case range of
        Just (Left  l) -> queryItem "ids" (C.intercalate "," $ map toByteString' l)
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
memberUpdate = MemberUpdate Nothing Nothing Nothing Nothing Nothing Nothing Nothing

genRandom :: (Q.Arbitrary a, MonadIO m) => TestM e a
genRandom = liftIO . Q.generate $ Q.arbitrary

defPassword :: Text
defPassword = "secret"

randomEmail :: TestM e Email
randomEmail = do
    uid <- liftIO nextRandom
    return $ Email ("success+" <> UUID.toText uid) "simulator.amazonses.com"

selfConv :: UserId -> Id C
selfConv u = Id (toUUID u)

-- TODO: Refactor, as used also in other services
retryWhileN :: Int -> (a -> Bool) -> TestM e a -> TestM e a
retryWhileN n f m = retrying (constantDelay 1000000 <> limitRetries n)
                             (const (return . f))
                             (const m)
