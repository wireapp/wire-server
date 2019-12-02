module API.Util where

import Imports
import Bilge hiding (timeout)
import Bilge.Assert
import Brig.Types hiding (UserIds)
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
import Data.String.Conversions (cs, ST)
import Data.Text.Encoding (decodeUtf8)
import Data.UUID.V4
import Galley.Types
import Galley.Types.Conversations.Roles (roleNameWireAdmin)
import Galley.Types.Teams hiding (EventType (..))
import Galley.Types.Teams.Intra
import Gundeck.Types.Notification
import Test.Tasty.Cannon (TimeoutUnit (..), (#))
import Test.Tasty.HUnit
import TestSetup

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


-------------------------------------------------------------------------------
-- API Operations

symmPermissions :: [Perm] -> Permissions
symmPermissions p = let s = Set.fromList p in fromJust (newPermissions s s)

-- | FUTUREWORK: this is dead code (see 'NonBindingNewTeam').  remove!
createTeam :: HasCallStack => Text -> UserId -> [TeamMember] -> TestM TeamId
createTeam name owner mems = do
    g <- view tsGalley
    let mm = if null mems then Nothing else Just $ unsafeRange (take 127 mems)
    let nt = NonBindingNewTeam $ newNewTeam (unsafeRange name) (unsafeRange "icon") & newTeamMembers .~ mm
    resp <- post (g . path "/teams" . zUser owner . zConn "conn" . zType "access" . json nt) <!! do
        const 201  === statusCode
        const True === isJust . getHeader "Location"
    fromBS (getHeader' "Location" resp)

changeTeamStatus :: HasCallStack => TeamId -> TeamStatus -> TestM ()
changeTeamStatus tid s = do
      g <- view tsGalley
      put
        ( g . paths ["i", "teams", toByteString' tid, "status"]
        . json (TeamStatusUpdate s Nothing)
        ) !!! const 200 === statusCode

createTeamInternal :: HasCallStack => Text -> UserId -> TestM TeamId
createTeamInternal name owner = do
    tid <- createTeamInternalNoActivate name owner
    changeTeamStatus tid Active
    return tid

createTeamInternalNoActivate :: HasCallStack => Text -> UserId -> TestM TeamId
createTeamInternalNoActivate name owner = do
    g <- view tsGalley
    tid <- randomId
    let nt = BindingNewTeam $ newNewTeam (unsafeRange name) (unsafeRange "icon")
    _ <- put (g . paths ["/i/teams", toByteString' tid] . zUser owner . zConn "conn" . zType "access" . json nt) <!! do
        const 201  === statusCode
        const True === isJust . getHeader "Location"
    return tid

createTeamInternalWithCurrency :: HasCallStack => Text -> UserId -> Currency.Alpha -> TestM TeamId
createTeamInternalWithCurrency name owner cur = do
    g <- view tsGalley
    tid <- createTeamInternalNoActivate name owner
    _ <- put (g . paths ["i", "teams", toByteString' tid, "status"] . json (TeamStatusUpdate Active $ Just cur)) !!!
        const 200 === statusCode
    return tid

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
    post (g . paths ["teams", toByteString' tid, "members"] . zUser usr . zConn "conn" .payload) !!!
        const 200 === statusCode

addTeamMemberInternal :: HasCallStack => TeamId -> TeamMember -> TestM ()
addTeamMemberInternal tid mem = do
    g <- view tsGalley
    let payload = json (newNewTeamMember mem)
    post (g . paths ["i", "teams", toByteString' tid, "members"] . payload) !!!
        const 200 === statusCode

createTeamConv :: HasCallStack => UserId -> TeamId -> [UserId] -> Maybe Text -> Maybe (Set Access) -> Maybe Milliseconds -> TestM ConvId
createTeamConv u tid us name acc mtimer = createTeamConvAccess u tid us name acc Nothing mtimer

createTeamConvAccess :: HasCallStack => UserId -> TeamId -> [UserId] -> Maybe Text -> Maybe (Set Access) -> Maybe AccessRole -> Maybe Milliseconds -> TestM ConvId
createTeamConvAccess u tid us name acc role mtimer = do
    r <- createTeamConvAccessRaw u tid us name acc role mtimer <!! const 201 === statusCode
    fromBS (getHeader' "Location" r)

createTeamConvAccessRaw :: UserId -> TeamId -> [UserId] -> Maybe Text -> Maybe (Set Access) -> Maybe AccessRole -> Maybe Milliseconds -> TestM ResponseLBS
createTeamConvAccessRaw u tid us name acc role mtimer = do
    g <- view tsGalley
    let tinfo = ConvTeamInfo tid False
    let conv = NewConvUnmanaged $
               NewConv us name (fromMaybe (Set.fromList []) acc) role (Just tinfo) mtimer Nothing roleNameWireAdmin
    post ( g
          . path "/conversations"
          . zUser u
          . zConn "conn"
          . zType "access"
          . json conv
          )

updateTeamConv :: UserId -> ConvId -> ConversationRename -> TestM ResponseLBS
updateTeamConv zusr convid upd = do
    g <- view tsGalley
    put ( g
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
    let conv = NewConvManaged $
               NewConv us name (fromMaybe (Set.fromList []) acc) Nothing (Just tinfo) mtimer Nothing roleNameWireAdmin
    r <- post ( g
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
    let conv = NewConvUnmanaged $
               NewConv [u2] n mempty Nothing (Just $ ConvTeamInfo tid False) Nothing Nothing roleNameWireAdmin
    post $ g . path "/conversations/one2one" . zUser u1 . zConn "conn" . zType "access" . json conv

postConv :: UserId -> [UserId] -> Maybe Text -> [Access] -> Maybe AccessRole -> Maybe Milliseconds -> TestM ResponseLBS
postConv u us name a r mtimer = do
    g <- view tsGalley
    let conv = NewConvUnmanaged $ NewConv us name (Set.fromList a) r Nothing mtimer Nothing roleNameWireAdmin
    post $ g . path "/conversations" . zUser u . zConn "conn" . zType "access" . json conv

postConvWithReceipt :: UserId -> [UserId] -> Maybe Text -> [Access] -> Maybe AccessRole -> Maybe Milliseconds -> ReceiptMode -> TestM ResponseLBS
postConvWithReceipt u us name a r mtimer rcpt = do
    g <- view tsGalley
    let conv = NewConvUnmanaged $ NewConv us name (Set.fromList a) r Nothing mtimer (Just rcpt) roleNameWireAdmin
    post $ g . path "/conversations" . zUser u . zConn "conn" . zType "access" . json conv

postSelfConv :: UserId -> TestM ResponseLBS
postSelfConv u = do
    g <- view tsGalley
    post $ g . path "/conversations/self" . zUser u . zConn "conn" . zType "access"

postO2OConv :: UserId -> UserId -> Maybe Text -> TestM ResponseLBS
postO2OConv u1 u2 n = do
    g <- view tsGalley
    let conv = NewConvUnmanaged $ NewConv [u2] n mempty Nothing Nothing Nothing Nothing roleNameWireAdmin
    post $ g . path "/conversations/one2one" . zUser u1 . zConn "conn" . zType "access" . json conv

postConnectConv :: UserId -> UserId -> Text -> Text -> Maybe Text -> TestM ResponseLBS
postConnectConv a b name msg email = do
    g <- view tsGalley
    post $ g
      . path "/i/conversations/connect"
      . zUser a
      . zConn "conn"
      . zType "access"
      . json (Connect b (Just msg) (Just name) email)

putConvAccept :: UserId -> ConvId -> TestM ResponseLBS
putConvAccept invited cid = do
    g <- view tsGalley
    put $ g
      . paths ["/i/conversations", C.pack $ show cid, "accept", "v2"]
      . zUser invited
      . zType "access"
      . zConn "conn"

postOtrMessage :: (Request -> Request)
               -> UserId
               -> ClientId
               -> ConvId
               -> [(UserId, ClientId, Text)]
               -> TestM ResponseLBS
postOtrMessage f u d c rec = do
    g <- view tsGalley
    post $ g
      . f
      . paths ["conversations", toByteString' c, "otr", "messages"]
      . zUser u . zConn "conn"
      . zType "access"
      . json (mkOtrPayload d rec)

postOtrBroadcastMessage :: (Request -> Request) -> UserId -> ClientId -> [(UserId, ClientId, Text)] -> TestM ResponseLBS
postOtrBroadcastMessage f u d rec = do
    g <- view tsGalley
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

postProtoOtrMessage :: UserId -> ClientId -> ConvId -> OtrRecipients -> TestM ResponseLBS
postProtoOtrMessage u d c rec = do
  g <- view tsGalley
  let m = runPut (encodeMessage $ mkOtrProtoMessage d rec) in post $ g
    . paths ["conversations", toByteString' c, "otr", "messages"]
    . zUser u . zConn "conn"
    . zType "access"
    . contentProtobuf
    . bytes m

postProtoOtrBroadcast :: UserId -> ClientId -> OtrRecipients -> TestM ResponseLBS
postProtoOtrBroadcast u d rec = do
  g <- view tsGalley
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

getConvs :: UserId -> Maybe (Either [ConvId] ConvId) -> Maybe Int32 -> TestM ResponseLBS
getConvs u r s = do
    g <- view tsGalley
    get $ g
      . path "/conversations"
      . zUser u
      . zConn "conn"
      . zType "access"
      . convRange r s

getConv :: UserId -> ConvId -> TestM ResponseLBS
getConv u c = do
    g <- view tsGalley
    get $ g
      . paths ["conversations", toByteString' c]
      . zUser u
      . zConn "conn"
      . zType "access"

getConvIds :: UserId -> Maybe (Either [ConvId] ConvId) -> Maybe Int32 -> TestM ResponseLBS
getConvIds u r s = do
    g <- view tsGalley
    get $ g
      . path "/conversations/ids"
      . zUser u
      . zConn "conn"
      . zType "access"
      . convRange r s

postMembers :: UserId -> List1 UserId -> ConvId -> TestM ResponseLBS
postMembers u us c = do
    g <- view tsGalley
    let i = Invite us
    post $ g
         . paths ["conversations", toByteString' c, "members"]
         . zUser u
         . zConn "conn"
         . zType "access"
         . json i

deleteMember :: UserId -> UserId -> ConvId -> TestM ResponseLBS
deleteMember u1 u2 c = do
    g <- view tsGalley
    delete $ g
      . zUser u1
      . paths ["conversations", toByteString' c, "members", toByteString' u2]
      . zConn "conn"
      . zType "access"

getSelfMember :: UserId -> ConvId -> TestM ResponseLBS
getSelfMember u c = do
    g <- view tsGalley
    get $ g
      . paths ["conversations", toByteString' c, "self"]
      . zUser u
      . zConn "conn"
      . zType "access"

putMember :: UserId -> MemberUpdate -> ConvId -> TestM ResponseLBS
putMember u m c = do
    g <- view tsGalley
    put $ g
      . paths ["conversations", toByteString' c, "self"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json m

postJoinConv :: UserId -> ConvId -> TestM ResponseLBS
postJoinConv u c = do
    g <- view tsGalley
    post $ g
      . paths ["/conversations", toByteString' c, "join"]
      . zUser u
      . zConn "conn"
      . zType "access"

postJoinCodeConv :: UserId -> ConversationCode -> TestM ResponseLBS
postJoinCodeConv u j = do
    g <- view tsGalley
    post $ g
      . paths ["/conversations", "join"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json j

putAccessUpdate :: UserId -> ConvId -> ConversationAccessUpdate -> TestM ResponseLBS
putAccessUpdate u c acc = do
    g <- view tsGalley
    put $ g
      . paths ["/conversations", toByteString' c, "access"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json acc

putMessageTimerUpdate
    :: UserId -> ConvId -> ConversationMessageTimerUpdate -> TestM ResponseLBS
putMessageTimerUpdate u c acc = do
    g <- view tsGalley
    put $ g
      . paths ["/conversations", toByteString' c, "message-timer"]
      . zUser u
      . zConn "conn"
      . zType "access"
      . json acc

postConvCode :: UserId -> ConvId -> TestM ResponseLBS
postConvCode u c = do
    g <- view tsGalley
    post $ g
      . paths ["/conversations", toByteString' c, "code"]
      . zUser u
      . zConn "conn"
      . zType "access"

postConvCodeCheck :: ConversationCode -> TestM ResponseLBS
postConvCodeCheck code = do
    g <- view tsGalley
    post $ g
      . path "/conversations/code-check"
      . json code

getConvCode :: UserId -> ConvId -> TestM ResponseLBS
getConvCode u c = do
    g <- view tsGalley
    get $ g
      . paths ["/conversations", toByteString' c, "code"]
      . zUser u
      . zConn "conn"
      . zType "access"

deleteConvCode :: UserId -> ConvId -> TestM ResponseLBS
deleteConvCode u c = do
    g <- view tsGalley
    delete $ g
      . paths ["/conversations", toByteString' c, "code"]
      . zUser u
      . zConn "conn"
      . zType "access"

deleteClientInternal :: UserId -> ClientId -> TestM ResponseLBS
deleteClientInternal u c = do
    g <- view tsGalley
    delete $ g
      . zUser u
      . zConn "conn"
      . paths ["i", "clients", toByteString' c]

deleteUser :: HasCallStack => UserId -> TestM ()
deleteUser u = do
    g <- view tsGalley
    delete (g . path "/i/user" . zUser u) !!! const 200 === statusCode

assertConvMember :: HasCallStack => UserId -> ConvId -> TestM ()
assertConvMember u c =
    getSelfMember u c !!! do
        const 200      === statusCode
        const (Right u) === (fmap memId <$> responseJsonEither)

assertNotConvMember :: HasCallStack => UserId -> ConvId -> TestM ()
assertNotConvMember u c =
    getSelfMember u c !!! do
        const 200          === statusCode
        const (Right Null) === responseJsonEither

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
           -> TestM ConvId
assertConv r t c s us n mt = do
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
    evtData      e @?= Just (EdMembersJoin $ SimpleMembers (fmap (\x -> SimpleMember x Nothing roleNameWireAdmin) new))

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
    sorted (evtData e)  @?= sorted (Just (EdMembersLeave (UserIds old)))
  where
    sorted (Just (EdMembersLeave (UserIds m))) = Just (EdMembersLeave (UserIds (sort m)))
    sorted x = x

assertNoMsg :: HasCallStack => WS.WebSocket -> (Notification -> Assertion) -> TestM ()
assertNoMsg ws f = do
    x <- WS.awaitMatch (1 # Second) ws f
    liftIO $ case x of
        Left  _ -> return () -- expected
        Right _ -> assertFailure "Unexpected message"

-------------------------------------------------------------------------------
-- Helpers

testResponse :: HasCallStack => Int -> Maybe TestErrorLabel -> Assertions ()
testResponse status mlabel = do
    const status === statusCode
    case mlabel of
        Just label -> responseJsonEither === const (Right label)
        Nothing    -> (isLeft <$> responseJsonEither @TestErrorLabel) === const True

newtype TestErrorLabel = TestErrorLabel { fromTestErrorLabel :: ST }
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

connectUsersUnchecked :: UserId
                      -> List1 UserId
                      -> TestM (List1 (Response (Maybe Lazy.ByteString), Response (Maybe Lazy.ByteString)))
connectUsersUnchecked = connectUsersWith id

connectUsersWith :: (Request -> Request)
                 -> UserId
                 -> List1 UserId
                 -> TestM (List1 (Response (Maybe Lazy.ByteString), Response (Maybe Lazy.ByteString)))
connectUsersWith fn u us = mapM connectTo us
  where
    connectTo v = do
        b <- view tsBrig
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
putConnection :: UserId -> UserId -> Relation -> TestM ResponseLBS
putConnection from to r = do
    b <- view tsBrig
    put $ b
      . paths ["/connections", toByteString' to]
      . contentJson
      . body payload
      . zUser from
      . zConn "conn"
    where
      payload = RequestBodyLBS . encode $ object [ "status" .= r ]

randomUsers :: Int -> TestM [UserId]
randomUsers n = replicateM n randomUser

randomUser :: HasCallStack => TestM UserId
randomUser = randomUser' True

randomUser' :: HasCallStack => Bool -> TestM UserId
randomUser' hasPassword = do
    b <- view tsBrig
    e <- liftIO randomEmail
    let p = object $ [ "name" .= fromEmail e, "email" .= fromEmail e]
                  <> [ "password" .= defPassword | hasPassword]
    r <- post (b . path "/i/users" . json p) <!! const 201 === statusCode
    fromBS (getHeader' "Location" r)

ephemeralUser :: HasCallStack => TestM UserId
ephemeralUser = do
    b <- view tsBrig
    name <- UUID.toText <$> liftIO nextRandom
    let p = object [ "name" .= name ]
    r <- post (b . path "/register" . json p) <!! const 201 === statusCode
    user <- responseJsonError r
    return $ Brig.Types.userId user

randomClient :: HasCallStack => UserId -> LastPrekey -> TestM ClientId
randomClient uid lk = do
    b <- view tsBrig
    resp <- post (b . paths ["i", "clients", toByteString' uid] . json newClientBody)
            <!! const rStatus === statusCode
    client <- responseJsonError resp
    return (clientId client)
  where
    cType = PermanentClientType
    rStatus = 201
    newClientBody = (newClient cType lk)
        { newClientPassword = Just defPassword
        }

ensureDeletedState :: HasCallStack => Bool -> UserId -> UserId -> TestM ()
ensureDeletedState check from u = do
    b <- view tsBrig
    get ( b
        . paths ["users", toByteString' u]
        . zUser from
        . zConn "conn"
        ) !!! const (Just check) === fmap profileDeleted . responseJsonMaybe

getClients :: UserId -> TestM ResponseLBS
getClients u = do
    b <- view tsBrig
    get $ b
      . paths ["clients"]
      . zUser u
      . zConn "conn"

-- TODO: Refactor, as used also in brig
deleteClient :: UserId -> ClientId -> Maybe PlainTextPassword -> TestM ResponseLBS
deleteClient u c pw = do
    b <- view tsBrig
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
isUserDeleted :: HasCallStack => UserId -> TestM Bool
isUserDeleted u = do
    b <- view tsBrig
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

isMember :: UserId -> ConvId -> TestM Bool
isMember usr cnv = do
    g <- view tsGalley
    res <- get $ g
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
    toUserClientMap (u, css) = (u, Map.fromList css)

encodeCiphertext :: ByteString -> Text
encodeCiphertext = decodeUtf8 . B64.encode

memberUpdate :: MemberUpdate
memberUpdate = MemberUpdate Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

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
retryWhileN n f m = retrying (constantDelay 1000000 <> limitRetries n)
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
    [ Prekey (PrekeyId  1) "pQABAQECoQBYIOjl7hw0D8YRNqkkBQETCxyr7/ywE/2R5RWcUPM+GJACA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId  2) "pQABAQICoQBYIGoXawUQWQ9ZW+MXhvuo9ALOBUjLff8S5VdAokN29C1OA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId  3) "pQABAQMCoQBYIEjdt+YWd3lHmG8pamULLMubAMZw556IO8kW7s1MLFytA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId  4) "pQABAQQCoQBYIPIaOA3Xqfk4Lh2/pU88Owd2eW5eplHpywr+Mx4QGyiMA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId  5) "pQABAQUCoQBYIHnafNR4Gh3ID71lYzToewEVag4EKskDFq+gaeraOlSJA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId  6) "pQABAQYCoQBYIFXUkVftE7kK22waAzhOjOmJVex3EBTU8RHZFx2o1Ed8A6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId  7) "pQABAQcCoQBYIDXdN8VlKb5lbgPmoDPLPyqNIEyShG4oT/DlW0peRRZUA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId  8) "pQABAQgCoQBYIJH1ewvIVV3yGqQvdr/QM9HARzMgo5ksOTRyKEuN2aZzA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId  9) "pQABAQkCoQBYIFcAnXdx0M1Q1hoDDfgMK9r+Zchn8YlVHHaQwQYhRk1dA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId 10) "pQABAQoCoQBYIGs3vyxwmzEZ+qKNy4wpFkxc+Bgkb0D76ZEbxeeh/9DVA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId 11) "pQABAQsCoQBYIGUiBeOJALP5dkMduUZ/u6MDhHNrsrBUa3f0YlSSWZbzA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId 12) "pQABAQwCoQBYIMp6QNNTPDZgL3DSSD/QWWnBI7LsTZp2RhY/HLqnIwRZA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId 13) "pQABAQ0CoQBYIJXSSUrE5RCNyB5pg+m6vGwK7RvJ+rs9dsdHitxnfDhuA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId 14) "pQABAQ4CoQBYIHmtOX7jCKBHFDysb4H0z/QWoCSaEyjerZaT/HOP8bgDA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId 15) "pQABAQ8CoQBYIIaMCTcPKj2HuYQ7i9ZaxUw9j5Bz8TPjoAaTZ5eB0w1kA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId 16) "pQABARACoQBYIHWAOacKuWH81moJVveJ0FSfipWocfspOIBhaU6VLWUsA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId 17) "pQABARECoQBYIA8XtUXtnMxQslULnNAeHBIivlLRe/+qdh2j6nTfDAchA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId 18) "pQABARICoQBYIGgzg6SzgTTOgnk48pa6y2Rgjy004DkeBo4CMld3Jlr6A6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId 19) "pQABARMCoQBYIEoEFiIpCHgn74CAD+GhIfIgbQtdCqQqkOXHWxRlG6Y6A6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId 20) "pQABARQCoQBYINVEwTRxNSe0rxZxon4Rifz2l4rtQZn7mHtKYCiFAK9IA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId 21) "pQABARUCoQBYIN3aeX2Ayi2rPFbiaYb+O2rdHUpFhzRs2j28pCmbGpflA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId 22) "pQABARYCoQBYIJe5OJ17YKQrNmIH3sE++r++4Z5ld36axqAMjjQ3jtQWA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId 23) "pQABARcCoQBYIASE94LjK6Raipk/lN/YewouqO+kcQGpxIqP+iW2hyHiA6EAoQBYILLf1TIwSB62q69Ojs/X1tzJ+dYHNAw4QbW/7TC5vSZqBPY="
    , Prekey (PrekeyId 24) "pQABARgYAqEAWCBZ222LpS6/99Btlw+83PihrA655skwsNevt//8oz5axQOhAKEAWCCy39UyMEgetquvTo7P19bcyfnWBzQMOEG1v+0wub0magT2"
    , Prekey (PrekeyId 25) "pQABARgZAqEAWCDGEwo61w4O8T8lyw0HdoOjGWBKQUNqo6+jSfrPR9alrAOhAKEAWCCy39UyMEgetquvTo7P19bcyfnWBzQMOEG1v+0wub0magT2"
    , Prekey (PrekeyId 26) "pQABARgaAqEAWCBMSQoQ6B35plC80i1O3AWlJSftCEbCbju97Iykg5+NWQOhAKEAWCCy39UyMEgetquvTo7P19bcyfnWBzQMOEG1v+0wub0magT2"
    ]

-- | Changing these will break tests; all prekeys and client Id must match the same
-- fingerprint
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
