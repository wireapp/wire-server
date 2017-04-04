{-# LANGUAGE OverloadedStrings #-}

module API.Util where

import Bilge hiding (timeout)
import Bilge.Assert
import Brig.Types
import Control.Applicative hiding (empty)
import Control.Error
import Control.Lens ((&), (?~))
import Control.Monad hiding (mapM_)
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.ByteString.Char8 (pack, ByteString, intercalate)
import Data.ByteString.Conversion
import Data.Foldable (toList, mapM_)
import Data.Id
import Data.Int
import Data.List1 as List1
import Data.Maybe
import Data.Monoid
import Data.ProtocolBuffers (encodeMessage)
import Data.Set (Set)
import Data.Serialize (runPut)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.UUID.V4
import Galley.Types
import Gundeck.Types.Notification
import Gundeck.Types.Push
import Prelude hiding (head, mapM_)
import Test.Tasty
import Test.Tasty.HUnit

import Debug.Trace (traceShow)

import qualified Data.ByteString.Base64      as B64
import qualified Data.ByteString.Char8       as C
import qualified Data.ByteString.Lazy        as Lazy
import qualified Data.HashMap.Strict         as HashMap
import qualified Data.Map.Strict             as Map
import qualified Data.Set                    as Set
import qualified Data.UUID                   as UUID
import qualified Galley.Types.Proto          as Proto
import qualified Test.Tasty.Cannon           as WS

type Galley      = Request -> Request
type Brig        = Request -> Request
type ResponseLBS = Response (Maybe Lazy.ByteString)

test :: Manager -> String -> Http () -> TestTree
test m s h = testCase s (runHttpT m h)

-------------------------------------------------------------------------------
-- API Operations

postConv :: Galley -> UserId -> [UserId] -> Maybe Text -> [Access] -> Http ResponseLBS
postConv g u us name a = do
    let conv = NewConv us name (Set.fromList a)
    post $ g . path "/conversations" . zUser u . zConn "conn" . zType "access" . json conv

postSelfConv :: Galley -> UserId -> Http ResponseLBS
postSelfConv g u = post $ g . path "/conversations/self" . zUser u . zConn "conn" . zType "access"

postO2OConv :: Galley -> UserId -> UserId -> Maybe Text -> Http ResponseLBS
postO2OConv g u1 u2 n = do
    let conv = NewConv [u2] n mempty
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
    . json msg
  where
    msg = object
        [ "sender"     .= d
        , "recipients" .= (HashMap.map toJSON . HashMap.fromListWith HashMap.union $ map mkOtrMessage rec)
        , "data"       .= Just ("data" :: Text)
        ]

    mkOtrMessage (usr, clt, m) = (fn usr, HashMap.singleton (fn clt) m)

    fn :: (FromByteString a, ToByteString a) => a -> Text
    fn = fromJust . fromByteString . toByteString'

postProtoOtrMessage :: Galley -> UserId -> ClientId -> ConvId -> OtrRecipients -> Http ResponseLBS
postProtoOtrMessage g u d c rec = let m = runPut (encodeMessage otrMessage) in post $ g
    . paths ["conversations", toByteString' c, "otr", "messages"]
    . zUser u . zConn "conn"
    . zType "access"
    . contentProtobuf
    . bytes m
  where
    otrMessage =
        let rcps = Proto.fromOtrRecipients rec
            sndr = Proto.fromClientId d
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

deleteClient :: Galley -> UserId -> ClientId -> Http ResponseLBS
deleteClient g u c = delete $ g
    . zUser u
    . zConn "conn"
    . paths ["i", "clients", toByteString' c]

deleteUser :: Galley -> UserId -> Http ()
deleteUser g u = delete (g . path "/i/user" . zUser u) !!! const 200 === statusCode

-------------------------------------------------------------------------------
-- Common Assertions

assertConvEquals :: MonadIO m => Conversation -> Conversation -> m ()
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

assertConv :: Response (Maybe Lazy.ByteString)
           -> ConvType
           -> UserId
           -> UserId
           -> [UserId]
           -> Maybe Text
           -> Http ConvId
assertConv r t c s us n = do
    cId <- fromBS $ getHeader' "Location" r
    let cnv = decodeBody r :: Maybe Conversation
    let self = cmSelf . cnvMembers <$> cnv
    let others = cmOthers . cnvMembers <$> cnv
    liftIO $ do
        assertEqual "id" (Just cId) (cnvId <$> cnv)
        assertEqual "name" n (cnv >>= cnvName)
        assertEqual "type" (Just t) (cnvType <$> cnv)
        assertEqual "creator" (Just c) (cnvCreator <$> cnv)
        assertEqual "self" (Just s) (memId <$> self)
        assertEqual "others" (Just $ Set.fromList us) (Set.fromList . map omId . toList <$> others)
        assertBool  "otr muted not false" (Just False == (memOtrMuted <$> self))
        assertBool  "otr muted ref not empty" (isNothing (memOtrMutedRef =<< self))
        assertBool  "otr archived not false" (Just False == (memOtrArchived <$> self))
        assertBool  "otr archived ref not empty" (isNothing (memOtrArchivedRef =<< self))
        case t of
            SelfConv    -> assertEqual "access" (Just privateAccess) (cnvAccess <$> cnv)
            ConnectConv -> assertEqual "access" (Just privateAccess) (cnvAccess <$> cnv)
            One2OneConv -> assertEqual "access" (Just privateAccess) (cnvAccess <$> cnv)
            _           -> return ()
    return cId

wsAssertOtr :: ConvId -> UserId -> ClientId -> ClientId -> Text -> Notification -> IO ()
wsAssertOtr conv usr from to txt n = do
    let e = List1.head (WS.unpackPayload n)
    ntfTransient n @?= False
    evtConv      e @?= conv
    evtType      e @?= OtrMessageAdd
    evtFrom      e @?= usr
    evtData      e @?= Just (EdOtrMessage (OtrMessage from to txt (Just "data")))

wsAssertMemberJoin :: ConvId -> UserId -> [UserId] -> Notification -> IO ()
wsAssertMemberJoin conv usr new n = do
    let e = List1.head (WS.unpackPayload n)
    ntfTransient n @?= False
    evtConv      e @?= conv
    evtType      e @?= MemberJoin
    evtFrom      e @?= usr
    evtData      e @?= Just (EdMembers (Members new))

wsAssertMemberLeave :: ConvId -> UserId -> [UserId] -> Notification -> IO ()
wsAssertMemberLeave conv usr old n = do
    let e = List1.head (WS.unpackPayload n)
    ntfTransient n @?= False
    evtConv      e @?= conv
    evtType      e @?= MemberLeave
    evtFrom      e @?= usr
    evtData      e @?= Just (EdMembers (Members old))

-------------------------------------------------------------------------------
-- Helpers

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

connectUsers :: Brig -> UserId -> List1 UserId -> Http ()
connectUsers b u = mapM_ connectTo
  where
    connectTo v = do
        post ( b
             . zUser u
             . zConn "conn"
             . path "/connections"
             . json (ConnectionRequest v "chat" (Message "Y"))
             ) !!! const 201 === statusCode
        put ( b
            . zUser v
            . zConn "conn"
            . paths ["connections", toByteString' u]
            . json (ConnectionUpdate Accepted)
            ) !!! const 200 === statusCode

randomUsers :: Brig -> Int -> Http [UserId]
randomUsers b n = replicateM n (randomUser b)

randomUser :: Brig -> Http UserId
randomUser brig = do
    e <- liftIO mkEmail
    let p = object [ "name" .= fromEmail e, "email" .= fromEmail e, "password" .= ("secret" :: Text) ]
    r <- post (brig . path "/i/users" . json p) <!! const 201 === statusCode
    fromBS (getHeader' "Location" r)
  where
    mkEmail = do
        uid <- nextRandom
        return $ Email ("success+" <> UUID.toText uid) "simulator.amazonses.com"

randomClient :: Brig -> UserId -> LastPrekey -> Http ClientId
randomClient brig usr lk = do
    q <- post (brig . path "/clients" . zUser usr . zConn "conn" . json newClientBody)
            <!! const 201 === statusCode
    fromBS $ getHeader' "Location" q
  where
    newClientBody =
        let sig = SignalingKeys (EncKey (C.replicate 32 'a')) (MacKey (C.replicate 32 'b'))
        in (newClient PermanentClient lk sig)
            { newClientPassword = Just (PlainTextPassword "secret")
            }

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

decodeBody :: FromJSON a => Response (Maybe Lazy.ByteString) -> Maybe a
decodeBody r = do
    b <- responseBody r
    case decode b of
        Nothing -> traceShow b Nothing
        Just  a -> Just a

decodeBody' :: FromJSON a => String -> Response (Maybe Lazy.ByteString) -> a
decodeBody' s = fromMaybe (error $ "decodeBody: " ++ s) . decodeBody

fromBS :: (FromByteString a, Monad m) => ByteString -> m a
fromBS = maybe (fail "fromBS: no parse") return . fromByteString

convRange :: Maybe (Either [ConvId] ConvId) -> Maybe Int32 -> Request -> Request
convRange range size =
      maybe id (queryItem "size" . pack . show) size
    . case range of
        Just (Left  l) -> queryItem "ids" (intercalate "," $ map toByteString' l)
        Just (Right c) -> queryItem "start" (toByteString' c)
        Nothing        -> id

privateAccess :: List1 Access
privateAccess = singleton PrivateAccess

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

