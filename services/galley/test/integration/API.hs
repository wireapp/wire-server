{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module API (tests) where

import API.Util
import Bilge hiding (timeout)
import Bilge.Assert
import Brig.Types
import Control.Applicative hiding (empty)
import Control.Error
import Control.Lens ((^.))
import Control.Monad hiding (mapM_)
import Control.Monad.IO.Class
import Data.Aeson hiding (json)
import Data.ByteString.Conversion
import Data.Foldable (mapM_)
import Data.Id
import Data.Int
import Data.List ((\\), find)
import Data.List1
import Data.Misc
import Data.Maybe
import Data.Monoid
import Galley.Types
import Gundeck.Types.Notification
import Network.Wai.Utilities.Error
import Prelude hiding (head, mapM_)
import Test.Tasty
import Test.Tasty.Cannon (Cannon, TimeoutUnit (..), (#))
import Test.Tasty.HUnit
import API.SQS

import qualified Galley.Types.Teams       as Teams
import qualified API.Teams                as Teams
import qualified Control.Concurrent.Async as Async
import qualified Data.List1               as List1
import qualified Data.Map.Strict          as Map
import qualified Data.Set                 as Set
import qualified Data.Text                as T
import qualified Test.Tasty.Cannon        as WS

type TestSignature a = Galley -> Brig -> Cannon -> TestSetup -> Http a

test :: IO TestSetup -> TestName -> (TestSignature a) -> TestTree
test s n t = testCase n runTest
  where
    runTest = do
        setup <- s
        (void $ runHttpT (manager setup) (t (galley setup) (brig setup) (cannon setup) setup))

tests :: IO TestSetup -> TestTree
tests s = testGroup "Galley integration tests" [ mainTests, Teams.tests s ]
  where
    mainTests = testGroup "Main API"
        [ test s "status" status
        , test s "monitoring" monitor
        , test s "create conversation" postConvOk
        , test s "get empty conversations" getConvsOk
        , test s "get conversations by ids" getConvsOk2
        , test s "fail to get >100 conversations" getConvsFailMaxSize
        , test s "get conversation ids" getConvIdsOk
        , test s "paginate through conversation ids" paginateConvIds
        , test s "fail to get >1000 conversation ids" getConvIdsFailMaxSize
        , test s "page through conversations" getConvsPagingOk
        , test s "fail to create conversation when not connected" postConvFailNotConnected
        , test s "M:N conversation creation must have <N members" postConvFailNumMembers
        , test s "create self conversation" postSelfConvOk
        , test s "create 1:1 conversation" postO2OConvOk
        , test s "fail to create 1:1 conversation with yourself" postConvO2OFailWithSelf
        , test s "create connect conversation" postConnectConvOk
        , test s "create connect conversation with email" postConnectConvOk2
        , test s "upgrade connect/invite conversation" putConvAcceptOk
        , test s "upgrade conversation retries" putConvAcceptRetry
        , test s "create mutual connect conversation" postMutualConnectConvOk
        , test s "repeat / cancel connect requests" postRepeatConnectConvCancel
        , test s "block/unblock a connect/1-1 conversation" putBlockConvOk
        , test s "get conversation" getConvOk
        , test s "conversation meta access" accessConvMeta
        , test s "add members" postMembersOk
        , test s "add existing members" postMembersOk2
        , test s "add past members" postMembersOk3
        , test s "fail to add members when not connected" postMembersFail
        , test s "fail to add too many members" postTooManyMembersFail
        , test s "remove members" deleteMembersOk
        , test s "fail to remove members from self conv." deleteMembersFailSelf
        , test s "fail to remove members from 1:1 conv." deleteMembersFailO2O
        , test s "rename conversation" putConvRenameOk
        , test s "member update (otr mute)" putMemberOtrMuteOk
        , test s "member update (otr archive)" putMemberOtrArchiveOk
        , test s "member update (hidden)" putMemberHiddenOk
        , test s "member update (everything b)" putMemberAllOk
        , test s "send typing indicators" postTypingIndicators
        , test s "leave connect conversation" leaveConnectConversation
        , test s "post cryptomessage 1" postCryptoMessage1
        , test s "post cryptomessage 2" postCryptoMessage2
        , test s "post cryptomessage 3" postCryptoMessage3
        , test s "post cryptomessage 4" postCryptoMessage4
        , test s "post cryptomessage 5" postCryptoMessage5
        , test s "join conversation" postJoinConvOk
        , test s "join code-access conversation" postJoinCodeConvOk
        , test s "convert invite to code-access conversation" postConvertCodeConv
        , test s "convert code to team-access conversation" postConvertTeamConv
        , test s "cannot join private conversation" postJoinConvFail
        , test s "remove user" removeUser
        ]

-------------------------------------------------------------------------------
-- API Tests

status :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
status g _ _ _ = get (g . path "/i/status") !!!
    const 200 === statusCode

monitor :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
monitor g _ _ _ =
    get (g . path "/i/monitoring") !!! do
        const 200 === statusCode
        const (Just "application/json") =~= getHeader "Content-Type"

postConvOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postConvOk g b c _ = do
    alice <- randomUser b
    bob   <- randomUser b
    jane  <- randomUser b
    connectUsers b alice (list1 bob [jane])
    WS.bracketR3 c alice bob jane $ \(wsA, wsB, wsJ) -> do
        rsp <- postConv g alice [bob, jane] (Just "gossip") [] <!!
            const 201 === statusCode
        cid <- assertConv rsp RegularConv alice alice [bob, jane] (Just "gossip")
        cvs <- mapM (convView cid) [alice, bob, jane]
        liftIO $ mapM_ WS.assertSuccess =<< Async.mapConcurrently (checkWs alice) (zip cvs [wsA, wsB, wsJ])
  where
    convView cnv usr = decodeBody' "conversation" <$> getConv g usr cnv
    checkWs alice (cnv, ws) = WS.awaitMatch (5 # Second) ws $ \n -> do
        ntfTransient n @?= False
        let e = List1.head (WS.unpackPayload n)
        evtConv e @?= cnvId cnv
        evtType e @?= ConvCreate
        evtFrom e @?= alice
        case evtData e of
            Just (EdConversation c') -> assertConvEquals cnv c'
            _                        -> assertFailure "Unexpected event data"

postCryptoMessage1 :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postCryptoMessage1 g b c _ = do
    (alice, ac) <- randomUserWithClient b (someLastPrekeys !! 0)
    (bob,   bc) <- randomUserWithClient b (someLastPrekeys !! 1)
    (eve,   ec) <- randomUserWithClient b (someLastPrekeys !! 2)
    connectUsers b alice (list1 bob [eve])
    conv <- decodeConvId <$> postConv g alice [bob, eve] (Just "gossip") []

    -- WS receive timeout
    let t = 5 # Second

    -- Missing eve
    let m1 = [(bob, bc, "ciphertext1")]
    postOtrMessage id g alice ac conv m1 !!! do
        const 412 === statusCode
        assertTrue_ (eqMismatch [(eve, Set.singleton ec)] [] [] . decodeBody)

    -- Complete
    WS.bracketR2 c bob eve $ \(wsB, wsE) -> do
        let m2 = [(bob, bc, "ciphertext2"), (eve, ec, "ciphertext2")]
        postOtrMessage id g alice ac conv m2 !!! do
            const 201 === statusCode
            assertTrue_ (eqMismatch [] [] [] . decodeBody)
        void . liftIO $ WS.assertMatch t wsB (wsAssertOtr conv alice ac bc "ciphertext2")
        void . liftIO $ WS.assertMatch t wsE (wsAssertOtr conv alice ac ec "ciphertext2")

    -- Redundant self
    WS.bracketR3 c alice bob eve $ \(wsA, wsB, wsE) -> do
        let m3 = [(alice, ac, "ciphertext3"), (bob, bc, "ciphertext3"), (eve, ec, "ciphertext3")]
        postOtrMessage id g alice ac conv m3 !!! do
            const 201 === statusCode
            assertTrue_ (eqMismatch [] [(alice, Set.singleton ac)] [] . decodeBody)
        void . liftIO $ WS.assertMatch t wsB (wsAssertOtr conv alice ac bc "ciphertext3")
        void . liftIO $ WS.assertMatch t wsE (wsAssertOtr conv alice ac ec "ciphertext3")
        -- Alice should not get it
        assertNoMsg wsA (wsAssertOtr conv alice ac ac "ciphertext3")

    -- Deleted eve
    WS.bracketR2 c bob eve $ \(wsB, wsE) -> do
        deleteClient b eve ec (Just $ PlainTextPassword defPassword) !!! const 200 === statusCode
        let m4 = [(bob, bc, "ciphertext4"), (eve, ec, "ciphertext4")]
        postOtrMessage id g alice ac conv m4 !!! do
            const 201 === statusCode
            assertTrue_ (eqMismatch [] [] [(eve, Set.singleton ec)] . decodeBody)
        void . liftIO $ WS.assertMatch t wsB (wsAssertOtr conv alice ac bc "ciphertext4")
        -- Eve should not get it
        assertNoMsg wsE (wsAssertOtr conv alice ac ec "ciphertext4")

    -- Deleted eve & redundant self
    WS.bracketR3 c alice bob eve $ \(wsA, wsB, wsE) -> do
        let m5 = [(bob, bc, "ciphertext5"), (eve, ec, "ciphertext5"), (alice, ac, "ciphertext5")]
        postOtrMessage id g alice ac conv m5 !!! do
            const 201 === statusCode
            assertTrue_ (eqMismatch [] [(alice, Set.singleton ac)] [(eve, Set.singleton ec)] . decodeBody)
        void . liftIO $ WS.assertMatch t wsB (wsAssertOtr conv alice ac bc "ciphertext5")
        -- Neither Alice nor Eve should get it
        assertNoMsg wsA (wsAssertOtr conv alice ac ac "ciphertext5")
        assertNoMsg wsE (wsAssertOtr conv alice ac ec "ciphertext5")

    -- Missing Bob, deleted eve & redundant self
    let m6 = [(eve, ec, "ciphertext6"), (alice, ac, "ciphertext6")]
    postOtrMessage id g alice ac conv m6 !!! do
        const 412 === statusCode
        assertTrue_ (eqMismatch [(bob, Set.singleton bc)]
                                [(alice, Set.singleton ac)]
                                [(eve, Set.singleton ec)] . decodeBody)

    -- A second client for Bob
    bc2 <- randomClient b bob (someLastPrekeys !! 3)
    -- The first client listens for all messages of Bob
    WS.bracketR c bob $ \wsB -> do
        let cipher = "ciphertext7"
        -- The second client listens only for his own messages
        WS.bracketR (c . queryItem "client" (toByteString' bc2)) bob $ \wsB2 -> do
            let m7 = [(bob, bc, cipher), (bob, bc2, cipher)]
            postOtrMessage id g alice ac conv m7 !!! do
                const 201 === statusCode
                assertTrue_ (eqMismatch [] [] [] . decodeBody)
            -- Bob's first client gets both messages
            void . liftIO $ WS.assertMatch t wsB (wsAssertOtr conv alice ac bc cipher)
            void . liftIO $ WS.assertMatch t wsB (wsAssertOtr conv alice ac bc2 cipher)
            -- Bob's second client gets only the message destined for him
            void . liftIO $ WS.assertMatch t wsB2 (wsAssertOtr conv alice ac bc2 cipher)
            liftIO $ assertBool "unexpected equal clients" (bc /= bc2)
            assertNoMsg wsB2 (wsAssertOtr conv alice ac bc cipher)

postCryptoMessage2 :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postCryptoMessage2 g b _ _ = do
    (alice, ac) <- randomUserWithClient b (someLastPrekeys !! 0)
    (bob,   bc) <- randomUserWithClient b (someLastPrekeys !! 1)
    (eve,   ec) <- randomUserWithClient b (someLastPrekeys !! 2)
    connectUsers b alice (list1 bob [eve])
    conv <- decodeConvId <$> postConv g alice [bob, eve] (Just "gossip") []
    -- Missing eve
    let m = [(bob, bc, "hello bob")]
    r1 <- postOtrMessage id g alice ac conv m <!!
        const 412 === statusCode
    let x = decodeBody' "ClientMismatch" r1
    liftIO $ assertBool "client mismatch" (eqMismatch [(eve, Set.singleton ec)] [] [] (Just x))
    -- Fetch all missing clients prekeys
    r2 <- post (b . path "/users/prekeys" . json (missingClients x)) <!!
        const 200 === statusCode
    let p = decodeBody' "prekeys" r2 :: UserClientMap (Maybe Prekey)
    liftIO $ do
        Map.keys (userClientMap p) @=? [eve]
        Map.keys <$> Map.lookup eve (userClientMap p) @=? Just [ec]

postCryptoMessage3 :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postCryptoMessage3 g b _ _ = do
    (alice, ac) <- randomUserWithClient b (someLastPrekeys !! 0)
    (bob,   bc) <- randomUserWithClient b (someLastPrekeys !! 1)
    (eve,   ec) <- randomUserWithClient b (someLastPrekeys !! 2)
    connectUsers b alice (list1 bob [eve])
    conv <- decodeConvId <$> postConv g alice [bob, eve] (Just "gossip") []
    -- Missing eve
    let ciphertext = encodeCiphertext "hello bob"
    let m = otrRecipients [(bob, [(bc, ciphertext)])]
    r1 <- postProtoOtrMessage g alice ac conv m <!!
        const 412 === statusCode
    let x = decodeBody' "ClientMismatch" r1
    liftIO $ assertBool "client mismatch" (eqMismatch [(eve, Set.singleton ec)] [] [] (Just x))
    -- Fetch all missing clients prekeys
    r2 <- post (b . path "/users/prekeys" . json (missingClients x)) <!!
        const 200 === statusCode
    let p = decodeBody' "prekeys" r2 :: UserClientMap (Maybe Prekey)
    liftIO $ do
        Map.keys (userClientMap p) @=? [eve]
        Map.keys <$> Map.lookup eve (userClientMap p) @=? Just [ec]

postCryptoMessage4 :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postCryptoMessage4 g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    bc    <- randomClient b bob (someLastPrekeys !! 0)
    connectUsers b alice (list1 bob [])
    conv <- decodeConvId <$> postConv g alice [bob] (Just "gossip") []
    -- Unknown client ID => 403
    let ciphertext = encodeCiphertext "hello bob"
    let m = otrRecipients [(bob, [(bc, ciphertext)])]
    postProtoOtrMessage g alice (ClientId "172618352518396") conv m !!!
        const 403 === statusCode

postCryptoMessage5 :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postCryptoMessage5 g b _ _ = do
    (alice, ac) <- randomUserWithClient b (someLastPrekeys !! 0)
    (bob,   bc) <- randomUserWithClient b (someLastPrekeys !! 1)
    (eve,   ec) <- randomUserWithClient b (someLastPrekeys !! 2)
    connectUsers b alice (list1 bob [eve])
    conv <- decodeConvId <$> postConv g alice [bob, eve] (Just "gossip") []

    -- Missing eve
    let m = [(bob, bc, "hello bob")]

    -- These three are quivalent (i.e. report all missing clients)
    postOtrMessage id g alice ac conv m !!!
        const 412 === statusCode
    postOtrMessage (queryItem "ignore_missing" "false") g alice ac conv m !!!
        const 412 === statusCode
    postOtrMessage (queryItem "report_missing" "true") g alice ac conv m !!!
        const 412 === statusCode

    -- These two are quivalent (i.e. ignore all missing clients)
    postOtrMessage (queryItem "ignore_missing" "true") g alice ac conv m !!!
        const 201 === statusCode
    postOtrMessage (queryItem "report_missing" "false") g alice ac conv m !!!
        const 201 === statusCode

    -- Report missing clients of a specific user only
    postOtrMessage (queryItem "report_missing" (toByteString' bob)) g alice ac conv m !!!
        const 201 === statusCode
    _rs <- postOtrMessage (queryItem "report_missing" (toByteString' eve)) g alice ac conv [] <!!
        const 412 === statusCode
    let _mm = decodeBody' "ClientMismatch" _rs
    liftIO $ assertBool "client mismatch" (eqMismatch [(eve, Set.singleton ec)] [] [] (Just _mm))

    -- Ignore missing clients of a specific user only
    postOtrMessage (queryItem "ignore_missing" (toByteString' eve)) g alice ac conv m !!!
        const 201 === statusCode
    _rs <- postOtrMessage (queryItem "ignore_missing" (toByteString' eve)) g alice ac conv [] <!!
        const 412 === statusCode
    let _mm = decodeBody' "ClientMismatch" _rs
    liftIO $ assertBool "client mismatch" (eqMismatch [(bob, Set.singleton bc)] [] [] (Just _mm))

postJoinConvOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postJoinConvOk g b c _ = do
    alice <- randomUser b
    bob   <- randomUser b
    conv  <- decodeConvId <$> postConv g alice [] (Just "gossip") [InviteAccess, LinkAccess]
    WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
        postJoinConv g bob conv !!! const 200 === statusCode
        postJoinConv g bob conv !!! const 204 === statusCode
        void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB] $
            wsAssertMemberJoin conv bob [bob]

postJoinCodeConvOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postJoinCodeConvOk g b c _ = do
    alice <- randomUser b
    bob   <- randomUser b
    conv  <- decodeConvId <$> postConv g alice [] (Just "gossip") [CodeAccess]
    cCode <- decodeConvCodeEvent <$> postConvCode g alice conv
    -- currently ConversationCode is used both as return type for POST ../code and as body for ../join
    -- TODO: Should there be two different types?
    let payload = cCode {conversationUri = Nothing} -- unnecessary step, cCode can be posted as-is also.
    WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
        postJoinCodeConv g bob payload !!! const 200 === statusCode
        postJoinCodeConv g bob payload !!! const 204 === statusCode
        void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB] $
            wsAssertMemberJoin conv bob [bob]

postConvertCodeConv :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postConvertCodeConv g b c _ = do
    alice <- randomUser b
    conv  <- decodeConvId <$> postConv g alice [] (Just "gossip") [InviteAccess]
    -- Cannot do code operations if conversation not in code access
    postConvCode g alice conv !!! const 403 === statusCode
    deleteConvCode g alice conv !!! const 403 === statusCode
    getConvCode g alice conv !!! const 403 === statusCode
    -- cannot change to NoAccess as not a team conversation
    let teamAccess = ConversationAccessUpdate []
    putAccessUpdate g alice conv teamAccess !!! const 403 === statusCode
    -- change access
    WS.bracketR c alice $ \wsA -> do
        let codeAccess = ConversationAccessUpdate [InviteAccess, CodeAccess]
        putAccessUpdate g alice conv codeAccess !!! const 200 === statusCode
        -- test no-op
        putAccessUpdate g alice conv codeAccess !!! const 204 === statusCode
        void . liftIO $ WS.assertMatchN (5 # Second) [wsA] $
            wsAssertConvAccessUpdate conv alice codeAccess
    -- Create/get/update/delete codes
    getConvCode g alice conv !!! const 404 === statusCode
    c1 <- decodeConvCodeEvent <$> postConvCode g alice conv
    c1' <- decodeConvCode <$> getConvCode g alice conv
    liftIO $ assertEqual "c1 c1' codes should match" c1 c1'
    c2 <- decodeConvCodeEvent <$> postConvCode g alice conv
    liftIO $ assertBool "c2 should be different" (c1 /= c2)
    c2' <- decodeConvCode <$> getConvCode g alice conv
    liftIO $ assertEqual "c2 c2' codes should match" c2 c2'
    deleteConvCode g alice conv !!! const 200 === statusCode
    getConvCode g alice conv !!! const 404 === statusCode


postConvertTeamConv :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postConvertTeamConv g b c setup = do
    -- create team conversation team-alice, team-bob, guest-eve
    let a = awsEnv setup
    alice <- randomUser b
    tid   <- createTeamInternal g "foo" alice
    assertQueue "create team" a tActivate
    let p1 = symmPermissions [Teams.AddConversationMember]
    bobMem <- flip Teams.newTeamMember p1 <$> randomUser b
    addTeamMemberInternal g tid bobMem
    let bob = bobMem^.Teams.userId
    assertQueue "team member join" a $ tUpdate 2 [alice]
    eve  <- randomUser b
    connectUsers b alice (singleton eve)
    let acc = Just $ Set.fromList [InviteAccess, CodeAccess]
    conv <- createTeamConv g alice (ConvTeamInfo tid False) [bob, eve] (Just "blaa") acc
    -- mallory joins by herself
    mallory  <- randomUser b
    j <- decodeConvCodeEvent <$> postConvCode g alice conv
    WS.bracketR3 c alice bob eve $ \(wsA, wsB, wsE) -> do
        postJoinCodeConv g mallory j !!! const 200 === statusCode
        void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB, wsE] $
            wsAssertMemberJoin conv mallory [mallory]

    WS.bracketRN c [alice, bob, eve, mallory] $ \[wsA, wsB, wsE, wsM] -> do
        let teamAccess = ConversationAccessUpdate []
        putAccessUpdate g alice conv teamAccess !!! const 200 === statusCode
        void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB, wsE, wsM] $
            wsAssertConvAccessUpdate conv alice teamAccess
        -- non-team members get kicked out
        void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB, wsE, wsM] $
            wsAssertMemberLeave conv alice [eve, mallory]
        -- joining is no longer possible
        postJoinCodeConv g mallory j !!! const 404 === statusCode

postJoinConvFail :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postJoinConvFail g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    conv  <- decodeConvId <$> postConv g alice [] (Just "gossip") []
    void $ postJoinConv g bob conv !!! const 404 === statusCode

getConvsOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
getConvsOk g b _ _ = do
    usr <- randomUser b
    getConvs g usr Nothing Nothing !!! do
        const 200           === statusCode
        const [toUUID usr]  === map (toUUID . cnvId) . decodeConvList

getConvsOk2 :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
getConvsOk2 g b _ _ = do
    [alice, bob] <- randomUsers b 2
    connectUsers b alice (singleton bob)
    -- create & get one2one conv
    cnv1 <- decodeBody' "conversation" <$> postO2OConv g alice bob (Just "gossip1")
    getConvs g alice (Just $ Left [cnvId cnv1]) Nothing !!! do
        const 200 === statusCode
        const (Just [cnvId cnv1]) === fmap (map cnvId . convList) . decodeBody
    -- create & get group conv
    carl <- randomUser b
    connectUsers b alice (singleton carl)
    cnv2 <- decodeBody' "conversation" <$> postConv g alice [bob, carl] (Just "gossip2") []
    getConvs g alice (Just $ Left [cnvId cnv2]) Nothing !!! do
        const 200 === statusCode
        const (Just [cnvId cnv2]) === fmap (map cnvId . convList) . decodeBody
    -- get both
    rs <- getConvs g alice Nothing Nothing <!! const 200 === statusCode
    let cs = convList <$> decodeBody rs
    let c1 = cs >>= find ((== cnvId cnv1) . cnvId)
    let c2 = cs >>= find ((== cnvId cnv2) . cnvId)
    liftIO $ forM_ [(cnv1, c1), (cnv2, c2)] $ \(expected, actual) -> do
        assertEqual "name mismatch"
            (Just $ cnvName expected)
            (cnvName <$> actual)
        assertEqual "self member mismatch"
            (Just . cmSelf $ cnvMembers expected)
            (cmSelf . cnvMembers <$> actual)
        assertEqual "other members mismatch" (Just [])
            ((\c -> cmOthers (cnvMembers c) \\ cmOthers (cnvMembers expected)) <$> actual)

getConvsFailMaxSize :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
getConvsFailMaxSize g b _ _ = do
    usr <- randomUser b
    getConvs g usr Nothing (Just 501) !!!
        const 400 === statusCode

getConvIdsOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
getConvIdsOk g b _ _ = do
    [alice, bob] <- randomUsers b 2
    connectUsers b alice (singleton bob)
    void $ postO2OConv g alice bob (Just "gossip")
    getConvIds g alice Nothing Nothing !!! do
        const 200 === statusCode
        const 2   === length . decodeConvIdList
    getConvIds g bob Nothing Nothing !!! do
        const 200 === statusCode
        const 2   === length . decodeConvIdList

paginateConvIds :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
paginateConvIds g b _ _ = do
    [alice, bob, eve] <- randomUsers b 3
    connectUsers b alice (singleton bob)
    connectUsers b alice (singleton eve)
    replicateM_ 256 $
        postConv g alice [bob, eve] (Just "gossip") [] !!!
            const 201 === statusCode
    foldM_ (getChunk 16 alice) Nothing [15 .. 0 :: Int]
  where
    getChunk size alice start n = do
        resp <- getConvIds g alice start (Just size) <!! const 200 === statusCode
        let c = fromMaybe (ConversationList [] False) (decodeBody resp)
        liftIO $ do
            length (convList c) @?= fromIntegral size
            convHasMore c       @?= n > 0
        return (Just (Right (last (convList c))))

getConvIdsFailMaxSize :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
getConvIdsFailMaxSize g b _ _ = do
    usr <- randomUser b
    getConvIds g usr Nothing (Just 1001) !!!
        const 400 === statusCode

getConvsPagingOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
getConvsPagingOk g b _ _ = do
    [ally, bill, carl] <- randomUsers b 3
    connectUsers b ally (list1 bill [carl])
    replicateM_ 11 $ postConv g ally [bill, carl] (Just "gossip") []
    walk ally [3,3,3,3,2]  -- 11 (group) + 2 (1:1) + 1 (self)
    walk bill [3,3,3,3,1]  -- 11 (group) + 1 (1:1) + 1 (self)
    walk carl [3,3,3,3,1]  -- 11 (group) + 1 (1:1) + 1 (self)
  where
    walk u = foldM_ (next u 3) Nothing
    next u step start n = do
        r1 <- getConvIds g u (Right <$> start) (Just step) <!! const 200 === statusCode
        let ids1 = convList <$> decodeBody r1
        liftIO $ assertEqual "unexpected length (getConvIds)" (Just n) (length <$> ids1)

        r2 <- getConvs g u (Right <$> start) (Just step) <!! const 200 === statusCode
        let ids3 = map cnvId . convList <$> decodeBody r2
        liftIO $ assertEqual "unexpected length (getConvs)" (Just n) (length <$> ids3)

        liftIO $ assertBool "getConvIds /= getConvs" (ids1 == ids3)

        return $ ids1 >>= listToMaybe . reverse

postConvFailNotConnected :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postConvFailNotConnected g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    jane  <- randomUser b
    postConv g alice [bob, jane] Nothing [] !!! do
        const 403 === statusCode
        const (Just "not-connected") === fmap label . decodeBody

postConvFailNumMembers :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postConvFailNumMembers g b _ s = do
    let n = fromIntegral (maxConvTeamSize s)
    alice <- randomUser b
    bob:others <- replicateM n (randomUser b)
    connectUsers b alice (list1 bob others)
    postConv g alice (bob:others) Nothing [] !!! do
        const 400 === statusCode
        const (Just "client-error") === fmap label . decodeBody

postSelfConvOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postSelfConvOk g b _ _ = do
    alice <- randomUser b
    m <- postSelfConv g alice <!! const 200 === statusCode
    n <- postSelfConv g alice <!! const 200 === statusCode
    mId <- assertConv m SelfConv alice alice [] Nothing
    nId <- assertConv n SelfConv alice alice [] Nothing
    liftIO $ mId @=? nId

postO2OConvOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postO2OConvOk g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    connectUsers b alice (singleton bob)
    a <- postO2OConv g alice bob (Just "chat") <!! const 200 === statusCode
    c <- postO2OConv g alice bob (Just "chat") <!! const 200 === statusCode
    aId <- assertConv a One2OneConv alice alice [bob] (Just "chat")
    cId <- assertConv c One2OneConv alice alice [bob] (Just "chat")
    liftIO $ aId @=? cId

postConvO2OFailWithSelf :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postConvO2OFailWithSelf g b _ _ = do
    alice <- randomUser b
    let inv = NewConv [alice] Nothing mempty Nothing
    post (g . path "/conversations/one2one" . zUser alice . zConn "conn" . zType "access" . json inv) !!! do
        const 403 === statusCode
        const (Just "invalid-op") === fmap label . decodeBody

postConnectConvOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postConnectConvOk g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    m <- postConnectConv g alice bob "Alice" "connect with me!" Nothing <!!
            const 201 === statusCode
    n <- postConnectConv g alice bob "Alice" "connect with me!" Nothing <!!
            const 200 === statusCode
    mId <- assertConv m ConnectConv alice alice [] (Just "Alice")
    nId <- assertConv n ConnectConv alice alice [] (Just "Alice")
    liftIO $ mId @=? nId

postConnectConvOk2 :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postConnectConvOk2 g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    m <- decodeConvId <$> request alice bob
    n <- decodeConvId <$> request alice bob
    liftIO $ m @=? n
  where
    request alice bob =
        postConnectConv g alice bob "Alice" "connect with me!" (Just "me@me.com")

putConvAcceptOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
putConvAcceptOk g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    cnv   <- decodeConvId <$> postConnectConv g alice bob "Alice" "come to zeta!" Nothing
    putConvAccept g bob cnv !!! const 200 === statusCode
    getConv g alice cnv !!! do
        const 200 === statusCode
        const (Just One2OneConv) === fmap cnvType . decodeBody
    getConv g bob cnv !!! do
        const 200 === statusCode
        const (Just One2OneConv) === fmap cnvType . decodeBody

putConvAcceptRetry :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
putConvAcceptRetry g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    connectUsers b alice (singleton bob)
    cnv   <- decodeConvId <$> postO2OConv g alice bob (Just "chat")
    -- If the conversation type is already One2One, everything is 200 OK
    putConvAccept g bob cnv !!! const 200 === statusCode

postMutualConnectConvOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postMutualConnectConvOk g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    ac <- postConnectConv g alice bob "A" "a" Nothing <!!
            const 201 === statusCode
    acId <- assertConv ac ConnectConv alice alice [] (Just "A")
    bc <- postConnectConv g bob alice "B" "b" Nothing <!!
            const 200 === statusCode
    -- The connect conversation was simply accepted, thus the
    -- conversation name and message sent in Bob's request ignored.
    bcId <- assertConv bc One2OneConv alice bob [alice] (Just "A")
    liftIO $ acId @=? bcId

postRepeatConnectConvCancel :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postRepeatConnectConvCancel g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b

    -- Alice wants to connect
    rsp1 <- postConnectConv g alice bob "A" "a" Nothing <!! const 201 === statusCode
    let cnv = decodeBody' "conversation" rsp1
    liftIO $ do
        ConnectConv   @=? cnvType cnv
        (Just "A")    @=? cnvName cnv
        []            @=? cmOthers (cnvMembers cnv)
        privateAccess @=? cnvAccess cnv

    -- Alice blocks / cancels
    cancel alice cnv

    -- Alice makes another connect attempt
    rsp2 <- postConnectConv g alice bob "A2" "a2" Nothing <!! const 200 === statusCode
    let cnv2 = decodeBody' "conversation" rsp2
    liftIO $ do
        ConnectConv   @=? cnvType cnv2
        (Just "A2")   @=? cnvName cnv2
        []            @=? cmOthers (cnvMembers cnv2)
        privateAccess @=? cnvAccess cnv2

    -- Alice blocks / cancels again
    cancel alice cnv

    -- Now Bob attempts to connect
    rsp3 <- postConnectConv g bob alice "B" "b" Nothing <!! const 200 === statusCode
    let cnv3 = decodeBody' "conversation" rsp3
    liftIO $ do
        ConnectConv   @=? cnvType cnv3
        (Just "B")    @=? cnvName cnv3
        privateAccess @=? cnvAccess cnv3

    -- Bob accepting is a no-op, since he is already a member
    putConvAccept g bob (cnvId cnv) !!! const 200 === statusCode
    cnvX <- decodeBody' "conversation" <$> getConv g bob (cnvId cnv)
    liftIO $ do
        ConnectConv   @=? cnvType cnvX
        (Just "B")    @=? cnvName cnvX
        privateAccess @=? cnvAccess cnvX

    -- Alice accepts, finally turning it into a 1-1
    putConvAccept g alice (cnvId cnv) !!! const 200 === statusCode
    cnv4 <- decodeBody' "conversation" <$> getConv g alice (cnvId cnv)
    liftIO $ do
        One2OneConv     @=? cnvType cnv4
        (Just "B")      @=? cnvName cnv4
        privateAccess   @=? cnvAccess cnv4
  where
    cancel u c = do
        put (g . paths ["/i/conversations", toByteString' (cnvId c), "block"] . zUser u) !!!
            const 200 === statusCode
        getConv g u (cnvId c) !!! const 404 === statusCode

putBlockConvOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
putBlockConvOk g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    conv  <- decodeBody' "conversation" <$> postConnectConv g alice bob "Alice" "connect with me!" (Just "me@me.com")

    getConv g alice (cnvId conv) !!! const 200 === statusCode
    getConv g bob (cnvId conv)   !!! const 404 === statusCode

    put (g . paths ["/i/conversations", toByteString' (cnvId conv), "block"] . zUser bob) !!!
        const 200 === statusCode

    -- A is still the only member of the 1-1
    getConv g alice (cnvId conv) !!! do
        const 200 === statusCode
        const (cnvMembers conv) === cnvMembers . decodeBody' "conversation"

    -- B accepts the conversation by unblocking
    put (g . paths ["/i/conversations", toByteString' (cnvId conv), "unblock"] . zUser bob) !!!
        const 200 === statusCode
    getConv g bob (cnvId conv) !!! const 200 === statusCode

    -- B blocks A in the 1-1
    put (g . paths ["/i/conversations", toByteString' (cnvId conv), "block"] . zUser bob) !!!
        const 200 === statusCode

    -- B no longer sees the 1-1
    getConv g bob (cnvId conv) !!! const 404 === statusCode

    -- B unblocks A in the 1-1
    put (g . paths ["/i/conversations", toByteString' (cnvId conv), "unblock"] . zUser bob) !!!
        const 200 === statusCode

    -- B sees the blocked 1-1 again
    getConv g bob (cnvId conv) !!! do
        const 200 === statusCode

getConvOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
getConvOk g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    chuck <- randomUser b
    connectUsers b alice (list1 bob [chuck])
    conv  <- decodeConvId <$> postConv g alice [bob, chuck] (Just "gossip") []
    getConv g alice conv !!! const 200 === statusCode
    getConv g bob   conv !!! const 200 === statusCode
    getConv g chuck conv !!! const 200 === statusCode

accessConvMeta :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
accessConvMeta g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    chuck <- randomUser b
    connectUsers b alice (list1 bob [chuck])
    conv  <- decodeConvId <$> postConv g alice [bob, chuck] (Just "gossip") []
    let meta = ConversationMeta conv RegularConv alice [InviteAccess] (Just "gossip") Nothing
    get (g . paths ["i/conversations", toByteString' conv, "meta"] . zUser alice) !!! do
        const 200         === statusCode
        const (Just meta) === (decode <=< responseBody)

leaveConnectConversation :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
leaveConnectConversation g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    bdy   <- postConnectConv g alice bob "alice" "ni" Nothing <!! const 201 === statusCode
    let c = fromMaybe (error "invalid connect conversation") (cnvId <$> decodeBody bdy)
    deleteMember g alice alice c !!! const 403 === statusCode

postMembersOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postMembersOk g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    chuck <- randomUser b
    eve   <- randomUser b
    connectUsers b alice (list1 bob [chuck, eve])
    connectUsers b eve (singleton bob)
    conv  <- decodeConvId <$> postConv g alice [bob, chuck] (Just "gossip") []
    postMembers g alice (singleton eve) conv !!! const 200 === statusCode
    -- Check that last_event markers are set for all members
    forM_ [alice, bob, chuck, eve] $ \u -> do
        _ <- getSelfMember g u conv <!! const 200 === statusCode
        return ()

postMembersOk2 :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postMembersOk2 g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    chuck <- randomUser b
    connectUsers b alice (list1 bob [chuck])
    connectUsers b bob (singleton chuck)
    conv  <- decodeConvId <$> postConv g alice [bob, chuck] Nothing []
    postMembers g bob (singleton chuck) conv !!! const 204 === statusCode
    chuck' <- decodeBody <$> (getSelfMember g chuck conv <!! const 200 === statusCode)
    liftIO $
        assertEqual "wrong self member" (memId <$> chuck') (Just chuck)

postMembersOk3 :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postMembersOk3 g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    eve   <- randomUser b
    connectUsers b alice (list1 bob [eve])
    conv  <- decodeConvId <$> postConv g alice [bob, eve] (Just "gossip") []

    -- Bob leaves
    deleteMember g bob bob conv !!! const 200 === statusCode

    -- Fetch bob
    getSelfMember g bob conv !!! const 200 === statusCode

    -- Alice re-adds Bob to the conversation
    postMembers g alice (singleton bob) conv !!! const 200 === statusCode

    -- Fetch bob again
    getSelfMember g bob conv !!! const 200 === statusCode

postMembersFail :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postMembersFail g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    chuck <- randomUser b
    dave  <- randomUser b
    eve   <- randomUser b
    connectUsers b alice (list1 bob [chuck, eve])
    connectUsers b eve (singleton bob)
    conv  <- decodeConvId <$> postConv g alice [bob, chuck] (Just "gossip") []
    postMembers g eve (singleton bob) conv !!! const 404 === statusCode
    postMembers g alice (singleton eve) conv !!! const 200 === statusCode
    -- Not connected but already there
    postMembers g chuck (singleton eve) conv !!! const 204 === statusCode
    postMembers g chuck (singleton dave) conv !!! do
        const 403 === statusCode
        const (Just "not-connected") === fmap label . decodeBody
    void $ connectUsers b chuck (singleton dave)
    postMembers g chuck (singleton dave) conv !!! const 200 === statusCode
    postMembers g chuck (singleton dave) conv !!! const 204 === statusCode

postTooManyMembersFail :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postTooManyMembersFail g b _ s = do
    let n = fromIntegral (maxConvTeamSize s)
    alice <- randomUser b
    bob   <- randomUser b
    chuck <- randomUser b
    connectUsers b alice (list1 bob [chuck])
    conv  <- decodeConvId <$> postConv g alice [bob, chuck] (Just "gossip") []
    x:xs  <- randomUsers b (n - 2)
    postMembers g chuck (list1 x xs) conv !!! do
        const 403 === statusCode
        const (Just "too-many-members") === fmap label . decodeBody

deleteMembersOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
deleteMembersOk g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    eve   <- randomUser b
    connectUsers b alice (list1 bob [eve])
    conv  <- decodeConvId <$> postConv g alice [bob, eve] (Just "gossip") []
    deleteMember g bob bob conv     !!! const 200 === statusCode
    deleteMember g bob bob conv     !!! const 404 === statusCode
    deleteMember g alice eve conv   !!! const 200 === statusCode
    deleteMember g alice eve conv   !!! const 204 === statusCode
    deleteMember g alice alice conv !!! const 200 === statusCode
    deleteMember g alice alice conv !!! const 404 === statusCode

deleteMembersFailSelf :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
deleteMembersFailSelf g b _ _ = do
    alice <- randomUser b
    self  <- decodeConvId <$> postSelfConv g alice
    deleteMember g alice alice self !!! const 403 === statusCode

deleteMembersFailO2O :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
deleteMembersFailO2O g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    connectUsers b alice (singleton bob)
    o2o   <- decodeConvId <$> postO2OConv g alice bob (Just "foo")
    deleteMember g alice bob o2o !!! const 403 === statusCode

putConvRenameOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
putConvRenameOk g b c _ = do
    alice <- randomUser b
    bob   <- randomUser b
    connectUsers b alice (singleton bob)
    conv  <- decodeConvId <$> postO2OConv g alice bob (Just "gossip")
    WS.bracketR2 c alice bob $ \(wsA, wsB) -> do
        let update = ConversationRename "gossip++"
        put ( g
            . paths ["conversations", toByteString' conv]
            . zUser bob
            . zConn "conn"
            . zType "access"
            . json update
            ) !!! const 200 === statusCode
        void. liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB] $ \n -> do
            let e = List1.head (WS.unpackPayload n)
            ntfTransient n @?= False
            evtConv      e @?= conv
            evtType      e @?= ConvRename
            evtFrom      e @?= bob
            evtData      e @?= Just (EdConvRename (ConversationRename "gossip++"))

putMemberOtrMuteOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
putMemberOtrMuteOk g b c _ = do
    putMemberOk (memberUpdate { mupOtrMute = Just True, mupOtrMuteRef = Just "ref" }) g b c
    putMemberOk (memberUpdate { mupOtrMute = Just False }) g b c

putMemberOtrArchiveOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
putMemberOtrArchiveOk g b c _ = do
    putMemberOk (memberUpdate { mupOtrArchive = Just True, mupOtrArchiveRef = Just "ref" }) g b c
    putMemberOk (memberUpdate { mupOtrArchive = Just False }) g b c

putMemberHiddenOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
putMemberHiddenOk g b c _ = do
    putMemberOk (memberUpdate { mupHidden = Just True, mupHiddenRef = Just "ref" }) g b c
    putMemberOk (memberUpdate { mupHidden = Just False }) g b c

putMemberAllOk :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
putMemberAllOk g b c _ = putMemberOk
    (memberUpdate
        { mupOtrMute = Just True
        , mupOtrMuteRef = Just "mref"
        , mupOtrArchive = Just True
        , mupOtrArchiveRef = Just "aref"
        , mupHidden = Just True
        , mupHiddenRef = Just "href"
        }) g b c

putMemberOk :: MemberUpdate -> Galley -> Brig -> Cannon -> Http ()
putMemberOk update g b ca = do
    alice <- randomUser b
    bob   <- randomUser b
    connectUsers b alice (singleton bob)
    conv  <- decodeConvId <$> postO2OConv g alice bob (Just "gossip")
    getConv g alice conv !!! const 200 === statusCode

    -- Expected member state
    let memberBob = Member
                  { memId = bob
                  , memService = Nothing
                  , memOtrMuted = fromMaybe False (mupOtrMute update)
                  , memOtrMutedRef = mupOtrMuteRef update
                  , memOtrArchived = fromMaybe False (mupOtrArchive update)
                  , memOtrArchivedRef = mupOtrArchiveRef update
                  , memHidden = fromMaybe False (mupHidden update)
                  , memHiddenRef = mupHiddenRef update
                  }

    -- Update member state & verify push notification
    WS.bracketR ca bob $ \ws -> do
        putMember g bob update conv !!! const 200 === statusCode
        void. liftIO $ WS.assertMatch (5 # Second) ws $ \n -> do
            let e = List1.head (WS.unpackPayload n)
            ntfTransient n @?= False
            evtConv      e @?= conv
            evtType      e @?= MemberStateUpdate
            evtFrom      e @?= bob
            case evtData e of
                Just (EdMemberUpdate mis) -> do
                    assertEqual "otr_muted"        (mupOtrMute update)       (misOtrMuted mis)
                    assertEqual "otr_muted_ref"    (mupOtrMuteRef update)    (misOtrMutedRef mis)
                    assertEqual "otr_archived"     (mupOtrArchive update)    (misOtrArchived mis)
                    assertEqual "otr_archived_ref" (mupOtrArchiveRef update) (misOtrArchivedRef mis)
                    assertEqual "hidden"           (mupHidden update)        (misHidden mis)
                    assertEqual "hidden_ref"       (mupHiddenRef update)     (misHiddenRef mis)
                x -> assertFailure $ "Unexpected event data: " ++ show x

    -- Verify new member state
    rs <- getConv g bob conv <!! const 200 === statusCode
    let bob' = cmSelf . cnvMembers <$> decodeBody rs
    liftIO $ do
        assertBool   "user"             (isJust bob')
        let newBob = fromJust bob'
        assertEqual  "id"               (memId memberBob)             (memId newBob)
        assertEqual  "otr_muted"        (memOtrMuted memberBob)       (memOtrMuted newBob)
        assertEqual  "otr_muted_ref"    (memOtrMutedRef memberBob)    (memOtrMutedRef newBob)
        assertEqual  "otr_archived"     (memOtrArchived memberBob)    (memOtrArchived newBob)
        assertEqual  "otr_archived_ref" (memOtrArchivedRef memberBob) (memOtrArchivedRef newBob)
        assertEqual  "hidden"           (memHidden memberBob)         (memHidden newBob)
        assertEqual  "hidden__ref"      (memHiddenRef memberBob)      (memHiddenRef newBob)

postTypingIndicators :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
postTypingIndicators g b _ _ = do
    alice <- randomUser b
    bob   <- randomUser b
    connectUsers b alice (singleton bob)
    conv  <- decodeConvId <$> postO2OConv g alice bob Nothing
    post ( g
         . paths ["conversations", toByteString' conv, "typing"]
         . zUser bob
         . zConn "conn"
         . zType "access"
         . json (TypingData StartedTyping)
         ) !!!
        const 200 === statusCode
    post ( g
         . paths ["conversations", toByteString' conv, "typing"]
         . zUser bob
         . zConn "conn"
         . zType "access"
         . json (object ["status" .= ("dummy" :: T.Text)])
         ) !!!
        const 400 === statusCode

removeUser :: Galley -> Brig -> Cannon -> TestSetup -> Http ()
removeUser g b ca _ = do
    alice <- randomUser b
    bob   <- randomUser b
    carl  <- randomUser b
    connectUsers b alice (list1 bob [carl])
    conv1 <- decodeConvId <$> postConv g alice [bob] (Just "gossip") []
    conv2 <- decodeConvId <$> postConv g alice [bob, carl] (Just "gossip2") []
    conv3 <- decodeConvId <$> postConv g alice [carl] (Just "gossip3") []
    WS.bracketR3 ca alice bob carl $ \(wsA, wsB, wsC) -> do
        deleteUser g bob
        void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB] $
            matchMemberLeave conv1 bob
        void . liftIO $ WS.assertMatchN (5 # Second) [wsA, wsB, wsC] $
            matchMemberLeave conv2 bob
    -- Check memberships
    mems1 <- fmap cnvMembers . decodeBody <$> getConv g alice conv1
    mems2 <- fmap cnvMembers . decodeBody <$> getConv g alice conv2
    mems3 <- fmap cnvMembers . decodeBody <$> getConv g alice conv3
    let other u = find ((== u) . omId) . cmOthers
    liftIO $ do
        (mems1 >>= other bob)  @?= Nothing
        (mems2 >>= other bob)  @?= Nothing
        (mems2 >>= other carl) @?= Just (OtherMember carl Nothing)
        (mems3 >>= other bob)  @?= Nothing
        (mems3 >>= other carl) @?= Just (OtherMember carl Nothing)
  where
    matchMemberLeave conv u n = do
        let e = List1.head (WS.unpackPayload n)
        ntfTransient n @?= False
        evtConv      e @?= conv
        evtType      e @?= MemberLeave
        evtFrom      e @?= u
        evtData      e @?= Just (EdMembers (Members [u]))
