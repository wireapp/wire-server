{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module MLS.Util where

import API.Brig
import API.Galley
import Control.Concurrent.Async hiding (link)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Codensity
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Char8 as C8
import Data.Default
import Data.Foldable
import Data.Function
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T
import Data.Traversable
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import GHC.Stack
import Notifications
import System.Directory
import System.Exit
import System.FilePath
import System.IO hiding (print, putStrLn)
import System.IO.Temp
import System.Posix.Files
import System.Process
import Testlib.Assertions
import Testlib.HTTP
import Testlib.JSON
import Testlib.Prelude

mkClientIdentity :: (MakesValue u, MakesValue c) => u -> c -> App ClientIdentity
mkClientIdentity u c = do
  (domain, user) <- objQid u
  client <- c %. "id" & asString
  pure $ ClientIdentity {domain = domain, user = user, client = client}

cid2Str :: ClientIdentity -> String
cid2Str cid = cid.user <> ":" <> cid.client <> "@" <> cid.domain

data MessagePackage = MessagePackage
  { sender :: ClientIdentity,
    message :: ByteString,
    welcome :: Maybe ByteString,
    groupInfo :: Maybe ByteString
  }

getConv :: App Value
getConv = do
  mls <- getMLSState
  case mls.convId of
    Nothing -> assertFailure "Uninitialised test conversation"
    Just convId -> pure convId

toRandomFile :: ByteString -> App FilePath
toRandomFile bs = do
  p <- randomFileName
  liftIO $ BS.writeFile p bs
  pure p

randomFileName :: App FilePath
randomFileName = do
  bd <- getBaseDir
  (bd </>) . UUID.toString <$> liftIO UUIDV4.nextRandom

mlscli :: (HasCallStack) => ClientIdentity -> [String] -> Maybe ByteString -> App ByteString
mlscli cid args mbstdin = do
  groupOut <- randomFileName
  let substOut = argSubst "<group-out>" groupOut
  cs <- (.ciphersuite) <$> getMLSState
  let scheme = csSignatureScheme cs

  gs <- getClientGroupState cid

  substIn <- case gs.group of
    Nothing -> pure id
    Just groupData -> do
      fn <- toRandomFile groupData
      pure (argSubst "<group-in>" fn)
  store <- case Map.lookup scheme gs.keystore of
    Nothing -> do
      bd <- getBaseDir
      liftIO $ createDirectory (bd </> cid2Str cid)

      -- initialise new keystore
      path <- randomFileName
      ctype <- make gs.credType & asString
      void $ runCli path ["init", "--ciphersuite", cs.code, "-t", ctype, cid2Str cid] Nothing
      pure path
    Just s -> toRandomFile s

  let args' = map (substIn . substOut) args
  for_ args' $ \arg ->
    when (arg `elem` ["<group-in>", "<group-out>"]) $
      assertFailure ("Unbound arg: " <> arg)

  out <- runCli store args' mbstdin
  setGroup <- do
    groupOutWritten <- liftIO $ doesFileExist groupOut
    if groupOutWritten
      then do
        groupData <- liftIO (BS.readFile groupOut)
        pure $ \x -> x {group = Just groupData}
      else pure id
  setStore <- do
    storeData <- liftIO (BS.readFile store)
    pure $ \x -> x {keystore = Map.insert scheme storeData x.keystore}

  setClientGroupState cid (setGroup (setStore gs))

  pure out

runCli :: HasCallStack => FilePath -> [String] -> Maybe ByteString -> App ByteString
runCli store args mStdin =
  spawn
    ( proc
        "mls-test-cli"
        ( ["--store", store]
            <> args
        )
    )
    mStdin

argSubst :: String -> String -> String -> String
argSubst from to_ s =
  if s == from then to_ else s

createWireClient :: (MakesValue u, HasCallStack) => u -> App ClientIdentity
createWireClient u = do
  addClient u def
    >>= getJSON 201
    >>= mkClientIdentity u

data InitMLSClient = InitMLSClient
  {credType :: CredentialType}

instance Default InitMLSClient where
  def = InitMLSClient {credType = BasicCredentialType}

-- | Create new mls client and register with backend.
createMLSClient :: (MakesValue u, HasCallStack) => InitMLSClient -> u -> App ClientIdentity
createMLSClient opts u = do
  cid <- createWireClient u
  setClientGroupState cid def {credType = opts.credType}

  -- set public key
  pkey <- mlscli cid ["public-key"] Nothing
  ciphersuite <- (.ciphersuite) <$> getMLSState
  bindResponse
    ( updateClient
        cid
        def
          { mlsPublicKeys =
              Just (object [csSignatureScheme ciphersuite .= T.decodeUtf8 (Base64.encode pkey)])
          }
    )
    $ \resp -> resp.status `shouldMatchInt` 200
  pure cid

-- | create and upload to backend
uploadNewKeyPackage :: (HasCallStack) => ClientIdentity -> App String
uploadNewKeyPackage cid = do
  (kp, ref) <- generateKeyPackage cid

  -- upload key package
  bindResponse (uploadKeyPackages cid [kp]) $ \resp ->
    resp.status `shouldMatchInt` 201

  pure ref

generateKeyPackage :: HasCallStack => ClientIdentity -> App (ByteString, String)
generateKeyPackage cid = do
  suite <- (.ciphersuite) <$> getMLSState
  kp <- mlscli cid ["key-package", "create", "--ciphersuite", suite.code] Nothing
  ref <- B8.unpack . Base64.encode <$> mlscli cid ["key-package", "ref", "-"] (Just kp)
  fp <- keyPackageFile cid ref
  liftIO $ BS.writeFile fp kp
  pure (kp, ref)

-- | Create conversation and corresponding group.
createNewGroup :: (HasCallStack) => ClientIdentity -> App (String, Value)
createNewGroup cid = do
  conv <- postConversation cid defMLS >>= getJSON 201
  groupId <- conv %. "group_id" & asString
  convId <- conv %. "qualified_id"
  createGroup cid conv
  pure (groupId, convId)

-- | Retrieve self conversation and create the corresponding group.
createSelfGroup :: (HasCallStack) => ClientIdentity -> App (String, Value)
createSelfGroup cid = do
  conv <- getSelfConversation cid >>= getJSON 200
  groupId <- conv %. "group_id" & asString
  createGroup cid conv
  pure (groupId, conv)

createGroup :: (MakesValue conv) => ClientIdentity -> conv -> App ()
createGroup cid conv = do
  mls <- getMLSState
  case mls.groupId of
    Just _ -> assertFailure "only one group can be created"
    Nothing -> pure ()
  resetGroup cid conv

createSubConv :: (HasCallStack) => ClientIdentity -> String -> App ()
createSubConv cid subId = do
  mls <- getMLSState
  sub <- getSubConversation cid mls.convId subId >>= getJSON 200
  resetGroup cid sub
  void $ createPendingProposalCommit cid >>= sendAndConsumeCommitBundle

resetGroup :: (MakesValue conv) => ClientIdentity -> conv -> App ()
resetGroup cid conv = do
  convId <- objSubConvObject conv
  groupId <- conv %. "group_id" & asString
  modifyMLSState $ \s ->
    s
      { groupId = Just groupId,
        convId = Just convId,
        members = Set.singleton cid,
        epoch = 0,
        newMembers = mempty
      }
  resetClientGroup cid groupId

resetClientGroup :: ClientIdentity -> String -> App ()
resetClientGroup cid gid = do
  mls <- getMLSState
  removalKeyPaths <- asks (.removalKeyPaths)
  removalKeyPath <-
    assertOne $
      Map.lookup (csSignatureScheme mls.ciphersuite) removalKeyPaths
  void $
    mlscli
      cid
      [ "group",
        "create",
        "--removal-key",
        removalKeyPath,
        "--group-out",
        "<group-out>",
        "--ciphersuite",
        mls.ciphersuite.code,
        gid
      ]
      Nothing

keyPackageFile :: (HasCallStack) => ClientIdentity -> String -> App FilePath
keyPackageFile cid ref = do
  let ref' = map urlSafe ref
  bd <- getBaseDir
  pure $ bd </> cid2Str cid </> ref'
  where
    urlSafe '+' = '-'
    urlSafe '/' = '_'
    urlSafe c = c

unbundleKeyPackages :: (HasCallStack) => Value -> App [(ClientIdentity, ByteString)]
unbundleKeyPackages bundle = do
  let entryIdentity be = do
        d <- be %. "domain" & asString
        u <- be %. "user" & asString
        c <- be %. "client" & asString
        pure $ ClientIdentity {domain = d, user = u, client = c}

  bundleEntries <- bundle %. "key_packages" & asList
  for bundleEntries $ \be -> do
    kp64 <- be %. "key_package" & asString
    kp <- assertOne . toList . Base64.decode . B8.pack $ kp64
    cid <- entryIdentity be
    pure (cid, kp)

-- | Claim keypackages and create a commit/welcome pair on a given client.
-- Note that this alters the state of the group immediately. If we want to test
-- a scenario where the commit is rejected by the backend, we can restore the
-- group to the previous state by using an older version of the group file.
createAddCommit :: (HasCallStack) => ClientIdentity -> [Value] -> App MessagePackage
createAddCommit cid users = do
  mls <- getMLSState
  kps <- fmap concat . for users $ \user -> do
    bundle <- claimKeyPackages mls.ciphersuite cid user >>= getJSON 200
    unbundleKeyPackages bundle
  createAddCommitWithKeyPackages cid kps

withTempKeyPackageFile :: ByteString -> ContT a App FilePath
withTempKeyPackageFile bs = do
  bd <- lift getBaseDir
  ContT $ \k ->
    bracket
      (liftIO (openBinaryTempFile bd "kp"))
      (\(fp, _) -> liftIO (removeFile fp))
      $ \(fp, h) -> do
        liftIO $ BS.hPut h bs `finally` hClose h
        k fp

createAddCommitWithKeyPackages ::
  (HasCallStack) =>
  ClientIdentity ->
  [(ClientIdentity, ByteString)] ->
  App MessagePackage
createAddCommitWithKeyPackages cid clientsAndKeyPackages = do
  bd <- getBaseDir
  welcomeFile <- liftIO $ emptyTempFile bd "welcome"
  giFile <- liftIO $ emptyTempFile bd "gi"

  commit <- runContT (traverse (withTempKeyPackageFile . snd) clientsAndKeyPackages) $ \kpFiles ->
    mlscli
      cid
      ( [ "member",
          "add",
          "--group",
          "<group-in>",
          "--welcome-out",
          welcomeFile,
          "--group-info-out",
          giFile,
          "--group-out",
          "<group-out>"
        ]
          <> kpFiles
      )
      Nothing

  modifyMLSState $ \mls ->
    mls
      { newMembers = Set.fromList (map fst clientsAndKeyPackages)
      }

  welcome <- liftIO $ BS.readFile welcomeFile
  gi <- liftIO $ BS.readFile giFile
  pure $
    MessagePackage
      { sender = cid,
        message = commit,
        welcome = Just welcome,
        groupInfo = Just gi
      }

createRemoveCommit :: (HasCallStack) => ClientIdentity -> [ClientIdentity] -> App MessagePackage
createRemoveCommit cid targets = do
  bd <- getBaseDir
  welcomeFile <- liftIO $ emptyTempFile bd "welcome"
  giFile <- liftIO $ emptyTempFile bd "gi"

  groupStateMap <- do
    gs <- getClientGroupState cid
    groupData <- assertJust "Group state not initialised" gs.group
    Map.fromList <$> readGroupState groupData
  let indices = map (fromMaybe (error "could not find target") . flip Map.lookup groupStateMap) targets

  commit <-
    mlscli
      cid
      ( [ "member",
          "remove",
          "--group",
          "<group-in>",
          "--group-out",
          "<group-out>",
          "--welcome-out",
          welcomeFile,
          "--group-info-out",
          giFile
        ]
          <> map show indices
      )
      Nothing

  welcome <- liftIO $ BS.readFile welcomeFile
  gi <- liftIO $ BS.readFile giFile

  pure
    MessagePackage
      { sender = cid,
        message = commit,
        welcome = Just welcome,
        groupInfo = Just gi
      }

createAddProposals :: (HasCallStack) => ClientIdentity -> [Value] -> App [MessagePackage]
createAddProposals cid users = do
  mls <- getMLSState
  bundles <- for users $ (claimKeyPackages mls.ciphersuite cid >=> getJSON 200)
  kps <- concat <$> traverse unbundleKeyPackages bundles
  traverse (createAddProposalWithKeyPackage cid) kps

createReInitProposal :: (HasCallStack) => ClientIdentity -> App MessagePackage
createReInitProposal cid = do
  prop <-
    mlscli
      cid
      ["proposal", "--group-in", "<group-in>", "--group-out", "<group-out>", "re-init"]
      Nothing
  pure
    MessagePackage
      { sender = cid,
        message = prop,
        welcome = Nothing,
        groupInfo = Nothing
      }

createAddProposalWithKeyPackage ::
  ClientIdentity ->
  (ClientIdentity, ByteString) ->
  App MessagePackage
createAddProposalWithKeyPackage cid (_, kp) = do
  prop <- runContT (withTempKeyPackageFile kp) $ \kpFile ->
    mlscli
      cid
      ["proposal", "--group-in", "<group-in>", "--group-out", "<group-out>", "add", kpFile]
      Nothing
  pure
    MessagePackage
      { sender = cid,
        message = prop,
        welcome = Nothing,
        groupInfo = Nothing
      }

createPendingProposalCommit :: (HasCallStack) => ClientIdentity -> App MessagePackage
createPendingProposalCommit cid = do
  bd <- getBaseDir
  welcomeFile <- liftIO $ emptyTempFile bd "welcome"
  pgsFile <- liftIO $ emptyTempFile bd "pgs"
  commit <-
    mlscli
      cid
      [ "commit",
        "--group",
        "<group-in>",
        "--group-out",
        "<group-out>",
        "--welcome-out",
        welcomeFile,
        "--group-info-out",
        pgsFile
      ]
      Nothing

  welcome <- liftIO $ readWelcome welcomeFile
  pgs <- liftIO $ BS.readFile pgsFile
  pure
    MessagePackage
      { sender = cid,
        message = commit,
        welcome = welcome,
        groupInfo = Just pgs
      }

createExternalCommit ::
  (HasCallStack) =>
  ClientIdentity ->
  Maybe ByteString ->
  App MessagePackage
createExternalCommit cid mgi = do
  bd <- getBaseDir
  giFile <- liftIO $ emptyTempFile bd "gi"
  conv <- getConv
  gi <- case mgi of
    Nothing -> getGroupInfo cid conv >>= getBody 200
    Just v -> pure v
  commit <-
    mlscli
      cid
      [ "external-commit",
        "--group-info-in",
        "-",
        "--group-info-out",
        giFile,
        "--group-out",
        "<group-out>"
      ]
      (Just gi)

  modifyMLSState $ \mls ->
    mls
      { newMembers = Set.singleton cid
      -- This might be a different client than those that have been in the
      -- group from before.
      }

  newPgs <- liftIO $ BS.readFile giFile
  pure $
    MessagePackage
      { sender = cid,
        message = commit,
        welcome = Nothing,
        groupInfo = Just newPgs
      }

data MLSNotificationTag = MLSNotificationMessageTag | MLSNotificationWelcomeTag
  deriving (Show, Eq, Ord)

-- | Extract a conversation ID (including an optional subconversation) from an
-- event object.
eventSubConv :: (HasCallStack) => (MakesValue event) => event -> App Value
eventSubConv event = do
  sub <- lookupField event "subconv"
  conv <- event %. "qualified_conversation"
  objSubConvObject $
    object
      [ "parent_qualified_id" .= conv,
        "subconv_id" .= sub
      ]

consumingMessages :: (HasCallStack) => MessagePackage -> Codensity App ()
consumingMessages mp = Codensity $ \k -> do
  mls <- getMLSState
  -- clients that should receive the message itself
  let oldClients = Set.delete mp.sender mls.members
  -- clients that should receive a welcome message
  let newClients = Set.delete mp.sender mls.newMembers
  -- all clients that should receive some MLS notification, together with the
  -- expected notification tag
  let clients =
        map (,MLSNotificationMessageTag) (toList oldClients)
          <> map (,MLSNotificationWelcomeTag) (toList newClients)

  let newUsers =
        Set.delete mp.sender.user $
          Set.difference
            (Set.map (.user) newClients)
            (Set.map (.user) oldClients)
  withWebSockets (map fst clients) $ \wss -> do
    r <- k ()

    -- if the conversation is actually MLS (and not mixed), pick one client for
    -- each new user and wait for its join event
    when (mls.protocol == MLSProtocolMLS) $
      traverse_
        (awaitMatch isMemberJoinNotif)
        ( flip Map.restrictKeys newUsers
            . Map.mapKeys ((.user) . fst)
            . Map.fromList
            . toList
            $ zip clients wss
        )

    -- at this point we know that every new user has been added to the
    -- conversation
    for_ (zip clients wss) $ \((cid, t), ws) -> case t of
      MLSNotificationMessageTag -> void $ consumeMessageNoExternal cid (Just mp) ws
      MLSNotificationWelcomeTag -> consumeWelcome cid mp ws
    pure r

consumeMessageWithPredicate :: (HasCallStack) => (Value -> App Bool) -> ClientIdentity -> Maybe MessagePackage -> WebSocket -> App Value
consumeMessageWithPredicate p cid mmp ws = do
  mls <- getMLSState
  notif <- awaitMatch p ws
  event <- notif %. "payload.0"

  for_ mmp $ \mp -> do
    shouldMatch (eventSubConv event) (fromMaybe A.Null mls.convId)
    shouldMatch (event %. "from") mp.sender.user
    shouldMatch (event %. "data") (B8.unpack (Base64.encode mp.message))

  msgData <- event %. "data" & asByteString
  _ <- mlsCliConsume cid msgData
  showMessage cid msgData

-- | Get a single MLS message from a websocket and consume it. Return a JSON
-- representation of the message.
consumeMessage :: (HasCallStack) => ClientIdentity -> Maybe MessagePackage -> WebSocket -> App Value
consumeMessage = consumeMessageWithPredicate isNewMLSMessageNotif

-- | like 'consumeMessage' but but will not consume a message where the sender is the backend
consumeMessageNoExternal :: (HasCallStack) => ClientIdentity -> Maybe MessagePackage -> WebSocket -> App Value
consumeMessageNoExternal cid = consumeMessageWithPredicate isNewMLSMessageNotifButNoProposal cid
  where
    -- the backend (correctly) reacts to a commit removing someone from a parent conversation with a
    -- remove proposal, however, we don't want to consume this here
    isNewMLSMessageNotifButNoProposal :: Value -> App Bool
    isNewMLSMessageNotifButNoProposal n = do
      isNotif <- isNewMLSMessageNotif n
      if isNotif
        then do
          msg <- n %. "payload.0.data" & asByteString >>= showMessage cid
          sender <- msg `lookupField` "message.content.sender" `catch` \(_ :: AssertionFailure) -> pure Nothing
          let backendSender = object ["External" .= Number 0]
          pure $ sender /= Just backendSender
        else pure False

mlsCliConsume :: ClientIdentity -> ByteString -> App ByteString
mlsCliConsume cid msgData =
  mlscli
    cid
    [ "consume",
      "--group",
      "<group-in>",
      "--group-out",
      "<group-out>",
      "-"
    ]
    (Just msgData)

-- | Send an MLS message, wait for clients to receive it, then consume it on
-- the client side. If the message is a commit, the
-- 'sendAndConsumeCommitBundle' function should be used instead.
sendAndConsumeMessage :: (HasCallStack) => MessagePackage -> App Value
sendAndConsumeMessage mp = lowerCodensity $ do
  consumingMessages mp
  lift $ postMLSMessage mp.sender mp.message >>= getJSON 201

-- | Send an MLS commit bundle, wait for clients to receive it, consume it, and
-- update the test state accordingly.
sendAndConsumeCommitBundle :: (HasCallStack) => MessagePackage -> App Value
sendAndConsumeCommitBundle mp = do
  lowerCodensity $ do
    consumingMessages mp
    lift $ do
      r <- postMLSCommitBundle mp.sender (mkBundle mp) >>= getJSON 201

      -- if the sender is a new member (i.e. it's an external commit), then
      -- process the welcome message directly
      do
        mls <- getMLSState
        when (Set.member mp.sender mls.newMembers) $
          traverse_ (fromWelcome mp.sender) mp.welcome

      -- increment epoch and add new clients
      modifyMLSState $ \mls ->
        mls
          { epoch = epoch mls + 1,
            members = members mls <> newMembers mls,
            newMembers = mempty
          }

      pure r

consumeWelcome :: (HasCallStack) => ClientIdentity -> MessagePackage -> WebSocket -> App ()
consumeWelcome cid mp ws = do
  mls <- getMLSState
  notif <- awaitMatch isWelcomeNotif ws
  event <- notif %. "payload.0"

  shouldMatch (eventSubConv event) (fromMaybe A.Null mls.convId)
  shouldMatch (event %. "from") mp.sender.user
  shouldMatch (event %. "data") (fmap (B8.unpack . Base64.encode) mp.welcome)

  welcome <- event %. "data" & asByteString
  gs <- getClientGroupState cid
  assertBool
    "Existing clients in a conversation should not consume welcomes"
    (isNothing gs.group)
  fromWelcome cid welcome

fromWelcome :: ClientIdentity -> ByteString -> App ()
fromWelcome cid welcome =
  void $
    mlscli
      cid
      [ "group",
        "from-welcome",
        "--group-out",
        "<group-out>",
        "-"
      ]
      (Just welcome)

readWelcome :: FilePath -> IO (Maybe ByteString)
readWelcome fp = runMaybeT $ do
  liftIO (doesFileExist fp) >>= guard
  stat <- liftIO $ getFileStatus fp
  guard $ fileSize stat > 0
  liftIO $ BS.readFile fp

mkBundle :: MessagePackage -> ByteString
mkBundle mp = mp.message <> foldMap mkGroupInfoMessage mp.groupInfo <> fold mp.welcome

mkGroupInfoMessage :: ByteString -> ByteString
mkGroupInfoMessage gi = BS.pack [0x00, 0x01, 0x00, 0x04] <> gi

spawn :: (HasCallStack) => CreateProcess -> Maybe ByteString -> App ByteString
spawn cp minput = do
  (mout, ex) <- liftIO
    $ withCreateProcess
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
    _ -> assertFailure "Failed spawning process"

getClientGroupState :: (HasCallStack) => ClientIdentity -> App ClientGroupState
getClientGroupState cid = do
  mls <- getMLSState
  pure $ Map.findWithDefault def cid mls.clientGroupState

setClientGroupState :: (HasCallStack) => ClientIdentity -> ClientGroupState -> App ()
setClientGroupState cid g =
  modifyMLSState $ \s ->
    s {clientGroupState = Map.insert cid g (clientGroupState s)}

showMessage :: (HasCallStack) => ClientIdentity -> ByteString -> App Value
showMessage cid msg = do
  bs <- mlscli cid ["show", "message", "-"] (Just msg)
  assertOne (Aeson.decode (BS.fromStrict bs))

readGroupState :: (HasCallStack) => ByteString -> App [(ClientIdentity, Word32)]
readGroupState gs = do
  v :: Value <- assertJust "Could not decode group state" (Aeson.decode (BS.fromStrict gs))
  lnodes <- v %. "group" %. "public_group" %. "treesync" %. "tree" %. "leaf_nodes" & asList
  catMaybes <$$> for (zip lnodes [0 ..]) $ \(el, leafNodeIndex) -> do
    lookupField el "node" >>= \case
      Just lnode -> do
        case lnode of
          Null -> pure Nothing
          _ -> do
            vecb <- lnode %. "payload" %. "credential" %. "credential" %. "Basic" %. "identity" %. "vec"
            vec <- asList vecb
            ws <- BS.pack <$> for vec (\x -> asIntegral @Word8 x)
            [uc, domain] <- pure (C8.split '@' ws)
            [uid, client] <- pure (C8.split ':' uc)
            let cid = ClientIdentity (C8.unpack domain) (C8.unpack uid) (C8.unpack client)
            pure (Just (cid, leafNodeIndex))
      Nothing ->
        pure Nothing

createApplicationMessage ::
  (HasCallStack) =>
  ClientIdentity ->
  String ->
  App MessagePackage
createApplicationMessage cid messageContent = do
  message <-
    mlscli
      cid
      ["message", "--group-in", "<group-in>", messageContent, "--group-out", "<group-out>"]
      Nothing

  pure
    MessagePackage
      { sender = cid,
        message = message,
        welcome = Nothing,
        groupInfo = Nothing
      }

setMLSCiphersuite :: Ciphersuite -> App ()
setMLSCiphersuite suite = modifyMLSState $ \mls -> mls {ciphersuite = suite}

withCiphersuite :: HasCallStack => Ciphersuite -> App a -> App a
withCiphersuite suite action = do
  suite0 <- (.ciphersuite) <$> getMLSState
  setMLSCiphersuiteIO <- appToIOKleisli setMLSCiphersuite
  actionIO <- appToIO action
  liftIO $
    bracket
      (setMLSCiphersuiteIO suite)
      (const (setMLSCiphersuiteIO suite0))
      (const actionIO)

leaveCurrentConv ::
  (HasCallStack) =>
  ClientIdentity ->
  App ()
leaveCurrentConv cid = do
  mls <- getMLSState
  (_, mSubId) <- objSubConv mls.convId
  case mSubId of
    -- FUTUREWORK: implement leaving main conversation as well
    Nothing -> assertFailure "Leaving conversations is not supported"
    Just _ -> do
      void $ leaveSubConversation cid mls.convId >>= getBody 200
      modifyMLSState $ \s ->
        s
          { members = Set.difference mls.members (Set.singleton cid)
          }

getCurrentConv :: HasCallStack => ClientIdentity -> App Value
getCurrentConv cid = do
  mls <- getMLSState
  (conv, mSubId) <- objSubConv mls.convId
  resp <- case mSubId of
    Nothing -> getConversation cid conv
    Just sub -> getSubConversation cid conv sub
  getJSON 200 resp
