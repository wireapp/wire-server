{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module MLS.Util where

import API.Brig
import API.BrigCommon
import API.Galley
import Control.Concurrent.Async hiding (link)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Codensity
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
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
import System.IO.Error (isAlreadyExistsError)
import System.IO.Temp
import System.Posix.Files
import System.Process
import Testlib.Assertions
import Testlib.HTTP
import Testlib.JSON
import Testlib.Prelude
import Testlib.Printing

mkClientIdentity :: (MakesValue u, MakesValue c) => u -> c -> App ClientIdentity
mkClientIdentity u c = do
  (domain, user) <- objQid u
  client <- c %. "id" & asString
  pure $ ClientIdentity {domain = domain, user = user, client = client}

cid2Str :: ClientIdentity -> String
cid2Str cid = cid.user <> ":" <> cid.client <> "@" <> cid.domain

data MessagePackage = MessagePackage
  { sender :: ClientIdentity,
    convId :: ConvId,
    message :: ByteString,
    welcome :: Maybe ByteString,
    groupInfo :: Maybe ByteString
  }

toRandomFile :: ByteString -> App FilePath
toRandomFile bs = do
  p <- randomFileName
  liftIO $ BS.writeFile p bs
  pure p

randomFileName :: App FilePath
randomFileName = do
  bd <- getBaseDir
  (bd </>) . UUID.toString <$> liftIO UUIDV4.nextRandom

mlscli :: (HasCallStack) => Maybe ConvId -> Ciphersuite -> ClientIdentity -> [String] -> Maybe ByteString -> App ByteString
mlscli mConvId cs cid args mbstdin = do
  groupOut <- randomFileName
  let substOut = argSubst "<group-out>" groupOut
  let scheme = csSignatureScheme cs

  gs <- getClientGroupState cid

  substIn <- case flip Map.lookup gs.groups =<< mConvId of
    Nothing -> pure id
    Just groupData -> do
      fn <- toRandomFile groupData
      pure (argSubst "<group-in>" fn)
  store <- case Map.lookup scheme gs.keystore of
    Nothing -> do
      bd <- getBaseDir
      liftIO (createDirectory (bd </> cid2Str cid))
        `catch` \e ->
          if (isAlreadyExistsError e)
            then assertFailure "client directory for mls state already exists"
            else throwM e

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
    case (groupOutWritten, mConvId) of
      (True, Just convId) -> do
        groupData <- liftIO (BS.readFile groupOut)
        pure $ \x -> x {groups = Map.insert convId groupData x.groups}
      (True, Nothing) -> do
        print $ colored red "mls-test-cli: Group was written but no convId was provided, this probably indicates something is going to go wrong in this test."
        print =<< liftIO (prettierCallStack callStack)
        pure id
      _ -> pure id
  setStore <- do
    storeData <- liftIO (BS.readFile store)
    pure $ \x -> x {keystore = Map.insert scheme storeData x.keystore}

  setClientGroupState cid (setGroup (setStore gs))

  pure out

runCli :: (HasCallStack) => FilePath -> [String] -> Maybe ByteString -> App ByteString
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

createWireClient :: (MakesValue u, HasCallStack) => u -> AddClient -> App ClientIdentity
createWireClient u clientArgs = do
  addClient u clientArgs
    >>= getJSON 201
    >>= mkClientIdentity u

data InitMLSClient = InitMLSClient
  { credType :: CredentialType,
    clientArgs :: AddClient
  }

instance Default InitMLSClient where
  def = InitMLSClient {credType = BasicCredentialType, clientArgs = def}

-- | Create new mls client and register with backend.
createMLSClient :: (MakesValue u, HasCallStack) => Ciphersuite -> InitMLSClient -> u -> App ClientIdentity
createMLSClient ciphersuite opts u = do
  cid <- createWireClient u opts.clientArgs
  setClientGroupState cid def {credType = opts.credType}

  -- set public key
  pkey <- mlscli Nothing ciphersuite cid ["public-key"] Nothing
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
uploadNewKeyPackage :: (HasCallStack) => Ciphersuite -> ClientIdentity -> App String
uploadNewKeyPackage suite cid = do
  (kp, ref) <- generateKeyPackage cid suite

  -- upload key package
  bindResponse (uploadKeyPackages cid [kp]) $ \resp ->
    resp.status `shouldMatchInt` 201

  pure ref

generateKeyPackage :: (HasCallStack) => ClientIdentity -> Ciphersuite -> App (ByteString, String)
generateKeyPackage cid suite = do
  kp <- mlscli Nothing suite cid ["key-package", "create", "--ciphersuite", suite.code] Nothing
  ref <- B8.unpack . Base64.encode <$> mlscli Nothing suite cid ["key-package", "ref", "-"] (Just kp)
  fp <- keyPackageFile cid ref
  liftIO $ BS.writeFile fp kp
  pure (kp, ref)

-- | Create conversation and corresponding group.
createNewGroup :: (HasCallStack) => Ciphersuite -> ClientIdentity -> App ConvId
createNewGroup cs cid = do
  conv <- postConversation cid defMLS >>= getJSON 201
  convId <- objConvId conv
  createGroup cs cid convId
  pure convId

-- | Retrieve self conversation and create the corresponding group.
createSelfGroup :: (HasCallStack) => Ciphersuite -> ClientIdentity -> App (String, Value)
createSelfGroup cs cid = do
  conv <- getSelfConversation cid >>= getJSON 200
  convId <- objConvId conv
  groupId <- conv %. "group_id" & asString
  createGroup cs cid convId
  pure (groupId, conv)

createGroup :: Ciphersuite -> ClientIdentity -> ConvId -> App ()
createGroup cs cid convId = do
  let Just groupId = convId.groupId
  modifyMLSState $ \s ->
    let mlsConv =
          MLSConv
            { members = Set.singleton cid,
              newMembers = mempty,
              groupId,
              convId = convId,
              epoch = 0,
              ciphersuite = cs
            }
     in s {convs = Map.insert convId mlsConv s.convs}
  keys <- getMLSPublicKeys cid.qualifiedUserId >>= getJSON 200
  resetClientGroup cs cid groupId convId keys

createSubConv :: (HasCallStack) => Ciphersuite -> ConvId -> ClientIdentity -> String -> App ()
createSubConv cs convId cid subId = do
  sub <- getSubConversation cid convId subId >>= getJSON 200
  subConvId <- objConvId sub
  createGroup cs cid subConvId
  void $ createPendingProposalCommit subConvId cid >>= sendAndConsumeCommitBundle

createOne2OneSubConv :: (HasCallStack, MakesValue keys) => Ciphersuite -> ConvId -> ClientIdentity -> String -> keys -> App ()
createOne2OneSubConv cs convId cid subId keys = do
  sub <- getSubConversation cid convId subId >>= getJSON 200
  subConvId <- objConvId sub
  resetOne2OneGroupGeneric cs cid sub keys
  void $ createPendingProposalCommit subConvId cid >>= sendAndConsumeCommitBundle

resetOne2OneGroup :: (HasCallStack, MakesValue one2OneConv) => Ciphersuite -> ClientIdentity -> one2OneConv -> App ()
resetOne2OneGroup cs cid one2OneConv =
  resetOne2OneGroupGeneric cs cid (one2OneConv %. "conversation") (one2OneConv %. "public_keys")

-- | Useful when keys are to be taken from main conv and the conv here is the subconv
resetOne2OneGroupGeneric :: (HasCallStack, MakesValue conv, MakesValue keys) => Ciphersuite -> ClientIdentity -> conv -> keys -> App ()
resetOne2OneGroupGeneric cs cid conv keys = do
  convId <- objConvId conv
  groupId <- conv %. "group_id" & asString
  modifyMLSState $ \s ->
    let newMLSConv =
          MLSConv
            { members = Set.singleton cid,
              newMembers = mempty,
              groupId = groupId,
              convId = convId,
              epoch = 0,
              ciphersuite = cs
            }
        resetConv old new =
          old
            { groupId = new.groupId,
              convId = new.convId,
              members = new.members,
              newMembers = new.newMembers,
              epoch = new.epoch
            }
     in s {convs = Map.insertWith resetConv convId newMLSConv s.convs}

  resetClientGroup cs cid groupId convId keys

resetClientGroup :: (HasCallStack, MakesValue keys) => Ciphersuite -> ClientIdentity -> String -> ConvId -> keys -> App ()
resetClientGroup cs cid gid convId keys = do
  removalKey <- asByteString $ keys %. ("removal." <> csSignatureScheme cs)
  void $
    mlscli
      (Just convId)
      cs
      cid
      [ "group",
        "create",
        "--removal-key",
        "-",
        "--group-out",
        "<group-out>",
        "--ciphersuite",
        cs.code,
        gid
      ]
      (Just removalKey)

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
createAddCommit :: (HasCallStack) => ClientIdentity -> ConvId -> [Value] -> App MessagePackage
createAddCommit cid convId users = do
  conv <- getMLSConv convId
  kps <- fmap concat . for users $ \user -> do
    bundle <- claimKeyPackages conv.ciphersuite cid user >>= getJSON 200
    unbundleKeyPackages bundle
  createAddCommitWithKeyPackages cid convId kps

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
  ConvId ->
  [(ClientIdentity, ByteString)] ->
  App MessagePackage
createAddCommitWithKeyPackages cid convId clientsAndKeyPackages = do
  bd <- getBaseDir
  welcomeFile <- liftIO $ emptyTempFile bd "welcome"
  giFile <- liftIO $ emptyTempFile bd "gi"
  Just conv <- Map.lookup convId . (.convs) <$> getMLSState

  commit <- runContT (traverse (withTempKeyPackageFile . snd) clientsAndKeyPackages) $ \kpFiles ->
    mlscli
      (Just convId)
      conv.ciphersuite
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
      { convs =
          Map.adjust
            ( \oldConvState ->
                oldConvState {newMembers = Set.fromList (map fst clientsAndKeyPackages)}
            )
            convId
            mls.convs
      }

  welcome <- liftIO $ BS.readFile welcomeFile
  gi <- liftIO $ BS.readFile giFile
  pure $
    MessagePackage
      { sender = cid,
        convId = convId,
        message = commit,
        welcome = Just welcome,
        groupInfo = Just gi
      }

createRemoveCommit :: (HasCallStack) => ClientIdentity -> ConvId -> [ClientIdentity] -> App MessagePackage
createRemoveCommit cid convId targets = do
  bd <- getBaseDir
  welcomeFile <- liftIO $ emptyTempFile bd "welcome"
  giFile <- liftIO $ emptyTempFile bd "gi"

  groupStateMap <- do
    gs <- getClientGroupState cid
    groupData <- assertJust "Group state not initialised" (Map.lookup convId gs.groups)
    Map.fromList <$> readGroupState groupData
  let indices = map (fromMaybe (error "could not find target") . flip Map.lookup groupStateMap) targets

  conv <- getMLSConv convId

  commit <-
    mlscli
      (Just convId)
      conv.ciphersuite
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
        convId = convId,
        message = commit,
        welcome = Just welcome,
        groupInfo = Just gi
      }

createAddProposals :: (HasCallStack) => ConvId -> ClientIdentity -> [Value] -> App [MessagePackage]
createAddProposals convId cid users = do
  Just mls <- Map.lookup convId . (.convs) <$> getMLSState
  bundles <- for users $ (claimKeyPackages mls.ciphersuite cid >=> getJSON 200)
  kps <- concat <$> traverse unbundleKeyPackages bundles
  traverse (createAddProposalWithKeyPackage convId cid) kps

createReInitProposal :: (HasCallStack) => ConvId -> ClientIdentity -> App MessagePackage
createReInitProposal convId cid = do
  conv <- getMLSConv convId
  prop <-
    mlscli
      (Just convId)
      conv.ciphersuite
      cid
      ["proposal", "--group-in", "<group-in>", "--group-out", "<group-out>", "re-init"]
      Nothing
  pure
    MessagePackage
      { sender = cid,
        convId = convId,
        message = prop,
        welcome = Nothing,
        groupInfo = Nothing
      }

createAddProposalWithKeyPackage ::
  ConvId ->
  ClientIdentity ->
  (ClientIdentity, ByteString) ->
  App MessagePackage
createAddProposalWithKeyPackage convId cid (_, kp) = do
  conv <- getMLSConv convId
  prop <- runContT (withTempKeyPackageFile kp) $ \kpFile ->
    mlscli
      (Just convId)
      conv.ciphersuite
      cid
      ["proposal", "--group-in", "<group-in>", "--group-out", "<group-out>", "add", kpFile]
      Nothing
  pure
    MessagePackage
      { sender = cid,
        convId = convId,
        message = prop,
        welcome = Nothing,
        groupInfo = Nothing
      }

createPendingProposalCommit :: (HasCallStack) => ConvId -> ClientIdentity -> App MessagePackage
createPendingProposalCommit convId cid = do
  bd <- getBaseDir
  welcomeFile <- liftIO $ emptyTempFile bd "welcome"
  pgsFile <- liftIO $ emptyTempFile bd "pgs"
  conv <- getMLSConv convId
  commit <-
    mlscli
      (Just convId)
      conv.ciphersuite
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
        convId = convId,
        message = commit,
        welcome = welcome,
        groupInfo = Just pgs
      }

createExternalCommit ::
  (HasCallStack) =>
  ConvId ->
  ClientIdentity ->
  Maybe ByteString ->
  App MessagePackage
createExternalCommit convId cid mgi = do
  bd <- getBaseDir
  giFile <- liftIO $ emptyTempFile bd "gi"
  gi <- case mgi of
    Nothing -> getGroupInfo cid convId >>= getBody 200
    Just v -> pure v
  conv <- getMLSConv convId
  commit <-
    mlscli
      (Just convId)
      conv.ciphersuite
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
      { convs = Map.adjust (\oldConvState -> oldConvState {newMembers = Set.singleton cid}) convId mls.convs
      -- This might be a different client than those that have been in the
      -- group from before.
      }

  newPgs <- liftIO $ BS.readFile giFile
  pure $
    MessagePackage
      { sender = cid,
        convId = convId,
        message = commit,
        welcome = Nothing,
        groupInfo = Just newPgs
      }

data MLSNotificationTag = MLSNotificationMessageTag | MLSNotificationWelcomeTag
  deriving (Show, Eq, Ord)

consumingMessages :: (HasCallStack) => MLSProtocol -> MessagePackage -> Codensity App ()
consumingMessages mlsProtocol mp = Codensity $ \k -> do
  conv <- getMLSConv mp.convId
  -- clients that should receive the message itself
  let oldClients = Set.delete mp.sender conv.members
  -- clients that should receive a welcome message
  let newClients = Set.delete mp.sender conv.newMembers
  -- all clients that should receive some MLS notification, together with the
  -- expected notification tag
  let clients =
        map (,MLSNotificationMessageTag) (toList oldClients)
          <> map (,MLSNotificationWelcomeTag) (toList newClients)

  -- let newUsers =
  --       Set.delete mp.sender.user $
  --         Set.difference
  --           (Set.map (.user) newClients)
  --           (Set.map (.user) oldClients)
  withWebSockets (map fst clients) $ \wss -> do
    r <- k ()

    -- if the conversation is actually MLS (and not mixed), pick one client for
    -- each new user and wait for its join event. In Mixed protocol, the user is
    -- already in the conversation so they do not get a member-join
    -- notification.
    when (mlsProtocol == MLSProtocolMLS) $
      traverse_
        (awaitMatch (\n -> isMemberJoinNotif n))
        ( flip Map.restrictKeys newUsers
            . Map.mapKeys ((.user) . fst)
            . Map.fromList
            . toList
            $ zip clients wss
        )

    -- at this point we know that every new user has been added to the
    -- conversation
    for_ (zip clients wss) $ \((cid, t), ws) -> case t of
      MLSNotificationMessageTag -> void $ consumeMessageNoExternal conv.ciphersuite cid mp ws
      MLSNotificationWelcomeTag -> consumeWelcome cid mp ws
    pure r

consumeMessageWithPredicate :: (HasCallStack) => (Value -> App Bool) -> ConvId -> Ciphersuite -> ClientIdentity -> Maybe MessagePackage -> WebSocket -> App Value
consumeMessageWithPredicate p convId cs cid mmp ws = do
  notif <- awaitMatch p ws
  event <- notif %. "payload.0"

  event %. "qualified_conversation" `shouldMatch` convIdToQidObject convId
  lookupField event "subconv" `shouldMatch` convId.subconvId

  for_ mmp $ \mp -> do
    event %. "from" `shouldMatch` mp.sender.user
    event %. "data" `shouldMatch` (B8.unpack (Base64.encode mp.message))

  msgData <- event %. "data" & asByteString
  _ <- mlsCliConsume convId cs cid msgData
  showMessage cs cid msgData

-- | Get a single MLS message from a websocket and consume it. Return a JSON
-- representation of the message.
consumeMessage :: (HasCallStack) => ConvId -> Ciphersuite -> ClientIdentity -> Maybe MessagePackage -> WebSocket -> App Value
consumeMessage = consumeMessageWithPredicate isNewMLSMessageNotif

-- | like 'consumeMessage' but will not consume a message where the sender is the backend
consumeMessageNoExternal :: (HasCallStack) => Ciphersuite -> ClientIdentity -> MessagePackage -> WebSocket -> App Value
consumeMessageNoExternal cs cid mp = consumeMessageWithPredicate isNewMLSMessageNotifButNoProposal mp.convId cs cid (Just mp)
  where
    -- the backend (correctly) reacts to a commit removing someone from a parent conversation with a
    -- remove proposal, however, we don't want to consume this here
    isNewMLSMessageNotifButNoProposal :: Value -> App Bool
    isNewMLSMessageNotifButNoProposal n = do
      isRelevantNotif <- isNewMLSMessageNotif n &&~ isNotifConvId mp.convId n
      if isRelevantNotif
        then do
          msg <- n %. "payload.0.data" & asByteString >>= showMessage cs cid
          sender <- msg `lookupField` "message.content.sender" `catch` \(_ :: AssertionFailure) -> pure Nothing
          let backendSender = object ["External" .= Number 0]
          pure $ sender /= Just backendSender
        else pure False

mlsCliConsume :: (HasCallStack) => ConvId -> Ciphersuite -> ClientIdentity -> ByteString -> App ByteString
mlsCliConsume convId cs cid msgData =
  mlscli
    (Just convId)
    cs
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
--
-- returns response body of 'postMLSMessage'
sendAndConsumeMessage :: (HasCallStack) => MessagePackage -> App Value
sendAndConsumeMessage mp = lowerCodensity $ do
  consumingMessages MLSProtocolMLS mp
  lift $ postMLSMessage mp.sender mp.message >>= getJSON 201

sendAndConsumeCommitBundle :: (HasCallStack) => MessagePackage -> App Value
sendAndConsumeCommitBundle = sendAndConsumeCommitBundleWithProtocol MLSProtocolMLS

-- | Send an MLS commit bundle, wait for clients to receive it, consume it, and
-- update the test state accordingly.
sendAndConsumeCommitBundleWithProtocol :: (HasCallStack) => MLSProtocol -> MessagePackage -> App Value
sendAndConsumeCommitBundleWithProtocol protocol mp = do
  lowerCodensity $ do
    consumingMessages protocol mp
    lift $ do
      r <- postMLSCommitBundle mp.sender (mkBundle mp) >>= getJSON 201

      -- if the sender is a new member (i.e. it's an external commit), then
      -- process the welcome message directly
      do
        conv <- getMLSConv mp.convId
        when (Set.member mp.sender conv.newMembers) $
          traverse_ (fromWelcome mp.convId conv.ciphersuite mp.sender) mp.welcome

      -- increment epoch and add new clients
      modifyMLSState $ \mls ->
        mls
          { convs =
              Map.adjust
                ( \conv ->
                    conv
                      { epoch = conv.epoch + 1,
                        members = conv.members <> conv.newMembers,
                        newMembers = mempty
                      }
                )
                mp.convId
                mls.convs
          }

      pure r

consumeWelcome :: (HasCallStack) => ClientIdentity -> MessagePackage -> WebSocket -> App ()
consumeWelcome cid mp ws = do
  notif <- awaitMatch isWelcomeNotif ws
  event <- notif %. "payload.0"

  event %. "qualified_conversation" `shouldMatch` convIdToQidObject mp.convId
  lookupField event "subconv" `shouldMatch` mp.convId.subconvId
  event %. "from" `shouldMatch` mp.sender.user
  event %. "data" `shouldMatch` (fmap (B8.unpack . Base64.encode) mp.welcome)

  welcome <- event %. "data" & asByteString
  gs <- getClientGroupState cid
  assertBool
    "Existing clients in a conversation should not consume welcomes"
    (not $ Map.member mp.convId gs.groups)
  conv <- getMLSConv mp.convId
  fromWelcome mp.convId conv.ciphersuite cid welcome

fromWelcome :: ConvId -> Ciphersuite -> ClientIdentity -> ByteString -> App ()
fromWelcome convId cs cid welcome =
  void $
    mlscli
      (Just convId)
      cs
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

showMessage :: (HasCallStack) => Ciphersuite -> ClientIdentity -> ByteString -> App Value
showMessage cs cid msg = do
  bs <- mlscli Nothing cs cid ["show", "message", "-"] (Just msg)
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
  ConvId ->
  ClientIdentity ->
  String ->
  App MessagePackage
createApplicationMessage convId cid messageContent = do
  conv <- getMLSConv convId
  message <-
    mlscli
      (Just convId)
      conv.ciphersuite
      cid
      ["message", "--group-in", "<group-in>", messageContent, "--group-out", "<group-out>"]
      Nothing

  pure
    MessagePackage
      { sender = cid,
        convId = convId,
        message = message,
        welcome = Nothing,
        groupInfo = Nothing
      }

leaveConv ::
  (HasCallStack) =>
  ConvId ->
  ClientIdentity ->
  App ()
leaveConv convId cid = do
  case convId.subconvId of
    -- FUTUREWORK: implement leaving main conversation as well
    Nothing -> assertFailure "Leaving conversations is not supported"
    Just _ -> do
      void $ leaveSubConversation cid convId >>= getBody 200
      modifyMLSState $ \s ->
        s
          { convs = Map.adjust (\conv -> conv {members = Set.delete cid conv.members}) convId s.convs
          }

getConv :: (HasCallStack) => ConvId -> ClientIdentity -> App Value
getConv convId cid = do
  resp <- case convId.subconvId of
    Nothing -> getConversation cid (convIdToQidObject convId)
    Just sub -> getSubConversation cid convId sub
  getJSON 200 resp

getSubConvId :: (MakesValue user, HasCallStack) => user -> ConvId -> String -> App ConvId
getSubConvId user convId subConvName =
  getSubConversation user convId subConvName
    >>= getJSON 200
    >>= objConvId
