{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module MLS.Util where

import API.Brig
import API.Galley
import Control.Concurrent.Async hiding (link)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Cont
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Base64 qualified as Base64
import Data.ByteString.Char8 qualified as B8
import Data.ByteString.Char8 qualified as C8
import Data.Default
import Data.Foldable
import Data.Function
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Data.Text.Encoding qualified as T
import Data.Traversable
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDV4
import GHC.Stack
import System.Directory
import System.Exit
import System.FilePath
import System.IO hiding (print, putStrLn)
import System.IO.Temp
import System.Posix.Files
import System.Process
import Testlib.App
import Testlib.Assertions
import Testlib.Env
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

mlscli :: HasCallStack => ClientIdentity -> [String] -> Maybe ByteString -> App ByteString
mlscli cid args mbstdin = do
  groupOut <- randomFileName
  let substOut = argSubst "<group-out>" groupOut

  gs <- getClientGroupState cid

  substIn <- case gs.group of
    Nothing -> pure id
    Just groupData -> do
      fn <- toRandomFile groupData
      pure (argSubst "<group-in>" fn)
  store <- maybe randomFileName toRandomFile gs.keystore

  let args' = map (substIn . substOut) args
  for_ args' $ \arg ->
    when (arg `elem` ["<group-in>", "<group-out>"]) $
      assertFailure ("Unbound arg: " <> arg)

  out <-
    spawn
      ( proc
          "mls-test-cli"
          ( ["--store", store]
              <> args'
          )
      )
      mbstdin

  setGroup <- do
    groupOutWritten <- liftIO $ doesFileExist groupOut
    if groupOutWritten
      then do
        groupData <- liftIO (BS.readFile groupOut)
        pure $ \x -> x {group = Just groupData}
      else pure id
  setStore <- do
    storeData <- liftIO (BS.readFile store)
    pure $ \x -> x {keystore = Just storeData}

  setClientGroupState cid ((setGroup . setStore) gs)

  pure out

argSubst :: String -> String -> String -> String
argSubst from to_ s =
  if s == from then to_ else s

createWireClient :: (MakesValue u, HasCallStack) => u -> App ClientIdentity
createWireClient u = do
  lpk <- getLastPrekey
  c <- addClient u def {lastPrekey = Just lpk} >>= getJSON 201
  mkClientIdentity u c

initMLSClient :: HasCallStack => ClientIdentity -> App ()
initMLSClient cid = do
  bd <- getBaseDir
  liftIO $ createDirectory (bd </> cid2Str cid)
  void $ mlscli cid ["init", cid2Str cid] Nothing

-- | Create new mls client and register with backend.
createMLSClient :: (MakesValue u, HasCallStack) => u -> App ClientIdentity
createMLSClient u = do
  cid <- createWireClient u
  initMLSClient cid

  -- set public key
  pkey <- mlscli cid ["public-key"] Nothing
  bindResponse
    ( updateClient
        cid
        def
          { mlsPublicKeys =
              Just (object ["ed25519" .= T.decodeUtf8 (Base64.encode pkey)])
          }
    )
    $ \resp -> resp.status `shouldMatchInt` 200
  pure cid

-- | create and upload to backend
uploadNewKeyPackage :: HasCallStack => ClientIdentity -> App String
uploadNewKeyPackage cid = do
  (kp, ref) <- generateKeyPackage cid

  -- upload key package
  bindResponse (uploadKeyPackage cid kp) $ \resp ->
    resp.status `shouldMatchInt` 201

  pure ref

generateKeyPackage :: HasCallStack => ClientIdentity -> App (ByteString, String)
generateKeyPackage cid = do
  mls <- getMLSState
  kp <- mlscli cid ["key-package", "create", "--ciphersuite", mls.ciphersuite.code] Nothing
  ref <- B8.unpack . Base64.encode <$> mlscli cid ["key-package", "ref", "-"] (Just kp)
  fp <- keyPackageFile cid ref
  liftIO $ BS.writeFile fp kp
  pure (kp, ref)

-- | Create conversation and corresponding group.
createNewGroup :: HasCallStack => ClientIdentity -> App (String, Value)
createNewGroup cid = do
  conv <- postConversation cid defMLS >>= getJSON 201
  groupId <- conv %. "group_id" & asString
  convId <- conv %. "qualified_id"
  createGroup cid conv
  pure (groupId, convId)

-- | Retrieve self conversation and create the corresponding group.
createSelfGroup :: HasCallStack => ClientIdentity -> App (String, Value)
createSelfGroup cid = do
  conv <- getSelfConversation cid >>= getJSON 200
  conv %. "epoch" `shouldMatchInt` 0
  groupId <- conv %. "group_id" & asString
  convId <- conv %. "qualified_id"
  createGroup cid conv
  pure (groupId, convId)

createGroup :: MakesValue conv => ClientIdentity -> conv -> App ()
createGroup cid conv = do
  mls <- getMLSState
  case mls.groupId of
    Just _ -> assertFailure "only one group can be created"
    Nothing -> pure ()
  resetGroup cid conv

createSubConv :: ClientIdentity -> String -> App ()
createSubConv cid subId = do
  mls <- getMLSState
  sub <- getSubConversation cid mls.convId subId >>= getJSON 200
  resetGroup cid sub
  void $ createPendingProposalCommit cid >>= sendAndConsumeCommitBundle

resetGroup :: MakesValue conv => ClientIdentity -> conv -> App ()
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
  removalKeyPath <- asks (.removalKeyPath)
  mls <- getMLSState
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

keyPackageFile :: HasCallStack => ClientIdentity -> String -> App FilePath
keyPackageFile cid ref = do
  let ref' = map urlSafe ref
  bd <- getBaseDir
  pure $ bd </> cid2Str cid </> ref'
  where
    urlSafe '+' = '-'
    urlSafe '/' = '_'
    urlSafe c = c

unbundleKeyPackages :: HasCallStack => Value -> App [(ClientIdentity, ByteString)]
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
createAddCommit :: HasCallStack => ClientIdentity -> [Value] -> App MessagePackage
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
  HasCallStack =>
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

createRemoveCommit :: HasCallStack => ClientIdentity -> [ClientIdentity] -> App MessagePackage
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

createAddProposals :: HasCallStack => ClientIdentity -> [Value] -> App [MessagePackage]
createAddProposals cid users = do
  mls <- getMLSState
  bundles <- for users $ (claimKeyPackages mls.ciphersuite cid >=> getJSON 200)
  kps <- concat <$> traverse unbundleKeyPackages bundles
  traverse (createAddProposalWithKeyPackage cid) kps

createReInitProposal :: HasCallStack => ClientIdentity -> App MessagePackage
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

createPendingProposalCommit :: HasCallStack => ClientIdentity -> App MessagePackage
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
  HasCallStack =>
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

-- | Make all member clients consume a given message.
consumeMessage :: HasCallStack => MessagePackage -> App ()
consumeMessage msg = do
  mls <- getMLSState
  for_ (Set.delete msg.sender mls.members) $ \cid ->
    consumeMessage1 cid msg.message

consumeMessage1 :: HasCallStack => ClientIdentity -> ByteString -> App ()
consumeMessage1 cid msg =
  void $
    mlscli
      cid
      [ "consume",
        "--group",
        "<group-in>",
        "--group-out",
        "<group-out>",
        "-"
      ]
      (Just msg)

-- | Send an MLS message and simulate clients receiving it. If the message is a
-- commit, the 'sendAndConsumeCommit' function should be used instead.
sendAndConsumeMessage :: HasCallStack => MessagePackage -> App Value
sendAndConsumeMessage mp = do
  r <- postMLSMessage mp.sender mp.message >>= getJSON 201
  consumeMessage mp
  pure r

-- | Send an MLS commit bundle, simulate clients receiving it, and update the
-- test state accordingly.
sendAndConsumeCommitBundle :: HasCallStack => MessagePackage -> App Value
sendAndConsumeCommitBundle mp = do
  resp <- postMLSCommitBundle mp.sender (mkBundle mp) >>= getJSON 201
  consumeMessage mp
  traverse_ consumeWelcome mp.welcome

  -- increment epoch and add new clients
  modifyMLSState $ \mls ->
    mls
      { epoch = epoch mls + 1,
        members = members mls <> newMembers mls,
        newMembers = mempty
      }

  pure resp

consumeWelcome :: HasCallStack => ByteString -> App ()
consumeWelcome welcome = do
  mls <- getMLSState
  for_ mls.newMembers $ \cid -> do
    gs <- getClientGroupState cid
    assertBool
      "Existing clients in a conversation should not consume welcomes"
      (isNothing gs.group)
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

spawn :: HasCallStack => CreateProcess -> Maybe ByteString -> App ByteString
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

getClientGroupState :: HasCallStack => ClientIdentity -> App ClientGroupState
getClientGroupState cid = do
  mls <- getMLSState
  pure $ Map.findWithDefault emptyClientGroupState cid mls.clientGroupState

setClientGroupState :: HasCallStack => ClientIdentity -> ClientGroupState -> App ()
setClientGroupState cid g =
  modifyMLSState $ \s ->
    s {clientGroupState = Map.insert cid g (clientGroupState s)}

showMessage :: HasCallStack => ClientIdentity -> ByteString -> App Value
showMessage cid msg = do
  bs <- mlscli cid ["show", "message", "-"] (Just msg)
  assertOne (Aeson.decode (BS.fromStrict bs))

readGroupState :: HasCallStack => ByteString -> App [(ClientIdentity, Word32)]
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
  HasCallStack =>
  ClientIdentity ->
  String ->
  App MessagePackage
createApplicationMessage cid messageContent = do
  message <-
    mlscli
      cid
      ["message", "--group", "<group-in>", messageContent]
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
