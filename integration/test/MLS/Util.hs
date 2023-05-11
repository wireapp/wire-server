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
import Data.Aeson (Value, object)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as B8
import Data.Default
import Data.Foldable
import Data.Function
import Data.Hex
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text.Encoding as T
import Data.Traversable
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import GHC.Stack
import System.Directory
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Posix.Files
import System.Process
import Testlib.App
import Testlib.Assertions
import Testlib.Env
import Testlib.HTTP
import Testlib.JSON
import Testlib.Types

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
  bd <- getBaseDir
  let cdir = bd </> cid2Str cid

  groupOut <- randomFileName
  let substOut = argSubst "<group-out>" groupOut

  hasState <- hasClientGroupState cid
  substIn <-
    if hasState
      then do
        gs <- getClientGroupState cid
        fn <- toRandomFile gs
        pure (argSubst "<group-in>" fn)
      else pure id

  out <-
    spawn
      ( proc
          "mls-test-cli"
          ( ["--store", cdir </> "store"]
              <> map (substIn . substOut) args
          )
      )
      mbstdin

  groupOutWritten <- liftIO $ doesFileExist groupOut
  when groupOutWritten $ do
    gs <- liftIO (BS.readFile groupOut)
    setClientGroupState cid gs
  pure out

argSubst :: String -> String -> String -> String
argSubst from to_ s =
  if s == from then to_ else s

createWireClient :: (MakesValue u, HasCallStack) => u -> App ClientIdentity
createWireClient u = do
  lpk <- getLastPrekey
  c <- addClient u def {lastPrekey = Just lpk}
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
  kp <- mlscli cid ["key-package", "create"] Nothing
  ref <- B8.unpack . hex <$> mlscli cid ["key-package", "ref", "-"] (Just kp)
  fp <- keyPackageFile cid ref
  liftIO $ BS.writeFile fp kp
  pure (kp, ref)

-- | Create conversation and corresponding group.
setupMLSGroup :: HasCallStack => ClientIdentity -> App (String, Value)
setupMLSGroup cid = do
  conv <- bindResponse (postConversation cid (Just cid.client) defMLS) $ \resp -> do
    resp.status `shouldMatchInt` 201
    pure resp.json
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

resetGroup :: MakesValue conv => ClientIdentity -> conv -> App ()
resetGroup cid conv = do
  convId <- make conv
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
  groupJSON <-
    mlscli
      cid
      [ "group",
        "create",
        "--removal-key",
        removalKeyPath,
        gid
      ]
      Nothing
  setClientGroupState cid groupJSON

keyPackageFile :: HasCallStack => ClientIdentity -> String -> App FilePath
keyPackageFile cid ref = do
  bd <- getBaseDir
  pure $ bd </> cid2Str cid </> ref

unbundleKeyPackages :: Value -> App [(ClientIdentity, ByteString)]
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
  kps <- fmap concat . for users $ \user -> do
    bundle <- bindResponse (claimKeyPackages cid user) $ \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json
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

createAddProposals :: HasCallStack => ClientIdentity -> [Value] -> App [MessagePackage]
createAddProposals cid users = do
  bundles <- for users $ \u -> bindResponse (claimKeyPackages cid u) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json
  kps <- concat <$> traverse unbundleKeyPackages bundles
  traverse (createAddProposalWithKeyPackage cid) kps

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
    Nothing -> bindResponse (getGroupInfo cid conv) $ \resp -> do
      resp.status `shouldMatchInt` 200
      pure resp.body
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
  r <- bindResponse (postMLSMessage mp.sender mp.message) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json
  consumeMessage mp
  pure r

-- | Send an MLS commit bundle, simulate clients receiving it, and update the
-- test state accordingly.
sendAndConsumeCommitBundle :: HasCallStack => MessagePackage -> App Value
sendAndConsumeCommitBundle mp = do
  resp <- bindResponse (postMLSCommitBundle mp.sender (mkBundle mp)) $ \resp -> do
    resp.status `shouldMatchInt` 201
    resp.json
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
    hasState <- hasClientGroupState cid
    assertBool "Existing clients in a conversation should not consume welcomes" (not hasState)
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

hasClientGroupState :: HasCallStack => ClientIdentity -> App Bool
hasClientGroupState cid = do
  mls <- getMLSState
  pure $ Map.member cid mls.clientGroupState

getClientGroupState :: HasCallStack => ClientIdentity -> App ByteString
getClientGroupState cid = do
  mls <- getMLSState
  case Map.lookup cid mls.clientGroupState of
    Nothing -> assertFailure ("Attempted to get non-existing group state for client " <> cid2Str cid)
    Just g -> pure g

setClientGroupState :: HasCallStack => ClientIdentity -> ByteString -> App ()
setClientGroupState cid g =
  modifyMLSState $ \s ->
    s {clientGroupState = Map.insert cid g (clientGroupState s)}
