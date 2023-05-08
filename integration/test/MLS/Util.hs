{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module MLS.Util where

import API.Brig
import API.Galley
import Control.Concurrent.Async hiding (link)
import Control.Monad
import Control.Monad.IO.Class
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

getConvId :: App String
getConvId = do
  mls <- getMLSState
  maybe (assertFailure "Uninitialised test conversation") pure mls.convId

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
mlscli qcid args mbstdin = do
  bd <- getBaseDir
  let cdir = bd </> cid2Str qcid

  groupOut <- randomFileName
  let substOut = argSubst "<group-out>" groupOut

  hasState <- hasClientGroupState qcid
  substIn <-
    if hasState
      then do
        gs <- getClientGroupState qcid
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
    setClientGroupState qcid gs
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

groupFileLink :: HasCallStack => ClientIdentity -> App FilePath
groupFileLink cid = do
  bd <- getBaseDir
  pure $ bd </> cid2Str cid </> "group.latest"

currentGroupFile :: HasCallStack => ClientIdentity -> App FilePath
currentGroupFile = liftIO . getSymbolicLinkTarget <=< groupFileLink

parseGroupFileName :: FilePath -> App (FilePath, Int)
parseGroupFileName fp = do
  let base = takeFileName fp
  (prefix, version) <- case break (== '.') base of
    (p, '.' : v) -> pure (p, v)
    _ -> assertFailure "invalid group file name"
  n <- case reads version of
    [(v, "")] -> pure (v :: Int)
    _ -> assertFailure "could not parse group file version"
  pure $ (prefix, n)

-- sets symlink and creates empty file
nextGroupFile :: HasCallStack => ClientIdentity -> App FilePath
nextGroupFile qcid = do
  bd <- getBaseDir
  link <- groupFileLink qcid
  exists <- liftIO $ doesFileExist link
  base' <-
    if exists
      then -- group file exists, bump version and update link
      do
        (prefix, n) <- parseGroupFileName =<< liftIO (getSymbolicLinkTarget link)
        liftIO $ removeFile link
        pure $ prefix <> "." <> show (n + 1)
      else -- group file does not exist yet, point link to version 0
        pure "group.0"

  let groupFile = bd </> cid2Str qcid </> base'
  liftIO $ createFileLink groupFile link
  pure groupFile

-- | Create conversation and corresponding group.
setupMLSGroup :: HasCallStack => ClientIdentity -> App (String, Value)
setupMLSGroup cid = do
  conv <- bindResponse (postConversation cid (Just cid.client) defMLS) $ \resp -> do
    resp.status `shouldMatchInt` 201
    pure resp.json
  groupId <- conv %. "group_id" & asString
  convId <- conv %. "qualified_id"
  createGroup cid groupId
  pure (groupId, convId)

createGroup :: ClientIdentity -> String -> App ()
createGroup cid gid = do
  mls <- getMLSState
  case mls.groupId of
    Just _ -> assertFailure "only one group can be created"
    Nothing -> pure ()

  groupJSON <- mlscli cid ["group", "create", gid] Nothing
  g <- nextGroupFile cid
  liftIO $ BS.writeFile g groupJSON
  setMLSState $
    mls
      { groupId = Just gid,
        members = Set.singleton cid
      }

keyPackageFile :: HasCallStack => ClientIdentity -> String -> App FilePath
keyPackageFile cid ref = do
  bd <- getBaseDir
  pure $ bd </> cid2Str cid </> ref

bundleKeyPackages :: Value -> App [(ClientIdentity, FilePath)]
bundleKeyPackages bundle = do
  let entryIdentity be = do
        d <- be %. "domain" & asString
        u <- be %. "user" & asString
        c <- be %. "client" & asString
        pure $ ClientIdentity {domain = d, user = u, client = c}

  bundleEntries <- bundle %. "key_packages" & asList
  for bundleEntries $ \be -> do
    kp64 <- be %. "key_package" & asString
    kp <- assertOne . toList . Base64.decode . B8.pack $ kp64
    ref <-
      fmap (B8.unpack . hex . Base64.decodeLenient . B8.pack) $
        be %. "key_package_ref" & asString
    cid <- entryIdentity be
    fn <- keyPackageFile cid ref
    liftIO $ BS.writeFile fn kp
    pure (cid, fn)

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
    bundleKeyPackages bundle
  createAddCommitWithKeyPackages cid kps

createAddCommitWithKeyPackages ::
  ClientIdentity ->
  [(ClientIdentity, FilePath)] ->
  App MessagePackage
createAddCommitWithKeyPackages qcid clientsAndKeyPackages = do
  bd <- getBaseDir
  g <- currentGroupFile qcid
  gNew <- nextGroupFile qcid
  welcomeFile <- liftIO $ emptyTempFile bd "welcome"
  giFile <- liftIO $ emptyTempFile bd "pgs"
  commit <-
    mlscli
      qcid
      ( [ "member",
          "add",
          "--group",
          g,
          "--welcome-out",
          welcomeFile,
          "--group-info-out",
          giFile,
          "--group-out",
          gNew
        ]
          <> map snd clientsAndKeyPackages
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
      { sender = qcid,
        message = commit,
        welcome = Just welcome,
        groupInfo = Just gi
      }

createAddProposalWithKeyPackage ::
  ClientIdentity ->
  (ClientIdentity, FilePath) ->
  App MessagePackage
createAddProposalWithKeyPackage cid (_, kp) = do
  g <- currentGroupFile cid
  gNew <- nextGroupFile cid
  prop <-
    mlscli
      cid
      ["proposal", "--group-in", g, "--group-out", gNew, "add", kp]
      Nothing
  pure
    MessagePackage
      { sender = cid,
        message = prop,
        welcome = Nothing,
        groupInfo = Nothing
      }

-- | Make all member clients consume a given message.
consumeMessage :: HasCallStack => MessagePackage -> App ()
consumeMessage msg = do
  mls <- getMLSState
  for_ (Set.delete msg.sender mls.members) $ \cid ->
    consumeMessage1 cid msg.message

consumeMessage1 :: HasCallStack => ClientIdentity -> ByteString -> App ()
consumeMessage1 cid msg = do
  bd <- getBaseDir
  g <- currentGroupFile cid
  gNew <- nextGroupFile cid
  void $
    mlscli
      cid
      [ "consume",
        "--group",
        g,
        "--group-out",
        gNew,
        "--signer-key",
        bd </> "removal.key",
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
