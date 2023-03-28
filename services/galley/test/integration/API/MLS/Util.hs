{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module API.MLS.Util where

import API.Util
import Bilge
import Bilge.Assert
import Control.Arrow ((&&&))
import Control.Error.Util
import Control.Lens (preview, to, view, (.~), (^..))
import Control.Monad.Catch
import Control.Monad.State (StateT, evalStateT)
import qualified Control.Monad.State as State
import Control.Monad.Trans.Maybe
import Crypto.PubKey.Ed25519
import Data.Aeson.Lens
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64U
import Data.ByteString.Conversion
import qualified Data.ByteString.Lazy as LBS
import Data.Domain
import Data.Hex
import Data.Id
import Data.Json.Util hiding ((#))
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock (getCurrentTime)
import qualified Data.Tuple.Extra as Tuple
import Galley.Keys
import Galley.Options
import qualified Galley.Options as Opts
import Imports hiding (getSymbolicLinkTarget)
import System.Directory (getSymbolicLinkTarget)
import System.FilePath
import System.IO.Temp
import System.Posix hiding (createDirectory)
import System.Process
import Test.QuickCheck (arbitrary, generate)
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Conversation
import Wire.API.Conversation.Action
import Wire.API.Conversation.Protocol
import Wire.API.Conversation.Role (roleNameWireMember)
import Wire.API.Event.Conversation
import Wire.API.Federation.API.Galley
import Wire.API.MLS.CipherSuite
import Wire.API.MLS.CommitBundle
import Wire.API.MLS.Credential
import Wire.API.MLS.GroupInfoBundle
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Keys
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation
import Wire.API.User.Client
import Wire.API.User.Client.Prekey

cid2Str :: ClientIdentity -> String
cid2Str cid =
  show (ciUser cid)
    <> ":"
    <> T.unpack (client . ciClient $ cid)
    <> "@"
    <> T.unpack (domainText (ciDomain cid))

mapRemoteKeyPackageRef ::
  (MonadIO m, MonadHttp m, MonadCatch m) =>
  (Request -> Request) ->
  KeyPackageBundle ->
  m ()
mapRemoteKeyPackageRef brig bundle =
  void $
    put
      ( brig
          . paths ["i", "mls", "key-package-refs"]
          . json bundle
      )
      !!! const 204 === statusCode

postMessage ::
  ( HasCallStack,
    MonadIO m,
    MonadHttp m,
    HasGalley m
  ) =>
  ClientIdentity ->
  ByteString ->
  m ResponseLBS
postMessage sender msg = do
  galley <- viewGalley
  post
    ( galley
        . paths ["mls", "messages"]
        . zUser (ciUser sender)
        . zClient (ciClient sender)
        . zConn "conn"
        . content "message/mls"
        . bytes msg
    )

postCommitBundle ::
  ( HasCallStack,
    MonadIO m,
    MonadHttp m,
    HasGalley m
  ) =>
  ClientIdentity ->
  ByteString ->
  m ResponseLBS
postCommitBundle sender bundle = do
  galley <- viewGalley
  post
    ( galley
        . paths ["mls", "commit-bundles"]
        . zUser (ciUser sender)
        . zClient (ciClient sender)
        . zConn "conn"
        . content "application/x-protobuf"
        . bytes bundle
    )

-- FUTUREWORK: remove this and start using commit bundles everywhere in tests
postWelcome ::
  ( MonadIO m,
    MonadHttp m,
    MonadReader TestSetup m,
    HasCallStack
  ) =>
  UserId ->
  ByteString ->
  m ResponseLBS
postWelcome uid welcome = do
  galley <- view tsUnversionedGalley
  post
    ( galley
        . paths ["v2", "mls", "welcome"]
        . zUser uid
        . zConn "conn"
        . content "message/mls"
        . bytes welcome
    )

mkAppAckProposalMessage ::
  GroupId ->
  Epoch ->
  KeyPackageRef ->
  [MessageRange] ->
  SecretKey ->
  PublicKey ->
  Message 'MLSPlainText
mkAppAckProposalMessage gid epoch ref mrs priv pub = do
  let tbs =
        mkRawMLS $
          MessageTBS
            { tbsMsgFormat = KnownFormatTag,
              tbsMsgGroupId = gid,
              tbsMsgEpoch = epoch,
              tbsMsgAuthData = mempty,
              tbsMsgSender = MemberSender ref,
              tbsMsgPayload = ProposalMessage (mkAppAckProposal mrs)
            }
      sig = BA.convert $ sign priv pub (rmRaw tbs)
   in Message tbs (MessageExtraFields sig Nothing Nothing)

saveRemovalKey :: FilePath -> TestM ()
saveRemovalKey fp = do
  keys <- fromJust <$> view (tsGConf . optSettings . setMlsPrivateKeyPaths)
  keysByPurpose <- liftIO $ loadAllMLSKeys keys
  let (_, pub) = fromJust (mlsKeyPair_ed25519 (keysByPurpose RemovalPurpose))
  liftIO $ BS.writeFile fp (BA.convert pub)

data MLSState = MLSState
  { mlsBaseDir :: FilePath,
    -- | for creating clients
    mlsUnusedPrekeys :: [LastPrekey],
    mlsMembers :: Set ClientIdentity,
    -- | users expected to receive a welcome message after the next commit
    mlsNewMembers :: Set ClientIdentity,
    mlsGroupId :: Maybe GroupId,
    mlsEpoch :: Word64
  }

newtype MLSTest a = MLSTest {unMLSTest :: StateT MLSState TestM a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadThrow,
      MonadHttp,
      MonadIO,
      MonadCatch,
      MonadFail,
      MonadMask,
      State.MonadState MLSState,
      MonadReader TestSetup
    )

instance HasGalley MLSTest where
  viewGalley = MLSTest $ lift viewGalley
  viewGalleyOpts = MLSTest $ lift viewGalleyOpts

instance HasBrig MLSTest where
  viewBrig = MLSTest $ lift viewBrig

instance HasSettingsOverrides MLSTest where
  withSettingsOverrides f (MLSTest action) = MLSTest $
    State.StateT $ \s ->
      withSettingsOverrides f (State.runStateT action s)

liftTest :: TestM a -> MLSTest a
liftTest = MLSTest . lift

runMLSTest :: MLSTest a -> TestM a
runMLSTest (MLSTest m) =
  withSystemTempDirectory "mls" $ \tmp -> do
    saveRemovalKey (tmp </> "removal.key")
    evalStateT
      m
      MLSState
        { mlsBaseDir = tmp,
          mlsUnusedPrekeys = someLastPrekeys,
          mlsMembers = mempty,
          mlsNewMembers = mempty,
          mlsGroupId = Nothing,
          mlsEpoch = 0
        }

data MessagePackage = MessagePackage
  { mpSender :: ClientIdentity,
    mpMessage :: ByteString,
    mpWelcome :: Maybe ByteString,
    mpPublicGroupState :: Maybe ByteString
  }

takeLastPrekeyNG :: HasCallStack => MLSTest LastPrekey
takeLastPrekeyNG = do
  s <- State.get
  case mlsUnusedPrekeys s of
    (pk : pks) -> do
      State.modify (\s' -> s' {mlsUnusedPrekeys = pks})
      pure pk
    [] -> error "no prekeys left"

mlscli :: HasCallStack => ClientIdentity -> [String] -> Maybe ByteString -> MLSTest ByteString
mlscli qcid args mbstdin = do
  bd <- State.gets mlsBaseDir
  let cdir = bd </> cid2Str qcid
  liftIO $ do
    spawn (proc "mls-test-cli" (["--store", cdir </> "store"] <> args)) mbstdin

createWireClient :: HasCallStack => Qualified UserId -> MLSTest ClientIdentity
createWireClient qusr = do
  lpk <- takeLastPrekeyNG
  clientId <- liftTest $ randomClient (qUnqualified qusr) lpk
  pure $ mkClientIdentity qusr clientId

initMLSClient :: HasCallStack => ClientIdentity -> MLSTest ()
initMLSClient cid = do
  bd <- State.gets mlsBaseDir
  createDirectory $ bd </> cid2Str cid
  void $ mlscli cid ["init", cid2Str cid] Nothing

createLocalMLSClient :: Local UserId -> MLSTest ClientIdentity
createLocalMLSClient (tUntagged -> qusr) = do
  qcid <- createWireClient qusr
  initMLSClient qcid

  -- set public key
  pkey <- mlscli qcid ["public-key"] Nothing
  brig <- viewBrig
  let update = defUpdateClient {updateClientMLSPublicKeys = Map.singleton Ed25519 pkey}
  put
    ( brig
        . paths ["clients", toByteString' . ciClient $ qcid]
        . zUser (ciUser qcid)
        . json update
    )
    !!! const 200 === statusCode
  pure qcid

-- | Create new mls client and register with backend. If the user is remote,
-- this only creates a fake client (see 'createFakeMLSClient').
createMLSClient :: HasCallStack => Qualified UserId -> MLSTest ClientIdentity
createMLSClient qusr = do
  loc <- liftTest $ qualifyLocal ()
  foldQualified loc createLocalMLSClient (createFakeMLSClient . tUntagged) qusr

-- | Like 'createMLSClient', but do not actually register client with backend.
createFakeMLSClient :: HasCallStack => Qualified UserId -> MLSTest ClientIdentity
createFakeMLSClient qusr = do
  c <- liftIO $ generate arbitrary
  let cid = mkClientIdentity qusr c
  initMLSClient cid
  pure cid

-- | create and upload to backend
uploadNewKeyPackage :: HasCallStack => ClientIdentity -> MLSTest KeyPackageRef
uploadNewKeyPackage qcid = do
  (kp, _) <- generateKeyPackage qcid

  -- upload key package
  brig <- viewBrig
  post
    ( brig
        . paths ["mls", "key-packages", "self", toByteString' . ciClient $ qcid]
        . zUser (ciUser qcid)
        . json (KeyPackageUpload [kp])
    )
    !!! const 201 === statusCode
  pure $ fromJust (kpRef' kp)

generateKeyPackage :: HasCallStack => ClientIdentity -> MLSTest (RawMLS KeyPackage, KeyPackageRef)
generateKeyPackage qcid = do
  kp <- liftIO . decodeMLSError =<< mlscli qcid ["key-package", "create"] Nothing
  let ref = fromJust (kpRef' kp)
  fp <- keyPackageFile qcid ref
  liftIO $ BS.writeFile fp (rmRaw kp)
  pure (kp, ref)

groupFileLink :: HasCallStack => ClientIdentity -> MLSTest FilePath
groupFileLink qcid = State.gets $ \mls ->
  mlsBaseDir mls </> cid2Str qcid </> "group.latest"

currentGroupFile :: HasCallStack => ClientIdentity -> MLSTest FilePath
currentGroupFile = liftIO . getSymbolicLinkTarget <=< groupFileLink

parseGroupFileName :: FilePath -> IO (FilePath, Int)
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
nextGroupFile :: HasCallStack => ClientIdentity -> MLSTest FilePath
nextGroupFile qcid = do
  bd <- State.gets mlsBaseDir
  link <- groupFileLink qcid
  exists <- doesFileExist link
  base' <-
    liftIO $
      if exists
        then -- group file exists, bump version and update link
        do
          (prefix, n) <- parseGroupFileName =<< getSymbolicLinkTarget link
          removeFile link
          pure $ prefix <> "." <> show (n + 1)
        else -- group file does not exist yet, point link to version 0
          pure "group.0"

  let groupFile = bd </> cid2Str qcid </> base'
  createFileLink groupFile link
  pure groupFile

rollBackClient :: HasCallStack => ClientIdentity -> MLSTest ByteString
rollBackClient cid = do
  link <- groupFileLink cid
  groupFile <- liftIO $ getSymbolicLinkTarget link
  (prefix, n) <-
    liftIO $ parseGroupFileName groupFile
  when (n == 0) $ do
    liftIO . assertFailure $ "Cannot roll back client " <> cid2Str cid
  state <- liftIO $ BS.readFile groupFile
  removeFile groupFile
  removeFile link
  bd <- State.gets mlsBaseDir
  let newGroupFile = bd </> cid2Str cid </> (prefix <> "." <> show (n - 1))
  createFileLink newGroupFile link
  pure state

setGroupState :: HasCallStack => ClientIdentity -> ByteString -> MLSTest ()
setGroupState cid state = do
  fp <- nextGroupFile cid
  liftIO $ BS.writeFile fp state

-- | Create a conversation from a provided action and then create a
-- corresponding group.
setupMLSGroupWithConv ::
  HasCallStack =>
  MLSTest Conversation ->
  ClientIdentity ->
  MLSTest (GroupId, Qualified ConvId)
setupMLSGroupWithConv convAction creator = do
  ownDomain <- liftTest viewFederationDomain
  liftIO $ assertEqual "creator is not local" (ciDomain creator) ownDomain
  conv <- convAction
  let groupId =
        fromJust
          (preview (to cnvProtocol . _ProtocolMLS . to cnvmlsGroupId) conv)

  createGroup creator groupId
  pure (groupId, cnvQualifiedId conv)

-- | Create conversation and corresponding group.
setupMLSGroup :: HasCallStack => ClientIdentity -> MLSTest (GroupId, Qualified ConvId)
setupMLSGroup creator = setupMLSGroupWithConv action creator
  where
    action =
      responseJsonError
        =<< liftTest
          ( postConvQualified
              (ciUser creator)
              (Just (ciClient creator))
              defNewMLSConv
          )
          <!! const 201 === statusCode

-- | Create self-conversation and corresponding group.
setupMLSSelfGroup :: HasCallStack => ClientIdentity -> MLSTest (GroupId, Qualified ConvId)
setupMLSSelfGroup creator = setupMLSGroupWithConv action creator
  where
    action =
      responseJsonError
        =<< liftTest
          (getSelfConv (ciUser creator))
          <!! const 200 === statusCode

createGroup :: ClientIdentity -> GroupId -> MLSTest ()
createGroup cid gid = do
  State.gets mlsGroupId >>= \case
    Just _ -> liftIO $ assertFailure "only one group can be created"
    Nothing -> pure ()

  groupJSON <- mlscli cid ["group", "create", T.unpack (toBase64Text (unGroupId gid))] Nothing
  g <- nextGroupFile cid
  liftIO $ BS.writeFile g groupJSON
  State.modify $ \s ->
    s
      { mlsGroupId = Just gid,
        mlsMembers = Set.singleton cid
      }

-- | Create a local group only without a conversation. This simulates creating
-- an MLS conversation on a remote backend.
setupFakeMLSGroup :: ClientIdentity -> MLSTest (GroupId, Qualified ConvId)
setupFakeMLSGroup creator = do
  groupId <-
    liftIO $
      fmap (GroupId . BS.pack) (replicateM 32 (generate arbitrary))
  createGroup creator groupId
  qcnv <- randomQualifiedId (ciDomain creator)
  pure (groupId, qcnv)

keyPackageFile :: HasCallStack => ClientIdentity -> KeyPackageRef -> MLSTest FilePath
keyPackageFile qcid ref =
  State.gets $ \mls ->
    mlsBaseDir mls
      </> cid2Str qcid
      </> T.unpack (T.decodeUtf8 (hex (unKeyPackageRef ref)))

claimLocalKeyPackages :: HasCallStack => ClientIdentity -> Local UserId -> MLSTest KeyPackageBundle
claimLocalKeyPackages qcid lusr = do
  brig <- viewBrig
  responseJsonError
    =<< post
      ( brig
          . paths ["mls", "key-packages", "claim", toByteString' (tDomain lusr), toByteString' (tUnqualified lusr)]
          . zUser (ciUser qcid)
      )
      <!! const 200 === statusCode

-- | Get all test clients of a user by listing the temporary MLS directory.
getUserClients :: HasCallStack => Qualified UserId -> MLSTest [ClientIdentity]
getUserClients qusr = do
  bd <- State.gets mlsBaseDir
  files <- getDirectoryContents bd
  let toClient f = do
        cid <- hush . decodeMLS' . T.encodeUtf8 . T.pack $ f
        guard (cidQualifiedUser cid == qusr)
        pure cid
  pure . mapMaybe toClient $ files

-- | Generate one key package for each client of a remote user
claimRemoteKeyPackages :: HasCallStack => Remote UserId -> MLSTest KeyPackageBundle
claimRemoteKeyPackages (tUntagged -> qusr) = do
  brig <- viewBrig
  clients <- getUserClients qusr
  bundle <- fmap (KeyPackageBundle . Set.fromList) $
    for clients $ \cid -> do
      (kp, ref) <- generateKeyPackage cid
      pure $
        KeyPackageBundleEntry
          { kpbeUser = qusr,
            kpbeClient = ciClient cid,
            kpbeRef = ref,
            kpbeKeyPackage = KeyPackageData (rmRaw kp)
          }
  mapRemoteKeyPackageRef brig bundle
  pure bundle

-- | Claim key package for a local user, or generate and map key packages for remote ones.
claimKeyPackages ::
  HasCallStack =>
  ClientIdentity ->
  Qualified UserId ->
  MLSTest KeyPackageBundle
claimKeyPackages cid qusr = do
  loc <- liftTest $ qualifyLocal ()
  foldQualified loc (claimLocalKeyPackages cid) claimRemoteKeyPackages qusr

bundleKeyPackages :: KeyPackageBundle -> MLSTest [(ClientIdentity, FilePath)]
bundleKeyPackages bundle = do
  let bundleEntries = kpbEntries bundle
      entryIdentity be = mkClientIdentity (kpbeUser be) (kpbeClient be)
  for (toList bundleEntries) $ \be -> do
    let d = kpData . kpbeKeyPackage $ be
        qcid = entryIdentity be
    fn <- keyPackageFile qcid (kpbeRef be)
    liftIO $ BS.writeFile fn d
    pure (qcid, fn)

-- | Claim keypackages and create a commit/welcome pair on a given client.
-- Note that this alters the state of the group immediately. If we want to test
-- a scenario where the commit is rejected by the backend, we can restore the
-- group to the previous state by using an older version of the group file.
createAddCommit :: HasCallStack => ClientIdentity -> [Qualified UserId] -> MLSTest MessagePackage
createAddCommit cid users = do
  kps <- concat <$> traverse (bundleKeyPackages <=< claimKeyPackages cid) users
  createAddCommitWithKeyPackages cid kps

createExternalCommit ::
  HasCallStack =>
  ClientIdentity ->
  Maybe ByteString ->
  Qualified ConvId ->
  MLSTest MessagePackage
createExternalCommit qcid mpgs qcnv = do
  bd <- State.gets mlsBaseDir
  gNew <- nextGroupFile qcid
  pgsFile <- liftIO $ emptyTempFile bd "pgs"
  pgs <- case mpgs of
    Nothing ->
      LBS.toStrict . fromJust . responseBody
        <$> getGroupInfo (ciUser qcid) qcnv
    Just v -> pure v
  commit <-
    mlscli
      qcid
      [ "external-commit",
        "--group-state-in",
        "-",
        "--group-state-out",
        pgsFile,
        "--group-out",
        gNew
      ]
      (Just pgs)

  State.modify $ \mls ->
    mls
      { mlsNewMembers = Set.singleton qcid -- This might be a different client
      -- than those that have been in the
      -- group from before.
      }

  newPgs <- liftIO $ BS.readFile pgsFile
  pure $
    MessagePackage
      { mpSender = qcid,
        mpMessage = commit,
        mpWelcome = Nothing,
        mpPublicGroupState = Just newPgs
      }

createAddProposals :: HasCallStack => ClientIdentity -> [Qualified UserId] -> MLSTest [MessagePackage]
createAddProposals cid users = do
  kps <- concat <$> traverse (bundleKeyPackages <=< claimKeyPackages cid) users
  traverse (createAddProposalWithKeyPackage cid) kps

-- | Create an application message.
createApplicationMessage ::
  HasCallStack =>
  ClientIdentity ->
  String ->
  MLSTest MessagePackage
createApplicationMessage cid messageContent = do
  groupFile <- currentGroupFile cid
  message <-
    mlscli
      cid
      ["message", "--group", groupFile, messageContent]
      Nothing

  pure $
    MessagePackage
      { mpSender = cid,
        mpMessage = message,
        mpWelcome = Nothing,
        mpPublicGroupState = Nothing
      }

createAddCommitWithKeyPackages ::
  ClientIdentity ->
  [(ClientIdentity, FilePath)] ->
  MLSTest MessagePackage
createAddCommitWithKeyPackages qcid clientsAndKeyPackages = do
  bd <- State.gets mlsBaseDir
  g <- currentGroupFile qcid
  gNew <- nextGroupFile qcid
  welcomeFile <- liftIO $ emptyTempFile bd "welcome"
  pgsFile <- liftIO $ emptyTempFile bd "pgs"
  commit <-
    mlscli
      qcid
      ( [ "member",
          "add",
          "--group",
          g,
          "--welcome-out",
          welcomeFile,
          "--group-state-out",
          pgsFile,
          "--group-out",
          gNew
        ]
          <> map snd clientsAndKeyPackages
      )
      Nothing

  State.modify $ \mls ->
    mls
      { mlsNewMembers = Set.fromList (map fst clientsAndKeyPackages)
      }

  welcome <- liftIO $ BS.readFile welcomeFile
  pgs <- liftIO $ BS.readFile pgsFile
  pure $
    MessagePackage
      { mpSender = qcid,
        mpMessage = commit,
        mpWelcome = Just welcome,
        mpPublicGroupState = Just pgs
      }

createAddProposalWithKeyPackage ::
  ClientIdentity ->
  (ClientIdentity, FilePath) ->
  MLSTest MessagePackage
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
      { mpSender = cid,
        mpMessage = prop,
        mpWelcome = Nothing,
        mpPublicGroupState = Nothing
      }

createPendingProposalCommit :: HasCallStack => ClientIdentity -> MLSTest MessagePackage
createPendingProposalCommit qcid = do
  bd <- State.gets mlsBaseDir
  welcomeFile <- liftIO $ emptyTempFile bd "welcome"
  pgsFile <- liftIO $ emptyTempFile bd "pgs"
  g <- currentGroupFile qcid
  gNew <- nextGroupFile qcid
  commit <-
    mlscli
      qcid
      [ "commit",
        "--group",
        g,
        "--group-out",
        gNew,
        "--welcome-out",
        welcomeFile,
        "--group-state-out",
        pgsFile
      ]
      Nothing

  welcome <- liftIO $ readWelcome welcomeFile
  pgs <- liftIO $ BS.readFile pgsFile
  pure
    MessagePackage
      { mpSender = qcid,
        mpMessage = commit,
        mpWelcome = welcome,
        mpPublicGroupState = Just pgs
      }

readWelcome :: FilePath -> IO (Maybe ByteString)
readWelcome fp = runMaybeT $ do
  liftIO (doesFileExist fp) >>= guard
  stat <- liftIO $ getFileStatus fp
  guard $ fileSize stat > 0
  liftIO $ BS.readFile fp

createRemoveCommit :: HasCallStack => ClientIdentity -> [ClientIdentity] -> MLSTest MessagePackage
createRemoveCommit cid targets = do
  bd <- State.gets mlsBaseDir
  welcomeFile <- liftIO $ emptyTempFile bd "welcome"
  pgsFile <- liftIO $ emptyTempFile bd "pgs"
  g <- currentGroupFile cid
  gNew <- nextGroupFile cid

  kprefByClient <- liftIO $ Map.fromList <$> readGroupState g
  let fetchKeyPackage c = keyPackageFile c (kprefByClient Map.! c)
  kps <- traverse fetchKeyPackage targets

  commit <-
    mlscli
      cid
      ( [ "member",
          "remove",
          "--group",
          g,
          "--group-out",
          gNew,
          "--welcome-out",
          welcomeFile,
          "--group-state-out",
          pgsFile
        ]
          <> kps
      )
      Nothing
  welcome <- liftIO $ readWelcome welcomeFile
  pgs <- liftIO $ BS.readFile pgsFile
  pure
    MessagePackage
      { mpSender = cid,
        mpMessage = commit,
        mpWelcome = welcome,
        mpPublicGroupState = Just pgs
      }

createExternalAddProposal :: HasCallStack => ClientIdentity -> MLSTest MessagePackage
createExternalAddProposal joiner = do
  groupId <-
    State.gets mlsGroupId >>= \case
      Nothing -> liftIO $ assertFailure "Creating add proposal for non-existing group"
      Just g -> pure g
  epoch <- State.gets mlsEpoch
  proposal <-
    mlscli
      joiner
      [ "proposal-external",
        "--group-id",
        T.unpack (toBase64Text (unGroupId groupId)),
        "--epoch",
        show epoch,
        "add"
      ]
      Nothing

  State.modify $ \mls ->
    mls
      { mlsNewMembers = mlsNewMembers mls <> Set.singleton joiner
      }
  pure
    MessagePackage
      { mpSender = joiner,
        mpMessage = proposal,
        mpWelcome = Nothing,
        mpPublicGroupState = Nothing
      }

consumeWelcome :: HasCallStack => ByteString -> MLSTest ()
consumeWelcome welcome = do
  qcids <- State.gets mlsNewMembers
  for_ qcids $ \qcid -> do
    link <- groupFileLink qcid
    liftIO $
      doesFileExist link >>= \e ->
        assertBool "Existing clients in a conversation should not consume commits" (not e)
    groupFile <- nextGroupFile qcid
    void $
      mlscli
        qcid
        [ "group",
          "from-welcome",
          "--group-out",
          groupFile,
          "-"
        ]
        (Just welcome)

-- | Make all member clients consume a given message.
consumeMessage :: HasCallStack => MessagePackage -> MLSTest ()
consumeMessage msg = do
  mems <- State.gets mlsMembers
  for_ (Set.delete (mpSender msg) mems) $ \cid ->
    consumeMessage1 cid (mpMessage msg)

consumeMessage1 :: HasCallStack => ClientIdentity -> ByteString -> MLSTest ()
consumeMessage1 cid msg = do
  bd <- State.gets mlsBaseDir
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
sendAndConsumeMessage :: HasCallStack => MessagePackage -> MLSTest ([Event], UnreachableUserList)
sendAndConsumeMessage mp = do
  res <-
    fmap (mmssEvents Tuple.&&& mmssUnreachableUserList) $
      responseJsonError
        =<< postMessage (mpSender mp) (mpMessage mp)
          <!! const 201 === statusCode
  consumeMessage mp

  for_ (mpWelcome mp) $ \welcome -> do
    postWelcome (ciUser (mpSender mp)) welcome
      !!! const 201 === statusCode
    consumeWelcome welcome

  pure res

-- | Send an MLS commit message, simulate clients receiving it, and update the
-- test state accordingly.
sendAndConsumeCommit ::
  HasCallStack =>
  MessagePackage ->
  MLSTest [Event]
sendAndConsumeCommit mp = do
  (events, _) <- sendAndConsumeMessage mp

  -- increment epoch and add new clients
  State.modify $ \mls ->
    mls
      { mlsEpoch = mlsEpoch mls + 1,
        mlsMembers = mlsMembers mls <> mlsNewMembers mls,
        mlsNewMembers = mempty
      }

  pure events

mkBundle :: MessagePackage -> Either Text CommitBundle
mkBundle mp = do
  commitB <- decodeMLS' (mpMessage mp)
  welcomeB <- traverse decodeMLS' (mpWelcome mp)
  pgs <- note "public group state unavailable" (mpPublicGroupState mp)
  pgsB <- decodeMLS' pgs
  pure $
    CommitBundle commitB welcomeB $
      GroupInfoBundle UnencryptedGroupInfo TreeFull pgsB

createBundle :: MonadIO m => MessagePackage -> m ByteString
createBundle mp = do
  bundle <-
    either (liftIO . assertFailure . T.unpack) pure $
      mkBundle mp
  pure (serializeCommitBundle bundle)

sendAndConsumeCommitBundle ::
  HasCallStack =>
  MessagePackage ->
  MLSTest [Event]
sendAndConsumeCommitBundle mp = do
  bundle <- createBundle mp
  events <-
    fmap mmssEvents
      . responseJsonError
      =<< postCommitBundle (mpSender mp) bundle
        <!! const 201 === statusCode
  consumeMessage mp
  traverse_ consumeWelcome (mpWelcome mp)

  -- increment epoch and add new clients
  State.modify $ \mls ->
    mls
      { mlsEpoch = mlsEpoch mls + 1,
        mlsMembers = mlsMembers mls <> mlsNewMembers mls,
        mlsNewMembers = mempty
      }

  pure events

mlsBracket ::
  HasCallStack =>
  [ClientIdentity] ->
  ([WS.WebSocket] -> MLSTest a) ->
  MLSTest a
mlsBracket clients k = do
  c <- view tsCannon
  WS.bracketAsClientRN c (map (ciUser &&& ciClient) clients) k

readGroupState :: FilePath -> IO [(ClientIdentity, KeyPackageRef)]
readGroupState fp = do
  j <- BS.readFile fp
  pure $ do
    node <- j ^.. key "group" . key "tree" . key "tree" . key "nodes" . _Array . traverse
    leafNode <- node ^.. key "node" . key "LeafNode"
    identity <-
      either (const []) pure . decodeMLS' . BS.pack . map fromIntegral $
        leafNode ^.. key "key_package" . key "payload" . key "credential" . key "credential" . key "Basic" . key "identity" . key "vec" . _Array . traverse . _Integer
    kpr <- (unhexM . T.encodeUtf8 =<<) $ leafNode ^.. key "key_package_ref" . _String
    pure (identity, KeyPackageRef kpr)

getClientsFromGroupState ::
  ClientIdentity ->
  Qualified UserId ->
  MLSTest [(ClientIdentity, KeyPackageRef)]
getClientsFromGroupState cid u = do
  groupFile <- currentGroupFile cid
  groupState <- liftIO $ readGroupState groupFile
  pure $ filter (\(cid', _) -> cidQualifiedUser cid' == u) groupState

clientKeyPair :: ClientIdentity -> MLSTest (ByteString, ByteString)
clientKeyPair cid = do
  bd <- State.gets mlsBaseDir
  credential <-
    liftIO . BS.readFile $
      bd </> cid2Str cid </> "store" </> T.unpack (T.decodeUtf8 (B64U.encode "self"))
  let s =
        credential ^.. key "signature_private_key" . key "value" . _Array . traverse . _Integer
          & fmap fromIntegral
          & BS.pack
  pure $ BS.splitAt 32 s

receiveNewRemoteConv ::
  (MonadReader TestSetup m, MonadIO m) =>
  Qualified ConvId ->
  GroupId ->
  m ()
receiveNewRemoteConv conv gid = do
  client <- view tsFedGalleyClient
  let nrc =
        NewRemoteConversation (qUnqualified conv) $
          ProtocolMLS
            ( ConversationMLSData
                gid
                (Epoch 1)
                MLS_128_DHKEMX25519_AES128GCM_SHA256_Ed25519
            )
  void $
    runFedClient
      @"on-new-remote-conversation"
      client
      (qDomain conv)
      nrc

receiveOnConvUpdated ::
  (MonadReader TestSetup m, MonadIO m) =>
  Qualified ConvId ->
  Qualified UserId ->
  Qualified UserId ->
  m ()
receiveOnConvUpdated conv origUser joiner = do
  client <- view tsFedGalleyClient
  now <- liftIO getCurrentTime
  let cu =
        ConversationUpdate
          { cuTime = now,
            cuOrigUserId = origUser,
            cuConvId = qUnqualified conv,
            cuAlreadyPresentUsers = [qUnqualified joiner],
            cuAction =
              SomeConversationAction
                SConversationJoinTag
                ConversationJoin
                  { cjUsers = pure joiner,
                    cjRole = roleNameWireMember
                  }
          }
  void $
    runFedClient
      @"on-conversation-updated"
      client
      (qDomain conv)
      cu

getGroupInfo ::
  ( HasCallStack,
    MonadIO m,
    MonadHttp m,
    HasGalley m
  ) =>
  UserId ->
  Qualified ConvId ->
  m ResponseLBS
getGroupInfo sender qcnv = do
  galley <- viewGalley
  get
    ( galley
        . paths
          [ "conversations",
            toByteString' (qDomain qcnv),
            toByteString' (qUnqualified qcnv),
            "groupinfo"
          ]
        . zUser sender
        . zConn "conn"
    )

getSelfConv ::
  UserId ->
  TestM ResponseLBS
getSelfConv u = do
  g <- viewGalley
  get $
    g
      . paths ["/conversations", "mls-self"]
      . zUser u
      . zConn "conn"
      . zType "access"

withMLSDisabled :: HasSettingsOverrides m => m a -> m a
withMLSDisabled = withSettingsOverrides noMLS
  where
    noMLS = Opts.optSettings . Opts.setMlsPrivateKeyPaths .~ Nothing
