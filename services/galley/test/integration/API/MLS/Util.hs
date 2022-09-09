{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

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
import Control.Lens (preview, to, view)
import Control.Monad.Catch
import Control.Monad.State (StateT, evalStateT)
import qualified Control.Monad.State as State
import Crypto.PubKey.Ed25519
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.ByteString.Conversion
import Data.Default
import Data.Domain
import Data.Hex
import Data.Id
import Data.Json.Util
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Galley.Keys
import Galley.Options
import Imports
import System.Directory (getSymbolicLinkTarget)
import System.FilePath
import System.IO.Temp
import System.Process
import Test.QuickCheck (arbitrary, generate)
import qualified Test.Tasty.Cannon as WS
import Test.Tasty.HUnit
import TestHelpers
import TestSetup
import Wire.API.Conversation
import Wire.API.Conversation.Protocol
import Wire.API.Event.Conversation
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Keys
import Wire.API.MLS.Message
import Wire.API.MLS.Proposal
import Wire.API.MLS.Serialisation
import Wire.API.User.Client
import Wire.API.User.Client.Prekey

data CreateClients = CreateWithoutKey | CreateWithKey | DontCreateClients
  deriving (Eq)

data CreateConv = CreateConv | CreateProteusConv | DontCreateConv
  deriving (Eq)

data UserOrigin = LocalUser | RemoteUser Domain

createNewConv :: ClientId -> CreateConv -> Maybe NewConv
createNewConv c CreateConv = Just (defNewMLSConv c)
createNewConv _ CreateProteusConv = Just defNewProteusConv
createNewConv _ DontCreateConv = Nothing

data SetupOptions = SetupOptions
  { createClients :: CreateClients,
    creatorOrigin :: UserOrigin,
    createConv :: CreateConv,
    makeConnections :: Bool,
    numCreatorClients :: Int
  }

instance Default SetupOptions where
  def =
    SetupOptions
      { createClients = CreateWithKey,
        creatorOrigin = LocalUser,
        createConv = DontCreateConv,
        makeConnections = True,
        numCreatorClients = 1
      }

data MessagingSetup = MessagingSetup
  { creator :: Participant,
    users :: [Participant],
    conversation :: Qualified ConvId,
    groupId :: GroupId,
    welcome :: ByteString,
    commit :: ByteString
  }
  deriving (Show)

data AddKeyPackage = AddKeyPackage
  { mapKeyPackage :: Bool,
    setPublicKey :: Bool
  }
  deriving (Show)

instance Default AddKeyPackage where
  def =
    AddKeyPackage
      { mapKeyPackage = True,
        setPublicKey = True
      }

cli :: String -> FilePath -> [String] -> CreateProcess
cli store tmp args =
  proc "mls-test-cli" $
    ["--store", tmp </> (store <> ".db")] <> args

data Participant = Participant
  { pUserId :: Qualified UserId,
    pClientIds :: NonEmpty ClientId
  }
  deriving (Show)

userClientQid :: Qualified UserId -> ClientId -> String
userClientQid usr c =
  show (qUnqualified usr)
    <> ":"
    <> T.unpack (client c)
    <> "@"
    <> T.unpack (domainText (qDomain usr))

cid2Str :: ClientIdentity -> String
cid2Str cid =
  show (ciUser cid)
    <> ":"
    <> T.unpack (client . ciClient $ cid)
    <> "@"
    <> T.unpack (domainText (ciDomain cid))

pClients :: Participant -> NonEmpty (String, ClientId)
pClients p =
  pClientIds p <&> \c ->
    (userClientQid (pUserId p) c, c)

pClientQid :: Participant -> String
pClientQid p = userClientQid (pUserId p) (NonEmpty.head (pClientIds p))

pClientId :: Participant -> ClientId
pClientId = NonEmpty.head . pClientIds

readKeyPackages :: FilePath -> Participant -> IO (NonEmpty (ClientId, RawMLS KeyPackage))
readKeyPackages tmp participant = for (pClients participant) $ \(qcid, cid) -> do
  b <- BS.readFile (tmp </> qcid)
  pure (cid, fromRight (error "parsing RawMLS KeyPackage") (decodeMLS' b))

setupUserClient ::
  HasCallStack =>
  FilePath ->
  CreateClients ->
  -- | Whether to claim/map the key package
  Bool ->
  Qualified UserId ->
  State.StateT [LastPrekey] TestM ClientId
setupUserClient tmp doCreateClients mapKeyPackage usr = do
  localDomain <- lift viewFederationDomain
  lpk <- takeLastPrekey
  lift $ do
    -- create client if requested
    c <- case doCreateClients of
      DontCreateClients -> liftIO $ generate arbitrary
      _ -> randomClient (qUnqualified usr) lpk

    let qcid = userClientQid usr c

    -- generate key package
    void . liftIO $ spawn (cli qcid tmp ["init", qcid]) Nothing
    kp <-
      liftIO $
        decodeMLSError
          =<< spawn (cli qcid tmp ["key-package", "create"]) Nothing
    liftIO $ BS.writeFile (tmp </> qcid) (rmRaw kp)

    -- Set Bob's private key and upload key package if required. If a client
    -- does not have to be created and it is remote, pretend to have claimed its
    -- key package.
    case doCreateClients of
      CreateWithKey -> addKeyPackage def {mapKeyPackage = mapKeyPackage} usr c kp
      DontCreateClients | localDomain /= qDomain usr -> do
        brig <- view tsBrig
        let bundle =
              KeyPackageBundle $
                Set.singleton $
                  KeyPackageBundleEntry
                    { kpbeUser = usr,
                      kpbeClient = c,
                      kpbeRef = fromJust $ kpRef' kp,
                      kpbeKeyPackage = KeyPackageData $ rmRaw kp
                    }
        when mapKeyPackage $ mapRemoteKeyPackageRef brig bundle
      DontCreateClients -> pure ()
      CreateWithoutKey -> pure ()

    pure c

setupParticipant ::
  HasCallStack =>
  FilePath ->
  CreateClients ->
  Int ->
  Qualified UserId ->
  State.StateT [LastPrekey] TestM Participant
setupParticipant tmp doCreateClients numClients usr =
  Participant usr . NonEmpty.fromList
    <$> replicateM numClients (setupUserClient tmp doCreateClients True usr)

setupParticipants ::
  HasCallStack =>
  FilePath ->
  SetupOptions ->
  -- | A list of pairs, where each pair represents the number of clients for a
  -- participant other than the group creator and whether the participant is
  -- local or remote.
  [(Int, UserOrigin)] ->
  State.StateT [LastPrekey] TestM (Participant, [Participant])
setupParticipants tmp SetupOptions {..} ns = do
  creator <- do
    u <- lift $ createUserOrId creatorOrigin
    let createCreatorClients = createClientsForUR creatorOrigin createClients
    c0 <- setupUserClient tmp createCreatorClients False u
    cs <- replicateM (numCreatorClients - 1) (setupUserClient tmp createCreatorClients True u)
    pure (Participant u (c0 :| cs))
  others <- for ns $ \(n, ur) -> do
    qusr <- lift (createUserOrId ur)
    participant <- setupParticipant tmp (createClientsForUR ur createClients) n qusr
    pure (participant, ur)
  lift . when makeConnections $ do
    for_ others $ \(o, ur) -> case (creatorOrigin, ur) of
      (LocalUser, LocalUser) ->
        connectUsers (qUnqualified (pUserId creator)) (pure ((qUnqualified . pUserId) o))
      (LocalUser, RemoteUser _) ->
        connectWithRemoteUser
          (qUnqualified . pUserId $ creator)
          (pUserId o)
      (RemoteUser _, LocalUser) ->
        connectWithRemoteUser
          (qUnqualified . pUserId $ o)
          (pUserId creator)
      (RemoteUser _, RemoteUser _) ->
        liftIO $
          assertFailure "Trying to have both the creator and a group participant remote"
  pure (creator, fst <$> others)
  where
    createUserOrId :: UserOrigin -> TestM (Qualified UserId)
    createUserOrId = \case
      LocalUser -> randomQualifiedUser
      RemoteUser d -> randomQualifiedId d

    createClientsForUR LocalUser cc = cc
    createClientsForUR (RemoteUser _) _ = DontCreateClients

withLastPrekeys :: Monad m => State.StateT [LastPrekey] m a -> m a
withLastPrekeys m = State.evalStateT m someLastPrekeys

setupGroup ::
  HasCallStack =>
  FilePath ->
  CreateConv ->
  Participant ->
  String ->
  TestM (GroupId, Qualified ConvId)
setupGroup tmp createConv creator name = do
  (mGroupId, conversation) <- case createNewConv (pClientId creator) createConv of
    Nothing -> pure (Nothing, error "No conversation created")
    Just nc -> do
      conv <-
        responseJsonError =<< postConvQualified (qUnqualified (pUserId creator)) nc
          <!! const 201 === statusCode

      pure (preview (to cnvProtocol . _ProtocolMLS . to cnvmlsGroupId) conv, cnvQualifiedId conv)

  groupId <- case mGroupId of
    Just gid -> pure gid
    -- generate a random group id
    Nothing -> liftIO $ fmap (GroupId . BS.pack) (replicateM 32 (generate arbitrary))

  groupJSON <-
    liftIO $
      spawn
        ( cli
            (pClientQid creator)
            tmp
            ["group", "create", T.unpack (toBase64Text (unGroupId groupId))]
        )
        Nothing
  liftIO $ BS.writeFile (tmp </> name) groupJSON

  pure (groupId, conversation)

setupCommit ::
  (HasCallStack, Foldable f) =>
  String ->
  Participant ->
  String ->
  String ->
  f (String, ClientId) ->
  IO (ByteString, ByteString)
setupCommit tmp admin groupName newGroupName clients =
  (,)
    <$> spawn
      ( cli
          (pClientQid admin)
          tmp
          $ [ "member",
              "add",
              "--group",
              tmp </> groupName,
              "--welcome-out",
              tmp </> "welcome",
              "--group-out",
              tmp </> newGroupName
            ]
            <> map ((tmp </>) . fst) (toList clients)
      )
      Nothing
      <*> BS.readFile (tmp </> "welcome")

setupRemoveCommit ::
  (HasCallStack, Foldable f) =>
  String ->
  Participant ->
  String ->
  String ->
  f (String, ClientId) ->
  IO (ByteString, Maybe ByteString)
setupRemoveCommit tmp admin groupName newGroupName clients = do
  let welcomeFile = tmp </> "welcome"
  commit <-
    spawn
      ( cli
          (pClientQid admin)
          tmp
          $ [ "member",
              "remove",
              "--group",
              tmp </> groupName,
              "--group-out",
              tmp </> newGroupName,
              "--welcome-out",
              welcomeFile
            ]
            <> map ((tmp </>) . fst) (toList clients)
      )
      Nothing
  welcome <-
    doesFileExist welcomeFile >>= \case
      False -> pure Nothing
      True -> Just <$> BS.readFile welcomeFile
  pure (commit, welcome)

mergeWelcome ::
  (HasCallStack) =>
  String ->
  String ->
  String ->
  String ->
  String ->
  IO ()
mergeWelcome tmp clientQid groupIn groupOut welcomeIn =
  void $
    spawn
      ( cli
          clientQid
          tmp
          [ groupIn,
            "from-welcome",
            "--group-out",
            tmp </> groupOut,
            tmp </> welcomeIn
          ]
      )
      Nothing

bareAddProposal ::
  HasCallStack =>
  String ->
  Participant ->
  Participant ->
  String ->
  String ->
  IO ByteString
bareAddProposal tmp creator participantToAdd groupIn groupOut =
  spawn
    ( cli
        (pClientQid creator)
        tmp
        $ [ "proposal",
            "--group-in",
            tmp </> groupIn,
            "--group-out",
            tmp </> groupOut,
            "add",
            tmp </> pClientQid participantToAdd
          ]
    )
    Nothing

pendingProposalsCommit ::
  HasCallStack =>
  String ->
  Participant ->
  String ->
  IO (ByteString, Maybe ByteString)
pendingProposalsCommit tmp creator groupName = do
  let welcomeFile = tmp </> "welcome"
  commit <-
    spawn
      ( cli
          (pClientQid creator)
          tmp
          $ [ "commit",
              "--group",
              tmp </> groupName,
              "--welcome-out",
              welcomeFile
            ]
      )
      Nothing
  welcome <-
    doesFileExist welcomeFile >>= \case
      False -> pure Nothing
      True -> Just <$> BS.readFile welcomeFile
  pure (commit, welcome)

createExternalProposal ::
  HasCallStack =>
  String ->
  String ->
  String ->
  String ->
  IO ByteString
createExternalProposal tmp creatorClientQid groupIn groupOut = do
  spawn
    ( cli
        creatorClientQid
        tmp
        $ [ "external-proposal",
            "--group-in",
            tmp </> groupIn,
            "--group-out",
            tmp </> groupOut,
            "add"
          ]
    )
    Nothing

createMessage ::
  HasCallStack =>
  String ->
  Participant ->
  String ->
  String ->
  IO ByteString
createMessage tmp sender groupName msgText =
  spawn (cli (pClientQid sender) tmp ["message", "--group", tmp </> groupName, msgText]) Nothing

takeLastPrekey :: MonadFail m => State.StateT [LastPrekey] m LastPrekey
takeLastPrekey = do
  (lpk : lpks) <- State.get
  State.put lpks
  pure lpk

-- | Setup: Alice creates a group and invites Bob that is local or remote to
-- Alice depending on the passed in creator origin. Return welcome and commit
-- message.
aliceInvitesBob :: HasCallStack => (Int, UserOrigin) -> SetupOptions -> TestM MessagingSetup
aliceInvitesBob bobConf opts = withSystemTempDirectory "mls" $ \tmp ->
  aliceInvitesBobWithTmp tmp bobConf opts

aliceInvitesBobWithTmp ::
  HasCallStack =>
  FilePath ->
  (Int, UserOrigin) ->
  SetupOptions ->
  TestM MessagingSetup
aliceInvitesBobWithTmp tmp bobConf opts@SetupOptions {..} = do
  (alice, [bob]) <- withLastPrekeys $ setupParticipants tmp opts [bobConf]
  -- create a group
  (groupId, conversation) <- setupGroup tmp createConv alice "group"

  -- add clients to it and get welcome message
  (commit, welcome) <-
    liftIO $
      setupCommit tmp alice "group" "group" $
        NonEmpty.tail (pClients alice) <> toList (pClients bob)

  pure $
    MessagingSetup
      { creator = alice,
        users = [bob],
        ..
      }

addKeyPackage ::
  HasCallStack =>
  AddKeyPackage ->
  Qualified UserId ->
  ClientId ->
  RawMLS KeyPackage ->
  TestM ()
addKeyPackage AddKeyPackage {..} u c kp = do
  brig <- view tsBrig

  when setPublicKey $ do
    -- set public key
    let update = defUpdateClient {updateClientMLSPublicKeys = Map.singleton Ed25519 (bcSignatureKey (kpCredential (rmValue kp)))}
    put
      ( brig
          . paths ["clients", toByteString' c]
          . zUser (qUnqualified u)
          . json update
      )
      !!! const 200 === statusCode

  -- upload key package
  post
    ( brig
        . paths ["mls", "key-packages", "self", toByteString' c]
        . zUser (qUnqualified u)
        . json (KeyPackageUpload [kp])
    )
    !!! const 201 === statusCode

  when mapKeyPackage $
    -- claim key package (technically, some other user should claim them, but it doesn't really make a difference)
    post
      ( brig
          . paths ["mls", "key-packages", "claim", toByteString' (qDomain u), toByteString' (qUnqualified u)]
          . zUser (qUnqualified u)
      )
      !!! const 200 === statusCode

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

claimKeyPackage :: (MonadIO m, MonadHttp m) => (Request -> Request) -> UserId -> Qualified UserId -> m ResponseLBS
claimKeyPackage brig claimant target =
  post
    ( brig
        . paths ["mls", "key-packages", "claim", toByteString' (qDomain target), toByteString' (qUnqualified target)]
        . zUser claimant
    )

postCommit :: HasCallStack => MessagingSetup -> TestM [Event]
postCommit MessagingSetup {..} =
  fmap mmssEvents . responseJsonError
    =<< postMessage (qUnqualified (pUserId creator)) commit
      <!! const 201 === statusCode

postMessage ::
  ( HasCallStack,
    MonadIO m,
    MonadCatch m,
    MonadThrow m,
    MonadHttp m,
    HasGalley m
  ) =>
  UserId ->
  ByteString ->
  m ResponseLBS
postMessage sender msg = do
  galley <- viewGalley
  post
    ( galley . paths ["v2", "mls", "messages"]
        . zUser sender
        . zConn "conn"
        . content "message/mls"
        . bytes msg
    )

postWelcome :: (MonadIO m, MonadHttp m, HasGalley m, HasCallStack) => UserId -> ByteString -> m ResponseLBS
postWelcome uid welcome = do
  galley <- viewGalley
  post
    ( galley
        . paths ["mls", "welcome"]
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
   in (Message tbs (MessageExtraFields sig Nothing Nothing))

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
    mpWelcome :: Maybe ByteString
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
  liftIO $ spawn (proc "mls-test-cli" (["--store", cdir </> "store"] <> args)) mbstdin

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
createLocalMLSClient (qUntagged -> qusr) = do
  qcid <- createWireClient qusr
  initMLSClient qcid

  -- set public key
  pkey <- mlscli qcid ["public-key"] Nothing
  brig <- view tsBrig
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
  foldQualified loc createLocalMLSClient (createFakeMLSClient . qUntagged) qusr

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
  brig <- view tsBrig
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

rollBackClient :: HasCallStack => ClientIdentity -> MLSTest ()
rollBackClient cid = do
  link <- groupFileLink cid
  (prefix, n) <-
    liftIO $ parseGroupFileName =<< getSymbolicLinkTarget link
  when (n == 0) $ do
    liftIO . assertFailure $ "Cannot roll back client " <> cid2Str cid
  removeFile link
  bd <- State.gets mlsBaseDir
  let groupFile = bd </> cid2Str cid </> (prefix <> "." <> show (n - 1))
  createFileLink groupFile link

-- | Create conversation and corresponding group.
setupMLSGroup :: HasCallStack => ClientIdentity -> MLSTest (GroupId, Qualified ConvId)
setupMLSGroup creator = do
  State.gets mlsGroupId >>= \case
    Just _ -> liftIO $ assertFailure "only one group can be created"
    Nothing -> pure ()

  ownDomain <- liftTest viewFederationDomain
  liftIO $ assertEqual "creator is not local" (ciDomain creator) ownDomain
  conv <-
    responseJsonError
      =<< liftTest
        ( postConvQualified
            (ciUser creator)
            (defNewMLSConv (ciClient creator))
        )
      <!! const 201 === statusCode
  let groupId = fromJust (preview (to cnvProtocol . _ProtocolMLS . to cnvmlsGroupId) conv)

  groupJSON <- mlscli creator ["group", "create", T.unpack (toBase64Text (unGroupId groupId))] Nothing
  g <- nextGroupFile creator
  liftIO $ BS.writeFile g groupJSON
  State.modify $ \s ->
    s
      { mlsGroupId = Just groupId,
        mlsMembers = Set.singleton creator
      }
  pure (groupId, cnvQualifiedId conv)

keyPackageFile :: HasCallStack => ClientIdentity -> KeyPackageRef -> MLSTest FilePath
keyPackageFile qcid ref =
  State.gets $ \mls ->
    mlsBaseDir mls </> cid2Str qcid
      </> T.unpack (T.decodeUtf8 (hex (unKeyPackageRef ref)))

claimLocalKeyPackages :: HasCallStack => ClientIdentity -> Local UserId -> MLSTest KeyPackageBundle
claimLocalKeyPackages qcid lusr = do
  brig <- view tsBrig
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
  pure . catMaybes . map toClient $ files

-- | Generate one key package for each client of a remote user
claimRemoteKeyPackages :: HasCallStack => Remote UserId -> MLSTest KeyPackageBundle
claimRemoteKeyPackages (qUntagged -> qusr) = do
  brig <- view tsBrig
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
claimKeyPackages :: HasCallStack => ClientIdentity -> Qualified UserId -> MLSTest KeyPackageBundle
claimKeyPackages cid qusr = do
  loc <- liftTest $ qualifyLocal ()
  foldQualified loc (claimLocalKeyPackages cid) claimRemoteKeyPackages qusr

-- | Claim keypackages and create a commit/welcome pair on a given client.
-- Note that this alters the state of the group immediately. If we want to test
-- a scenario where the commit is rejected by the backend, we can restore the
-- group to the previous state by using an older version of the group file.
createAddCommit :: HasCallStack => ClientIdentity -> [Qualified UserId] -> MLSTest MessagePackage
createAddCommit committer usersToAdd = do
  bundles <- traverse (claimKeyPackages committer) usersToAdd

  let bundleEntries = concatMap (toList . kpbEntries) bundles
      entryIdentity be = mkClientIdentity (kpbeUser be) (kpbeClient be)

  clientsAndKeyPackages <- for bundleEntries $ \be -> do
    let d = kpData . kpbeKeyPackage $ be
        qcid = entryIdentity be
    fn <- keyPackageFile qcid (kpbeRef be)
    liftIO $ BS.writeFile fn d
    pure (qcid, fn)

  createAddCommitWithKeyPackages committer clientsAndKeyPackages

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
        mpWelcome = Nothing
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
  commit <-
    mlscli
      qcid
      ( [ "member",
          "add",
          "--group",
          g,
          "--welcome-out",
          welcomeFile,
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
  pure $
    MessagePackage
      { mpSender = qcid,
        mpMessage = commit,
        mpWelcome = Just welcome
      }

createPendingProposalCommit :: HasCallStack => ClientIdentity -> MLSTest MessagePackage
createPendingProposalCommit qcid = do
  bd <- State.gets mlsBaseDir
  welcomeFile <- liftIO $ emptyTempFile bd "welcome"
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
        welcomeFile
      ]
      Nothing

  welcome <-
    liftIO $
      doesFileExist welcomeFile >>= \case
        False -> pure Nothing
        True -> Just <$> BS.readFile welcomeFile
  pure
    MessagePackage
      { mpSender = qcid,
        mpMessage = commit,
        mpWelcome = welcome
      }

createRemoveCommit :: HasCallStack => ClientIdentity -> [ClientIdentity] -> MLSTest MessagePackage
createRemoveCommit = error "TODO"

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
        mpWelcome = Nothing
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
  for_ (Set.delete (mpSender msg) mems) $ \qcid -> do
    bd <- State.gets mlsBaseDir
    g <- currentGroupFile qcid
    gNew <- nextGroupFile qcid
    void $
      mlscli
        qcid
        [ "consume",
          "--group",
          g,
          "--group-out",
          gNew,
          "--signer-key",
          bd </> "removal.key",
          "-"
        ]
        (Just (mpMessage msg))

-- | Send an MLS message and simulate clients receiving it. If the message is a
-- commit, the 'sendAndConsumeCommit' function should be used instead.
sendAndConsumeMessage :: HasCallStack => MessagePackage -> MLSTest [Event]
sendAndConsumeMessage mp = do
  events <-
    fmap mmssEvents . responseJsonError
      =<< postMessage (ciUser (mpSender mp)) (mpMessage mp)
      <!! const 201 === statusCode
  consumeMessage mp

  for_ (mpWelcome mp) $ \welcome -> do
    postWelcome (ciUser (mpSender mp)) welcome
      !!! const 201 === statusCode
    consumeWelcome welcome

  pure events

-- | Send an MLS commit message, simulate clients receiving it, and update the
-- test state accordingly.
sendAndConsumeCommit ::
  HasCallStack =>
  MessagePackage ->
  MLSTest [Event]
sendAndConsumeCommit mp = do
  events <- sendAndConsumeMessage mp

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
