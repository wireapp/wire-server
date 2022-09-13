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

module GalleyTest.API.MLS.Util where

import API.Util
import Bilge
import Bilge.Assert
import Control.Lens (preview, to, view)
import Control.Monad.Catch
import qualified Control.Monad.State as State
import Crypto.PubKey.Ed25519
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import Data.ByteString.Conversion
import Data.Default
import Data.Domain
import Data.Id
import Data.Json.Util
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import qualified Data.Text as T
import Galley.Keys
import Galley.Options
import Imports
import System.FilePath
import System.IO.Temp
import System.Process
import Test.QuickCheck (arbitrary, generate)
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

mapRemoteKeyPackageRef :: (MonadIO m, MonadHttp m, MonadCatch m) => (Request -> Request) -> KeyPackageBundle -> m ()
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
