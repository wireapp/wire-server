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
import Control.Lens (preview, to, view)
import Control.Monad.Catch
import qualified Control.Monad.State as State
import qualified Data.ByteString as BS
import Data.ByteString.Conversion
import Data.Default
import Data.Domain
import Data.Id
import Data.Json.Util
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import qualified Data.Text as T
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
import Wire.API.MLS.Serialisation
import Wire.API.User.Client
import Wire.API.User.Client.Prekey

data CreateClients = CreateWithoutKey | CreateWithKey | DontCreateClients
  deriving (Eq)

data CreateConv = CreateConv | CreateProteusConv | DontCreateConv
  deriving (Eq)

data UserOrigin = LocalUser | RemoteUser Domain

createNewConv :: CreateConv -> Maybe NewConv
createNewConv CreateConv = Just defNewMLSConv
createNewConv CreateProteusConv = Just defNewProteusConv
createNewConv DontCreateConv = Nothing

data SetupOptions = SetupOptions
  { createClients :: CreateClients,
    creatorOrigin :: UserOrigin,
    createConv :: CreateConv,
    makeConnections :: Bool
  }

instance Default SetupOptions where
  def =
    SetupOptions
      { createClients = CreateWithKey,
        creatorOrigin = LocalUser,
        createConv = DontCreateConv,
        makeConnections = True
      }

data MessagingSetup = MessagingSetup
  { creator :: Participant,
    users :: [Participant],
    conversation :: Qualified ConvId,
    welcome :: ByteString,
    commit :: ByteString
  }

data Participant = Participant
  { pUserId :: Qualified UserId,
    pClients :: NonEmpty (String, ClientId)
  }
  deriving (Show)

cli :: FilePath -> [String] -> CreateProcess
cli tmp args =
  proc "crypto-cli" $
    ["--store", tmp </> "store.db", "--enc-key", "test"] <> args

pClientQid :: Participant -> String
pClientQid = fst . NonEmpty.head . pClients

setupUserClient ::
  HasCallStack =>
  FilePath ->
  CreateClients ->
  Qualified UserId ->
  State.StateT [LastPrekey] TestM (String, ClientId, RawMLS KeyPackage)
setupUserClient tmp doCreateClients usr = do
  localDomain <- lift viewFederationDomain
  lpk <- takeLastPrekey
  lift $ do
    -- create client if requested
    c <- case doCreateClients of
      DontCreateClients -> liftIO $ generate arbitrary
      _ -> randomClient (qUnqualified usr) lpk

    let qcid =
          show (qUnqualified usr)
            <> ":"
            <> T.unpack (client c)
            <> "@"
            <> T.unpack (domainText (qDomain usr))

    -- generate key package
    kp <-
      liftIO $
        decodeMLSError
          =<< spawn (cli tmp ["key-package", qcid]) Nothing
    liftIO $ BS.writeFile (tmp </> qcid) (rmRaw kp)

    -- Set Bob's private key and upload key package if required. If a client
    -- does not have to be created and it is remote, pretend to have claimed its
    -- key package.
    case doCreateClients of
      CreateWithKey -> addKeyPackage usr c kp
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
        mapRemoteKeyPackageRef brig bundle
      _ -> pure ()

    pure (qcid, c, kp)

setupParticipant ::
  HasCallStack =>
  FilePath ->
  CreateClients ->
  Int ->
  Qualified UserId ->
  State.StateT [LastPrekey] TestM Participant
setupParticipant tmp doCreateClients numClients usr =
  Participant usr . NonEmpty.fromList . fmap (\(qcid, c, _kp) -> (qcid, c))
    <$> replicateM numClients (setupUserClient tmp doCreateClients usr)

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
  creator <- lift (createUserOrId creatorOrigin) >>= setupParticipant tmp DontCreateClients 1
  others <- for ns $ \(n, ur) ->
    lift (createUserOrId ur) >>= fmap (,ur) . setupParticipant tmp createClients n
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

withLastPrekeys :: Monad m => State.StateT [LastPrekey] m a -> m a
withLastPrekeys m = State.evalStateT m someLastPrekeys

setupGroup :: HasCallStack => FilePath -> CreateConv -> Participant -> String -> TestM (Qualified ConvId)
setupGroup tmp createConv creator name = do
  (mGroupId, conversation) <- case createNewConv createConv of
    Nothing -> pure (Nothing, error "No conversation created")
    Just nc -> do
      conv <-
        responseJsonError =<< postConvQualified (qUnqualified (pUserId creator)) nc
          <!! const 201 === statusCode

      pure (preview (to cnvProtocol . _ProtocolMLS . to cnvmlsGroupId) conv, cnvQualifiedId conv)

  let groupId = toBase64Text (maybe "test_group" unGroupId mGroupId)
  groupJSON <-
    liftIO $
      spawn (cli tmp ["group", pClientQid creator, T.unpack groupId]) Nothing
  liftIO $ BS.writeFile (tmp </> name) groupJSON

  pure conversation

setupCommit ::
  (HasCallStack, Foldable f) =>
  String ->
  String ->
  String ->
  f (String, ClientId) ->
  IO (ByteString, ByteString)
setupCommit tmp groupName newGroupName clients =
  (,)
    <$> spawn
      ( cli
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

takeLastPrekey :: MonadFail m => State.StateT [LastPrekey] m LastPrekey
takeLastPrekey = do
  (lpk : lpks) <- State.get
  State.put lpks
  pure lpk

-- | Setup: Alice creates a group and invites Bob that is local or remote to
-- Alice depending on the passed in creator origin. Return welcome and commit
-- message.
aliceInvitesBob :: HasCallStack => (Int, UserOrigin) -> SetupOptions -> TestM MessagingSetup
aliceInvitesBob bobConf opts@SetupOptions {..} = withSystemTempDirectory "mls" $ \tmp -> do
  (alice, [bob]) <- withLastPrekeys $ setupParticipants tmp opts [bobConf]
  -- create a group
  conversation <- setupGroup tmp createConv alice "group"

  -- add bob to it and get welcome message
  (commit, welcome) <- liftIO $ setupCommit tmp "group" "group" (pClients bob)

  pure $
    MessagingSetup
      { creator = alice,
        users = [bob],
        ..
      }

addKeyPackage :: HasCallStack => Qualified UserId -> ClientId -> RawMLS KeyPackage -> TestM ()
addKeyPackage u c kp = do
  let update = defUpdateClient {updateClientMLSPublicKeys = Map.singleton Ed25519 (bcSignatureKey (kpCredential (rmValue kp)))}
  -- set public key
  brig <- view tsBrig
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

  -- claim key package (technically, some other user should claim them, but it doesn't really make a difference)
  bundle <-
    responseJsonError
      =<< post
        ( brig
            . paths ["mls", "key-packages", "claim", toByteString' (qDomain u), toByteString' (qUnqualified u)]
            . zUser (qUnqualified u)
        )
      <!! const 200 === statusCode
  liftIO $ map (Just . kpbeRef) (toList (kpbEntries bundle)) @?= [kpRef' kp]

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

postCommit :: MessagingSetup -> TestM [Event]
postCommit MessagingSetup {..} = do
  galley <- viewGalley
  responseJsonError
    =<< post
      ( galley . paths ["mls", "messages"]
          . zUser (qUnqualified (pUserId creator))
          . zConn "conn"
          . content "message/mls"
          . bytes commit
      )
    <!! const 201 === statusCode

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
