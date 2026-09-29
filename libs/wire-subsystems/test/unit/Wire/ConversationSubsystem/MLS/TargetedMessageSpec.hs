{-# LANGUAGE ScopedTypeVariables #-}

module Wire.ConversationSubsystem.MLS.TargetedMessageSpec (spec) where

import Data.Domain (Domain (..))
import Data.Id
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map qualified as Map
import Data.Qualified
import Data.Tagged (Tagged)
import Data.UUID qualified as UUID
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.State (get, runState)
import Test.Hspec
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Protocol
import Wire.API.Error.Galley
import Wire.API.MLS.CipherSuite (legacyCipherSuite)
import Wire.API.MLS.Commit (HPKECiphertext (..))
import Wire.API.MLS.Credential
import Wire.API.MLS.Group.Serialisation qualified as Group
import Wire.API.MLS.Keys
import Wire.API.MLS.LeafNode (LeafIndex)
import Wire.API.MLS.ProtocolVersion (defaultProtocolVersion)
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation (ConvOrSubChoice (Conv))
import Wire.API.MLS.TargetedMessage
import Wire.API.MLS.TargetedMessage qualified as Targeted
import Wire.API.Push.V2 (RecipientClients (RecipientClientsSome))
import Wire.ConversationStore.MLS.Types
import Wire.ConversationSubsystem.MLS.Message
import Wire.ExternalAccess
import Wire.MockInterpreters.ConversationStore (inMemoryConversationStoreInterpreterWithMLS)
import Wire.MockInterpreters.NotificationSubsystem (inMemoryNotificationSubsystemInterpreter)
import Wire.MockInterpreters.Now (defaultTime, interpretNowConst)
import Wire.MockInterpreters.TinyLog (noopLogger)
import Wire.NotificationSubsystem (Push (..), Recipient (..))
import Wire.StoredConversation

spec :: Spec
spec = describe "targeted MLS messages" do
  it "pushes each batch message only to its designated recipient" do
    let fx = fixture
        result = runTargeted fx [targetedMessage 1]
    case result of
      Right pushes -> do
        length pushes `shouldBe` 1
        let push = head pushes
        push.recipients `shouldBe` [Recipient fx.recipientUser (RecipientClientsSome (NonEmpty.singleton fx.recipientClient))]
      Left err -> expectationFailure (show err)

  it "rejects duplicate recipients before pushing any message" do
    let fx = fixture
        result = runTargeted fx [targetedMessage 1, targetedMessage 1]
    case result of
      Left _ -> pure ()
      Right _ -> expectationFailure "duplicate recipients were accepted"

  it "rejects messages older than three epochs" do
    let fx = fixture
        result = runTargeted fx [targetedMessageAt 1 1]
    case result of
      Left _ -> pure ()
      Right _ -> expectationFailure "stale targeted message was accepted"

  it "accepts the current epoch and three epochs back, but rejects future epochs" do
    let fx = fixture
    expectAccepted fx 5
    expectAccepted fx 2
    expectRejected fx [targetedMessageAt 1 6]

  it "rejects a sender leaf that belongs to another client" do
    let fx = fixture
        message = mapTargetedMessage (targetedMessage 1) (\msg -> msg {sender = fx.recipientLeaf})
    expectRejected fx [message]

  it "rejects an unknown recipient leaf" do
    let fx = fixture
        message = mapTargetedMessage (targetedMessage 1) (\msg -> msg {recipient = 99})
    expectRejected fx [message]

  it "rejects a batch containing messages from different groups" do
    let fx = fixture
        message = mapTargetedMessage (targetedMessage 2) (\msg -> msg {Targeted.groupId = GroupId "other-group"})
    expectRejected fx [targetedMessage 1, message]

  it "does not push earlier messages when a later message is invalid" do
    let fx = fixture
        invalidMessage = mapTargetedMessage (targetedMessage 2) (\msg -> msg {sender = fx.recipientLeaf})
        (pushes, result) = runTargetedWithPushes fx [targetedMessage 1, invalidMessage]
    result `shouldSatisfy` isLeft
    pushes `shouldBe` []

data Fixture = Fixture
  { conversation :: StoredConversation,
    groupId :: GroupId,
    senderUser :: UserId,
    senderClient :: ClientId,
    recipientUser :: UserId,
    recipientClient :: ClientId,
    senderLeaf :: LeafIndex,
    recipientLeaf :: LeafIndex
  }

fixture :: Fixture
fixture =
  Fixture
    { conversation =
        StoredConversation
          { id_ = convId,
            localMembers = [newMember sender, newMember recipient],
            remoteMembers = [],
            metadata = defConversationMetadata (Just sender),
            protocol =
              ProtocolMLS
                ConversationMLSData
                  { cnvmlsGroupId = gid,
                    cnvmlsActiveData = Just (ActiveMLSConversationData (Epoch 5) defaultTime legacyCipherSuite)
                  }
          },
      groupId = gid,
      senderUser = sender,
      senderClient = senderC,
      recipientUser = recipient,
      recipientClient = recipientC,
      senderLeaf = 0,
      recipientLeaf = 1
    }
  where
    convId = Id (fromJust (UUID.fromString "aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa"))
    sender = Id (fromJust (UUID.fromString "bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb"))
    recipient = Id (fromJust (UUID.fromString "cccccccc-cccc-cccc-cccc-cccccccccccc"))
    senderC = ClientId 1
    recipientC = ClientId 2
    gid = Group.newGroupId RegularConv (Qualified (Conv convId) (Domain "example.com"))

targetedMessage :: Word32 -> RawMLS PersistentTargetedMessage
targetedMessage counter = targetedMessageAt counter 5

targetedMessageAt :: Word32 -> Word64 -> RawMLS PersistentTargetedMessage
targetedMessageAt counter epoch =
  let msg =
        PersistentTargetedMessage
          { protocolVersion = defaultProtocolVersion,
            wireFormat = TargetedMessageWireFormat,
            counter = counter,
            sender = fixture.senderLeaf,
            recipient = fixture.recipientLeaf,
            epoch = Epoch epoch,
            groupId = fixture.groupId,
            payload = HPKECiphertext "kem" "ciphertext",
            signature = "signature"
          }
   in RawMLS "raw-targeted-message" msg

mapTargetedMessage :: RawMLS PersistentTargetedMessage -> (PersistentTargetedMessage -> PersistentTargetedMessage) -> RawMLS PersistentTargetedMessage
mapTargetedMessage raw f = raw {value = f raw.value}

expectAccepted :: Fixture -> Word64 -> Expectation
expectAccepted fx epoch =
  case runTargeted fx [targetedMessageAt 1 epoch] of
    Right [_] -> pure ()
    Right pushes -> expectationFailure ("expected one push, got " <> show (length pushes))
    Left err -> expectationFailure err

expectRejected :: Fixture -> [RawMLS PersistentTargetedMessage] -> Expectation
expectRejected fx messages =
  case runTargeted fx messages of
    Left _ -> pure ()
    Right _ -> expectationFailure "invalid targeted message was accepted"

data TargetedMessageTestError = TargetedMessageRejected
  deriving (Show)

runErrorAsTestFailure :: forall e r a. (Member (Error TargetedMessageTestError) r) => Sem (Error e ': r) a -> Sem r a
runErrorAsTestFailure = mapError (const TargetedMessageRejected)

runTargeted :: Fixture -> [RawMLS PersistentTargetedMessage] -> Either String [Push]
runTargeted fx messages = snd (runTargetedWithPushes fx messages)

runTargetedWithPushes :: Fixture -> [RawMLS PersistentTargetedMessage] -> ([Push], Either String [Push])
runTargetedWithPushes fx messages =
  case rawResult of
    (_, (pushes, Left _)) -> (pushes, Left "targeted message was rejected")
    (_, (pushes, Right result)) -> (pushes, Right result)
  where
    rawResult =
      run
        . runState @[Qualified UserId] []
        . runState @[Push] []
        . runError @TargetedMessageTestError
        . runErrorAsTestFailure @MLSProtocolError
        . runErrorAsTestFailure @(Tagged 'MLSStaleMessage ())
        . runErrorAsTestFailure @(Tagged 'MLSUnsupportedMessage ())
        . runErrorAsTestFailure @(Tagged 'MLSInvalidLeafNodeIndex ())
        . runErrorAsTestFailure @(Tagged 'MLSClientSenderUserMismatch ())
        . runErrorAsTestFailure @(Tagged 'ConvNotFound ())
        . runInputConst (Just (MLSKeysByPurpose (error "test MLS keys are not used")))
        . inMemoryConversationStoreInterpreterWithMLS
          (Map.singleton fx.conversation.id_ fx.conversation)
          (Map.singleton fx.groupId (mempty, targetedMessageIndexMap fx))
        . interpretExternalAccess
        . inMemoryNotificationSubsystemInterpreter
        . interpretNowConst defaultTime
        . noopLogger
        $ do
          validateAndPropagateTargetedMessages
            (toLocalUnsafe (Domain "example.com") fx.senderUser)
            fx.senderClient
            RegularConv
            (qualifyAs (toLocalUnsafe (Domain "example.com") fx.senderUser) (Conv fx.conversation.id_))
            messages
          get

targetedMessageIndexMap :: Fixture -> IndexMap
targetedMessageIndexMap fx =
  imFromList
    [ (fx.senderLeaf, RegularClient (mkClientIdentity (Qualified fx.senderUser (Domain "example.com")) fx.senderClient)),
      (fx.recipientLeaf, RegularClient (mkClientIdentity (Qualified fx.recipientUser (Domain "example.com")) fx.recipientClient))
    ]

interpretExternalAccess :: Sem (ExternalAccess ': r) a -> Sem r a
interpretExternalAccess = interpret $ \case
  Deliver _ -> pure []
  DeliverAsync _ -> pure ()
  DeliverAndDeleteAsync _ _ -> pure ()
