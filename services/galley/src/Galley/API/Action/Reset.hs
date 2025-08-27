module Galley.API.Action.Reset (resetLocalMLSMainConversation) where

import Control.Monad.Codensity hiding (reset)
import Data.Aeson qualified as A
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.Qualified
import Galley.API.Action.Kick
import Galley.API.MLS.Util
import Galley.API.Util
import Galley.Effects
import Galley.Effects.FederatorAccess
import Galley.Env
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource
import Polysemy.TinyLog qualified as P
import System.Logger.Class qualified as Log
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Protocol
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.API
import Wire.API.Federation.Error
import Wire.API.Federation.Version
import Wire.API.MLS.Group.Serialisation as GroupId
import Wire.API.MLS.Group.Serialisation qualified as Group
import Wire.API.MLS.SubConversation
import Wire.API.Routes.Public.Galley.MLS
import Wire.API.VersionInfo
import Wire.ConversationStore
import Wire.MemberStore
import Wire.NotificationSubsystem
import Wire.Sem.Now (Now)
import Wire.StoredConversation as Data

resetLocalMLSMainConversation ::
  ( Member (Input Env) r,
    Member Now r,
    Member (ErrorS MLSStaleMessage) r,
    Member (ErrorS ConvNotFound) r,
    Member (ErrorS InvalidOperation) r,
    Member BackendNotificationQueueAccess r,
    Member FederatorAccess r,
    Member ExternalAccess r,
    Member NotificationSubsystem r,
    Member ProposalStore r,
    Member Random r,
    Member Resource r,
    Member ConversationStore r,
    Member MemberStore r,
    Member SubConversationStore r,
    Member P.TinyLog r
  ) =>
  Qualified UserId ->
  Local StoredConversation ->
  MLSReset ->
  Sem r GroupId
resetLocalMLSMainConversation qusr lcnv reset = do
  let cnv = tUnqualified lcnv
      cnvId = cnv.id_
      lcnvOrSub = qualifyAs lcnv (Conv cnvId)
      ctype = cnv.metadata.cnvmType
  mlsData <- case cnv.protocol of
    ProtocolMLS md -> pure md
    ProtocolMixed md -> pure md
    ProtocolProteus -> throwS @'InvalidOperation
  epoch <- case mlsData.cnvmlsActiveData of
    Nothing -> throwS @'InvalidOperation
    Just ad -> pure ad.epoch
  let gid = mlsData.cnvmlsGroupId

  lowerCodensity $ do
    withCommitLock lcnvOrSub reset.groupId reset.epoch
    lift $ do
      unless (reset.groupId == gid) $ throwS @'ConvNotFound
      unless (reset.epoch == epoch) $ throwS @'MLSStaleMessage
      removeAllMLSClients gid

      let newGid = case nextGenGroupId gid of
            Left _ -> Group.newGroupId ctype (tUntagged lcnvOrSub)
            Right gid' -> gid'

      resetConversation cnvId newGid

      -- kick all remote members from backends that don't support this group ID version
      let remoteUsers = map (.id_) cnv.remoteMembers
      let targets = convBotsAndMembers cnv
      results <-
        runFederatedConcurrentlyEither @_ @Brig remoteUsers $
          \_ -> do
            guardVersion $ \fedV -> fedV >= groupIdFedVersion GroupIdVersion2
      let kick qvictim = do
            r <-
              runError @FederationError $
                kickMember qusr (qualifyAs lcnvOrSub cnv) targets qvictim
            case r of
              Left e ->
                P.warn $
                  Log.field "conversation" (toByteString' cnvId)
                    Log.~~ Log.field "user" (toByteString' (qUnqualified qvictim))
                    Log.~~ Log.field "domain" (toByteString' (qDomain qvictim))
                    Log.~~ Log.field "exception" (A.encode (federationErrorToWai e))
                    Log.~~ Log.msg ("Failed to kick user from conversation after reset" :: Text)
              Right _ -> pure ()
      traverse_ kick $
        results >>= \case
          Left (ruids, _) -> sequenceA (tUntagged ruids)
          Right _ -> []
      pure newGid
