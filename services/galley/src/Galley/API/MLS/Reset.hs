-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.API.MLS.Reset
  ( resetMLSConversation,
    resetLocalMLSConversation,
    ResetConversationStaticErrors,
  )
where

import Control.Monad.Codensity hiding (reset)
import Data.Aeson qualified as A
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.Qualified
import Data.Time.Clock
import Galley.API.Action
import Galley.API.Error
import Galley.API.MLS.Enabled
import Galley.API.MLS.SubConversation
import Galley.API.MLS.Util
import Galley.API.Util
import Galley.Data.Conversation.Types
import Galley.Effects
import Galley.Effects.ConversationStore
import Galley.Effects.FederatorAccess
import Galley.Effects.MemberStore
import Galley.Env
import Galley.Types.Conversations.Members
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
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.Federation.Version
import Wire.API.MLS.Group.Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.Routes.Public.Galley.MLS
import Wire.API.VersionInfo
import Wire.NotificationSubsystem

resetMLSConversation ::
  ( Member (Input Env) r,
    Member (Input UTCTime) r,
    Member (ErrorS MLSNotEnabled) r,
    Member (ErrorS MLSStaleMessage) r,
    Member (ErrorS ConvAccessDenied) r,
    Member (ErrorS ConvNotFound) r,
    Member (Error MLSProtocolError) r,
    Member (Error InternalError) r,
    Member (ErrorS InvalidOperation) r,
    Member (ErrorS MLSFederatedResetNotSupported) r,
    Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member FederatorAccess r,
    Member ExternalAccess r,
    Member (Error FederationError) r,
    Member MemberStore r,
    Member NotificationSubsystem r,
    Member ProposalStore r,
    Member Random r,
    Member Resource r,
    Member SubConversationStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  MLSReset ->
  Sem r ()
resetMLSConversation lusr reset = do
  assertMLSEnabled
  (ctype, qcnvOrSub) <- getConvFromGroupId reset.groupId
  foldQualified
    lusr
    (\lcnvOrSub -> resetLocalMLSConversation (tUntagged lusr) ctype lcnvOrSub reset)
    (\r -> resetRemoteMLSConversation r lusr reset)
    qcnvOrSub

resetLocalMLSConversation ::
  ( Member (Input Env) r,
    Member (Input UTCTime) r,
    Member (Error InternalError) r,
    Member (ErrorS MLSNotEnabled) r,
    Member (ErrorS MLSStaleMessage) r,
    Member (ErrorS ConvAccessDenied) r,
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
  ConvType ->
  Local ConvOrSubConvId ->
  MLSReset ->
  Sem r ()
resetLocalMLSConversation qusr ctype lcnvOrSub reset =
  case tUnqualified lcnvOrSub of
    SubConv c s -> deleteLocalSubConversation qusr (c <$ lcnvOrSub) s reset
    Conv cnvId -> do
      let lcnvId = qualifyAs lcnvOrSub cnvId
      cnv <- getConversationAndCheckMembership qusr lcnvId
      mlsData <- case convProtocol cnv of
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
                Left _ -> newGroupId ctype (tUntagged lcnvOrSub)
                Right gid' -> gid'

          resetConversation cnvId newGid

          -- kick all remote members from backends that don't support this group ID version
          let remoteUsers = map rmId (convRemoteMembers cnv)
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

          pure ()

type ResetConversationStaticErrors =
  '[ ErrorS MLSNotEnabled,
     ErrorS ConvAccessDenied,
     ErrorS ConvNotFound,
     ErrorS InvalidOperation,
     ErrorS MLSStaleMessage
   ]

resetRemoteMLSConversation ::
  ( Member FederatorAccess r,
    Member (Error FederationError) r,
    Member (Error MLSProtocolError) r,
    Member (ErrorS MLSNotEnabled) r,
    Member (ErrorS MLSStaleMessage) r,
    Member (ErrorS ConvAccessDenied) r,
    Member (ErrorS InvalidOperation) r,
    Member (ErrorS ConvNotFound) r,
    Member (ErrorS MLSFederatedResetNotSupported) r
  ) =>
  Remote x ->
  Local UserId ->
  MLSReset ->
  Sem r ()
resetRemoteMLSConversation r lusr reset = do
  let req =
        ResetConversationRequest
          { userId = tUnqualified lusr,
            groupId = reset.groupId,
            epoch = reset.epoch
          }
  runFederatedEither r (fedClient @'Galley @"reset-conversation" req) >>= \case
    Left (FederationCallFailure FederatorClientVersionMismatch) ->
      throwS @MLSFederatedResetNotSupported
    Left e -> throw e
    Right (ResetConversationError e) -> rethrowErrors @ResetConversationStaticErrors e
    Right (ResetConversationMLSProtocolError msg) -> throw (mlsProtocolError msg)
    Right ResetConversationOk -> pure ()
