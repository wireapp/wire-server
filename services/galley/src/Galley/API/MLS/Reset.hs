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
import Data.Id
import Data.Qualified
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
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource
import Wire.API.Conversation hiding (Member)
import Wire.API.Conversation.Protocol
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.MLS.Group.Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.Routes.Public.Galley.MLS

resetMLSConversation ::
  ( Member (Input Env) r,
    Member (ErrorS MLSNotEnabled) r,
    Member (ErrorS MLSStaleMessage) r,
    Member (ErrorS ConvAccessDenied) r,
    Member (ErrorS ConvNotFound) r,
    Member (Error MLSProtocolError) r,
    Member (ErrorS InvalidOperation) r,
    Member (ErrorS MLSFederatedResetNotSupported) r,
    Member Resource r,
    Member ConversationStore r,
    Member FederatorAccess r,
    Member (Error FederationError) r,
    Member MemberStore r,
    Member SubConversationStore r
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
    Member (ErrorS MLSNotEnabled) r,
    Member (ErrorS MLSStaleMessage) r,
    Member (ErrorS ConvAccessDenied) r,
    Member (ErrorS ConvNotFound) r,
    Member (ErrorS InvalidOperation) r,
    Member Resource r,
    Member ConversationStore r,
    Member MemberStore r,
    Member SubConversationStore r
  ) =>
  Qualified UserId ->
  ConvType ->
  Local ConvOrSubConvId ->
  MLSReset ->
  Sem r ()
resetLocalMLSConversation qusr ctype lcnvOrSub reset =
  case tUnqualified lcnvOrSub of
    SubConv c s -> deleteLocalSubConversation qusr (c <$ lcnvOrSub) s reset -- TODO: extract
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
                Left _ ->
                  convToGroupId
                    GroupIdVersion2
                    (groupIdParts ctype 0 (tUntagged lcnvOrSub))
                Right gid' -> gid'

          resetConversation cnvId newGid

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
