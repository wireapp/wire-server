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

module Galley.API.MLS.Reset (resetMLSConversation) where

import Data.Id
import Data.Qualified
import Data.Time.Clock
import Galley.API.Action
import Galley.API.Error
import Galley.API.MLS.Enabled
import Galley.API.MLS.Util
import Galley.API.Update
import Galley.Effects
import Galley.Env
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Resource
import Polysemy.TinyLog qualified as P
import Wire.API.Conversation.Role
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Federation.Error
import Wire.API.MLS.SubConversation
import Wire.API.Routes.Public.Galley.MLS
import Wire.NotificationSubsystem

resetMLSConversation ::
  ( Member (Input Env) r,
    Member (Input UTCTime) r,
    Member (Input (Local ())) r,
    Member (ErrorS MLSNotEnabled) r,
    Member (ErrorS MLSStaleMessage) r,
    Member (ErrorS (ActionDenied LeaveConversation)) r,
    Member (ErrorS ConvNotFound) r,
    Member (Error MLSProtocolError) r,
    Member (Error InternalError) r,
    Member (ErrorS InvalidOperation) r,
    Member (ErrorS MLSFederatedResetNotSupported) r,
    Member (ErrorS GroupIdVersionNotSupported) r,
    Member BackendNotificationQueueAccess r,
    Member ConversationStore r,
    Member FederatorAccess r,
    Member ExternalAccess r,
    Member (Error FederationError) r,
    Member BrigAccess r,
    Member MemberStore r,
    Member NotificationSubsystem r,
    Member ProposalStore r,
    Member Random r,
    Member Resource r,
    Member SubConversationStore r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  MLSReset ->
  Sem r ()
resetMLSConversation lusr reset = do
  assertMLSEnabled
  (_, qcnvOrSub) <- getConvFromGroupId reset.groupId

  cnv <- case qUnqualified qcnvOrSub of
    Conv c -> pure c
    SubConv _ _ -> throwS @InvalidOperation
  let qcnv = qcnvOrSub $> cnv

  foldQualified
    lusr
    ( \lcnv ->
        void $
          updateLocalConversation
            @'ConversationResetTag
            lcnv
            (tUntagged lusr)
            Nothing
            reset
    )
    (\rcnv -> resetRemoteMLSConversation rcnv lusr reset)
    qcnv

resetRemoteMLSConversation ::
  ( Member (Input (Local ())) r,
    Member P.TinyLog r,
    Member (ErrorS (ActionDenied LeaveConversation)) r,
    Member (ErrorS InvalidOperation) r,
    Member (ErrorS ConvNotFound) r,
    Member (ErrorS MLSFederatedResetNotSupported) r,
    Member (ErrorS GroupIdVersionNotSupported) r,
    Member (ErrorS MLSStaleMessage) r,
    Member (Error FederationError) r,
    Member (Error InternalError) r,
    Member BrigAccess r,
    Member ExternalAccess r,
    Member FederatorAccess r,
    Member NotificationSubsystem r,
    Member MemberStore r
  ) =>
  Remote ConvId ->
  Local UserId ->
  MLSReset ->
  Sem r ()
resetRemoteMLSConversation rcnv lusr reset =
  mapError @UnreachableBackends @InternalError (\_ -> InternalErrorWithDescription "Unexpected UnreachableBackends error when updating remote protocol")
    . mapError @NonFederatingBackends @InternalError (\_ -> InternalErrorWithDescription "Unexpected NonFederatingBackends error when updating remote protocol")
    . (handleFedError =<<)
    . runError
    $ updateRemoteConversation @ConversationResetTag
      rcnv
      lusr
      Nothing
      reset
  where
    handleFedError ::
      ( Member (ErrorS MLSFederatedResetNotSupported) r,
        Member (Error FederationError) r
      ) =>
      Either FederationError x ->
      Sem r ()
    handleFedError (Left (FederationCallFailure FederatorClientVersionMismatch)) =
      throwS @MLSFederatedResetNotSupported
    handleFedError (Left e) = throw e
    handleFedError (Right _) = pure ()
