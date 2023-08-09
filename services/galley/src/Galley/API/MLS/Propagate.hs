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

module Galley.API.MLS.Propagate where

import Control.Comonad
import Data.Aeson qualified as A
import Data.Domain
import Data.Id
import Data.Json.Util
import Data.Map qualified as Map
import Data.Qualified
import Data.Time
import Galley.API.MLS.Types
import Galley.API.Push
import Galley.Data.Conversation.Types qualified as Data
import Galley.Data.Services
import Galley.Effects
import Galley.Effects.FederatorAccess
import Galley.Types.Conversations.Members
import Imports
import Network.Wai.Utilities.JSONResponse
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog hiding (trace)
import System.Logger.Class qualified as Logger
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.Message
import Wire.API.Unreachable

-- | Propagate a message.
propagateMessage ::
  ( Member ExternalAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member TinyLog r
  ) =>
  Qualified UserId ->
  Local Data.Conversation ->
  ClientMap ->
  Maybe ConnId ->
  ByteString ->
  Sem r (Maybe UnreachableUsers)
propagateMessage qusr lconv cm con raw = do
  -- FUTUREWORK: check the epoch
  let lmems = Data.convLocalMembers . tUnqualified $ lconv
      rmems = Data.convRemoteMembers . tUnqualified $ lconv
      botMap = Map.fromList $ do
        m <- lmems
        b <- maybeToList $ newBotMember m
        pure (lmId m, b)
      mm = defMessageMetadata
  now <- input @UTCTime
  let lcnv = fmap Data.convId lconv
      qcnv = tUntagged lcnv
      e = Event qcnv Nothing qusr now $ EdMLSMessage raw
      mkPush :: UserId -> ClientId -> MessagePush
      mkPush u c = newMessagePush botMap con mm [(u, c)] e

  for_ (lmems >>= localMemberMLSClients lcnv) $ \(u, c) ->
    runMessagePush lconv (Just qcnv) (mkPush u c)

  -- send to remotes
  unreachableFromList . concat
    <$$> traverse handleError
    <=< runFederatedConcurrentlyEither (map remoteMemberQualify rmems)
    $ \(tUnqualified -> rs) ->
      fedClient @'Galley @"on-mls-message-sent" $
        RemoteMLSMessage
          { rmmTime = now,
            rmmSender = qusr,
            rmmMetadata = mm,
            rmmConversation = tUnqualified lcnv,
            rmmRecipients = rs >>= remoteMemberMLSClients,
            rmmMessage = Base64ByteString raw
          }
  where
    localMemberMLSClients :: Local x -> LocalMember -> [(UserId, ClientId)]
    localMemberMLSClients loc lm =
      let localUserQId = tUntagged (qualifyAs loc localUserId)
          localUserId = lmId lm
       in map
            (\(c, _) -> (localUserId, c))
            (toList (Map.findWithDefault mempty localUserQId cm))

    remoteMemberMLSClients :: RemoteMember -> [(UserId, ClientId)]
    remoteMemberMLSClients rm =
      let remoteUserQId = tUntagged (rmId rm)
          remoteUserId = qUnqualified remoteUserQId
       in map
            (\(c, _) -> (remoteUserId, c))
            (toList (Map.findWithDefault mempty remoteUserQId cm))

    remotesToQIds = fmap (tUntagged . rmId)

    handleError ::
      Member TinyLog r =>
      Either (Remote [RemoteMember], FederationError) (Remote RemoteMLSMessageResponse) ->
      Sem r [Qualified UserId]
    handleError (Right x) = case tUnqualified x of
      RemoteMLSMessageOk -> pure []
      RemoteMLSMessageMLSNotEnabled -> do
        logFedError x (errorToResponse @'MLSNotEnabled)
        pure []
    handleError (Left (r, e)) = do
      logFedError r (toResponse e)
      pure $ remotesToQIds (tUnqualified r)
    logFedError :: Member TinyLog r => Remote x -> JSONResponse -> Sem r ()
    logFedError r e =
      warn $
        Logger.msg ("A message could not be delivered to a remote backend" :: ByteString)
          . Logger.field "remote_domain" (domainText (tDomain r))
          . Logger.field "error" (A.encode e.value)
