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
import Data.Domain
import Data.Id
import Data.Json.Util
import qualified Data.Map as Map
import Data.Qualified
import Data.Time
import Galley.API.MLS.Types
import Galley.API.Push
import qualified Galley.Data.Conversation.Types as Data
import Galley.Data.Services
import Galley.Effects
import Galley.Effects.FederatorAccess
import Galley.Types.Conversations.Members
import Imports
import qualified Network.Wai.Utilities.Error as Wai
import Network.Wai.Utilities.Server
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import qualified System.Logger.Class as Logger
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.MLS.SubConversation
import Wire.API.Message

-- | Propagate a message.
propagateMessage ::
  ( Member ExternalAccess r,
    Member FederatorAccess r,
    Member GundeckAccess r,
    Member (Input UTCTime) r,
    Member TinyLog r
  ) =>
  Qualified UserId ->
  Local ConvOrSubConv ->
  Maybe ConnId ->
  ByteString ->
  Sem r ()
propagateMessage qusr lConvOrSub con raw = do
  case tUnqualified lConvOrSub of
    (SubConv _ _) -> do
      pure ()
    (Conv mlsMessage) -> do
      let lMlsMessage = qualifyAs lConvOrSub mlsMessage
      let cm = mcMembers mlsMessage
          lconv = mcConv <$> lMlsMessage
      -- FUTUREWORK: check the epoch
      let lmems = Data.convLocalMembers . tUnqualified $ lconv
          botMap = Map.fromList $ do
            m <- lmems
            b <- maybeToList $ newBotMember m
            pure (lmId m, b)
          mm = defMessageMetadata
      now <- input @UTCTime
      let lcnv = fmap Data.convId lconv
          qcnv = tUntagged lcnv
          e = Event qcnv qusr now $ EdMLSMessage raw
          mkPush :: UserId -> ClientId -> MessagePush 'NormalMessage
          mkPush u c = newMessagePush lcnv botMap con mm (u, c) e
      runMessagePush lconv (Just qcnv) $
        foldMap (uncurry mkPush) (lmems >>= localMemberMLSClients lcnv cm)

      -- send to remotes
      traverse_ handleError
        <=< runFederatedConcurrentlyEither (map remoteMemberQualify (Data.convRemoteMembers . tUnqualified $ lconv))
        $ \(tUnqualified -> rs) ->
          fedClient @'Galley @"on-mls-message-sent" $
            RemoteMLSMessage
              { rmmTime = now,
                rmmSender = qusr,
                rmmMetadata = mm,
                rmmConversation = tUnqualified lcnv,
                rmmRecipients = rs >>= remoteMemberMLSClients cm,
                rmmMessage = Base64ByteString raw
              }
  where
    localMemberMLSClients :: Local x -> ClientMap -> LocalMember -> [(UserId, ClientId)]
    localMemberMLSClients loc cm lm =
      let localUserQId = tUntagged (qualifyAs loc localUserId)
          localUserId = lmId lm
       in map
            (\(c, _) -> (localUserId, c))
            (toList (Map.findWithDefault mempty localUserQId cm))

    remoteMemberMLSClients :: ClientMap -> RemoteMember -> [(UserId, ClientId)]
    remoteMemberMLSClients cm rm =
      let remoteUserQId = tUntagged (rmId rm)
          remoteUserId = qUnqualified remoteUserQId
       in map
            (\(c, _) -> (remoteUserId, c))
            (toList (Map.findWithDefault mempty remoteUserQId cm))

    handleError ::
      Member TinyLog r =>
      Either (Remote [a], FederationError) (Remote RemoteMLSMessageResponse) ->
      Sem r ()
    handleError (Right x) = case tUnqualified x of
      RemoteMLSMessageOk -> pure ()
      RemoteMLSMessageMLSNotEnabled -> logFedError x (errorToWai @'MLSNotEnabled)
    handleError (Left (r, e)) = logFedError r (toWai e)

    logFedError :: Member TinyLog r => Remote x -> Wai.Error -> Sem r ()
    logFedError r e =
      warn $
        Logger.msg ("A message could not be delivered to a remote backend" :: ByteString)
          . Logger.field "remote_domain" (domainText (tDomain r))
          . logErrorMsg e
