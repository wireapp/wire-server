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

module Galley.API.MLS.Propagate where

import Control.Comonad
import Data.Domain
import Data.Id
import Data.Json.Util
import qualified Data.Map as Map
import Data.Qualified
import qualified Data.Set as Set
import Data.Time
import Galley.API.Push
import qualified Galley.Data.Conversation.Types as Data
import Galley.Data.Services
import Galley.Effects
import Galley.Effects.FederatorAccess
import Galley.Types.Conversations.Members
import Imports
import Network.Wai.Utilities.Server
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog
import qualified System.Logger.Class as Logger
import Wire.API.Error
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
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
  Local Data.Conversation ->
  Maybe ConnId ->
  ByteString ->
  Sem r ()
propagateMessage qusr lconv con raw = do
  -- FUTUREWORK: check the epoch
  let lmems = Data.convLocalMembers . tUnqualified $ lconv
      botMap = Map.fromList $ do
        m <- lmems
        b <- maybeToList $ newBotMember m
        pure (lmId m, b)
      mm = defMessageMetadata
  now <- input @UTCTime
  let lcnv = fmap Data.convId lconv
      qcnv = qUntagged lcnv
      e = Event qcnv qusr now $ EdMLSMessage raw
      lclients = tUnqualified . clients <$> lmems
      mkPush :: UserId -> ClientId -> MessagePush 'NormalMessage
      mkPush u c = newMessagePush lcnv botMap con mm (u, c) e
  runMessagePush lconv (Just qcnv) $
    foldMap (uncurry mkPush) (cToList =<< lclients)

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
            rmmRecipients = rs >>= remoteMemberMLSClients,
            rmmMessage = Base64ByteString raw
          }
  where
    cToList :: (UserId, Set ClientId) -> [(UserId, ClientId)]
    cToList (u, s) = (u,) <$> Set.toList s

    clients :: LocalMember -> Local (UserId, Set ClientId)
    clients LocalMember {..} = qualifyAs lconv (lmId, Set.map fst lmMLSClients)

    remoteMemberMLSClients :: RemoteMember -> [(UserId, ClientId)]
    remoteMemberMLSClients rm =
      map
        (tUnqualified (rmId rm),)
        (toList (Set.map fst (rmMLSClients rm)))

    handleError :: Member TinyLog r => Either (Remote [a], FederationError) x -> Sem r ()
    handleError (Right _) = pure ()
    handleError (Left (r, e)) =
      warn $
        Logger.msg ("A message could not be delivered to a remote backend" :: ByteString)
          . Logger.field "remote_domain" (domainText (tDomain r))
          . logErrorMsg (toWai e)
