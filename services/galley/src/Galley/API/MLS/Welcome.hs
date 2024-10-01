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

module Galley.API.MLS.Welcome
  ( sendWelcomes,
    sendLocalWelcomes,
  )
where

import Control.Comonad
import Data.Aeson qualified as A
import Data.Domain
import Data.Id
import Data.Json.Util
import Data.List1
import Data.Map qualified as Map
import Data.Qualified
import Data.Time
import Galley.API.Push
import Galley.Effects.ExternalAccess
import Galley.Effects.FederatorAccess
import Imports
import Network.Wai.Utilities.JSONResponse
import Polysemy
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import System.Logger.Class qualified as Logger
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.Federation.Error
import Wire.API.MLS.Credential
import Wire.API.MLS.Message
import Wire.API.MLS.Serialisation
import Wire.API.MLS.SubConversation
import Wire.API.MLS.Welcome
import Wire.API.Message
import Wire.API.Push.V2 (RecipientClients (..))
import Wire.NotificationSubsystem

sendWelcomes ::
  ( Member FederatorAccess r,
    Member ExternalAccess r,
    Member P.TinyLog r,
    Member (Input UTCTime) r,
    Member NotificationSubsystem r
  ) =>
  Local ConvOrSubConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  [ClientIdentity] ->
  RawMLS Welcome ->
  Sem r ()
sendWelcomes loc qusr con cids welcome = do
  now <- input
  let qcnv = convFrom <$> tUntagged loc
      (locals, remotes) = partitionQualified loc (map cidQualifiedClient cids)
      msg = mkRawMLS $ mkMessage (MessageWelcome welcome)
  sendLocalWelcomes qcnv qusr con now msg (qualifyAs loc locals)
  sendRemoteWelcomes qcnv qusr msg remotes
  where
    convFrom (Conv c) = c
    convFrom (SubConv c _) = c

sendLocalWelcomes ::
  ( Member P.TinyLog r,
    Member ExternalAccess r,
    Member NotificationSubsystem r
  ) =>
  Qualified ConvId ->
  Qualified UserId ->
  Maybe ConnId ->
  UTCTime ->
  RawMLS Message ->
  Local [(UserId, ClientId)] ->
  Sem r ()
sendLocalWelcomes qcnv qusr con now welcome lclients = do
  -- only create one notification per user
  let rcpts =
        map (\(u, cs) -> Recipient u (RecipientClientsSome (List1 cs)))
          . Map.assocs
          . foldr
            (\(u, c) -> Map.insertWith (<>) u (pure c))
            mempty
          $ tUnqualified lclients
  let e = Event qcnv Nothing qusr now $ EdMLSWelcome welcome.raw
  runMessagePush lclients (Just qcnv) $
    newMessagePush mempty con defMessageMetadata rcpts e

sendRemoteWelcomes ::
  ( Member FederatorAccess r,
    Member P.TinyLog r
  ) =>
  Qualified ConvId ->
  Qualified UserId ->
  RawMLS Message ->
  [Remote (UserId, ClientId)] ->
  Sem r ()
sendRemoteWelcomes qcnv qusr welcome clients = do
  let msg = Base64ByteString welcome.raw
  traverse_ handleError <=< runFederatedConcurrentlyEither clients $ \rcpts ->
    fedClient @'Galley @"mls-welcome"
      MLSWelcomeRequest
        { originatingUser = qUnqualified qusr,
          welcomeMessage = msg,
          recipients = tUnqualified rcpts,
          qualifiedConvId = qcnv
        }
  where
    handleError ::
      (Member P.TinyLog r) =>
      Either (Remote [a], FederationError) (Remote MLSWelcomeResponse) ->
      Sem r ()
    handleError (Right x) = case tUnqualified x of
      MLSWelcomeSent -> pure ()
      MLSWelcomeMLSNotEnabled -> logFedError x (errorToResponse @'MLSNotEnabled)
    handleError (Left (r, e)) = logFedError r (toResponse e)

    logFedError :: (Member P.TinyLog r) => Remote x -> JSONResponse -> Sem r ()
    logFedError r e =
      P.warn $
        Logger.msg ("A welcome message could not be delivered to a remote backend" :: ByteString)
          . Logger.field "remote_domain" (domainText (tDomain r))
          . Logger.field "error" (A.encode e.value)
