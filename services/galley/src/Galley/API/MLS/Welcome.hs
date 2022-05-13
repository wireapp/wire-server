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

module Galley.API.MLS.Welcome (postMLSWelcome) where

import Control.Comonad
import Data.Id
import Data.Json.Util
import Data.Qualified
import Data.Tagged
import Data.Time
import Galley.API.MLS.KeyPackage
import Galley.API.Push
import Galley.API.Util
import Galley.Data.Conversation
import Galley.Effects.BrigAccess
import Galley.Effects.FederatorAccess
import Galley.Effects.GundeckAccess
import Galley.Effects.TeamStore
import Galley.Types.UserList
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Event.Conversation
import Wire.API.Federation.API
import Wire.API.Federation.API.Galley
import Wire.API.MLS.Credential
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Welcome

postMLSWelcome ::
  Members
    '[ BrigAccess,
       ErrorS 'ConvAccessDenied,
       ErrorS 'MLSKeyPackageRefNotFound,
       ErrorS 'NotConnected,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime,
       TeamStore
     ]
    r =>
  Local UserId ->
  ConnId ->
  RawMLS Welcome ->
  Sem r ()
postMLSWelcome lusr con wel = do
  rcpts <- welcomeRecipients (rmValue wel)
  let urcpts = fst <$$> rcpts
  ensureConnectedOrSameTeam lusr $ toUserList lusr urcpts
  traverse_ (sendWelcomes lusr con (rmRaw wel)) (bucketQualified rcpts)

welcomeRecipients ::
  Members
    '[ BrigAccess,
       ErrorS 'MLSKeyPackageRefNotFound
     ]
    r =>
  Welcome ->
  Sem r [Qualified (UserId, ClientId)]
welcomeRecipients =
  traverse
    ( fmap cidQualifiedClient
        . derefKeyPackage
        . gsNewMember
    )
    . welSecrets

sendWelcomes ::
  Members
    '[ ErrorS 'ConvAccessDenied,
       FederatorAccess,
       GundeckAccess,
       Input UTCTime
     ]
    r =>
  Local UserId ->
  ConnId ->
  ByteString ->
  Qualified [(UserId, ClientId)] ->
  Sem r ()
sendWelcomes lusr con rawWelcome recipients = do
  now <- input
  foldQualified lusr (sendLocalWelcomes con now rawWelcome) (sendRemoteWelcomes lusr rawWelcome) recipients

sendLocalWelcomes ::
  Members '[GundeckAccess] r =>
  ConnId ->
  UTCTime ->
  ByteString ->
  Local [(UserId, ClientId)] ->
  Sem r ()
sendLocalWelcomes con now rawWelcome lclients = do
  runMessagePush lclients Nothing $
    foldMap (uncurry mkPush) (tUnqualified lclients)
  where
    mkPush :: UserId -> ClientId -> MessagePush 'Broadcast
    mkPush u c =
      -- FUTUREWORK: use the conversation ID stored in the key package mapping table
      let lcnv = qualifyAs lclients (selfConv u)
          lusr = qualifyAs lclients u
          e = Event (qUntagged lcnv) (qUntagged lusr) now $ EdMLSWelcome rawWelcome
       in newMessagePush lclients () (Just con) defMessageMetadata (u, c) e

sendRemoteWelcomes ::
  Members
    '[ ErrorS 'ConvAccessDenied,
       FederatorAccess
     ]
    r =>
  Local UserId ->
  ByteString ->
  Remote [(UserId, ClientId)] ->
  Sem r ()
sendRemoteWelcomes lusr rawWelcome rClients = do
  let req =
        MLSWelcomeRequest
          { mwrSender = tUnqualified lusr,
            mwrRawWelcome = Base64ByteString rawWelcome,
            mwrRecipients = MLSWelcomeRecipient <$> tUnqualified rClients
          }
      rpc = fedClient @'Galley @"mls-welcome" req
  runFederated rClients rpc >>= mapFedError
  where
    mapFedError :: Member (ErrorS 'ConvAccessDenied) r => MLSWelcomeResponse -> Sem r ()
    mapFedError =
      mapError @MLSWelcomeError @(Tagged 'ConvAccessDenied ()) (const (Tagged ()))
        . fromEither
        . unWelcomeResponse
