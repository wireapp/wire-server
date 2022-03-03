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

module Galley.API.MLS (postMLSWelcome) where

import Control.Comonad
import Data.Id
import Data.Qualified
import Data.Time
import Galley.API.Error
import Galley.API.Push
import Galley.Data.Conversation
import Galley.Effects.BrigAccess
import Galley.Effects.GundeckAccess
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.API.ErrorDescription
import Wire.API.Event.Conversation
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Welcome

postMLSWelcome ::
  Members
    '[ BrigAccess,
       GundeckAccess,
       Error UnknownWelcomeRecipient,
       Input UTCTime
     ]
    r =>
  Local UserId ->
  RawMLS Welcome ->
  Sem r ()
postMLSWelcome lusr wel = do
  now <- input
  rcpts <- welcomeRecipients (extract wel)
  traverse_ (sendWelcomes now lusr (rmRaw wel)) (bucketQualified rcpts)

welcomeRecipients ::
  Members
    '[ BrigAccess,
       Error UnknownWelcomeRecipient
     ]
    r =>
  Welcome ->
  Sem r [Qualified (UserId, ClientId)]
welcomeRecipients = traverse (fmap cidQualifiedClient . derefKeyPackage . gsNewMember) . welSecrets

sendWelcomes ::
  Members '[GundeckAccess] r =>
  UTCTime ->
  Local x ->
  ByteString ->
  Qualified [(UserId, ClientId)] ->
  Sem r ()
sendWelcomes now loc rawWelcome =
  foldQualified loc (sendLocalWelcomes now rawWelcome) (sendRemoteWelcomes rawWelcome)

sendLocalWelcomes ::
  Members '[GundeckAccess] r =>
  UTCTime ->
  ByteString ->
  Local [(UserId, ClientId)] ->
  Sem r ()
sendLocalWelcomes now rawWelcome lclients = do
  runMessagePush lclients Nothing $
    foldMap (uncurry mkPush) (tUnqualified lclients)
  where
    -- TODO: add ConnId header to endpoint
    mkPush :: UserId -> ClientId -> MessagePush 'Broadcast
    mkPush u c =
      -- FUTUREWORK: use the conversation ID stored in the key package mapping table
      let lcnv = qualifyAs lclients (selfConv u)
          lusr = qualifyAs lclients u
          e = Event (qUntagged lcnv) (qUntagged lusr) now $ EdMLSMessage rawWelcome
       in newMessagePush lclients () Nothing defMessageMetadata (u, c) e

sendRemoteWelcomes :: ByteString -> Remote [(UserId, ClientId)] -> Sem r ()
sendRemoteWelcomes = undefined

derefKeyPackage ::
  Members
    '[ BrigAccess,
       Error UnknownWelcomeRecipient
     ]
    r =>
  KeyPackageRef ->
  Sem r ClientIdentity
derefKeyPackage ref =
  maybe (throwED @UnknownWelcomeRecipient) pure
    =<< getClientByKeyPackageRef ref
