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
import Galley.API.Error
import Galley.Effects.BrigAccess
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.ErrorDescription
import Wire.API.MLS.Credential
import Wire.API.MLS.KeyPackage
import Wire.API.MLS.Serialisation
import Wire.API.MLS.Welcome

postMLSWelcome ::
  Members
    '[ BrigAccess,
       Error UnknownWelcomeRecipient
     ]
    r =>
  Local UserId ->
  RawMLS Welcome ->
  Sem r ()
postMLSWelcome _ wel = do
  rcpts <- welcomeRecipients (extract wel)
  traverse_ (sendWelcome (rmRaw wel)) rcpts

welcomeRecipients ::
  Members
    '[ BrigAccess,
       Error UnknownWelcomeRecipient
     ]
    r =>
  Welcome ->
  Sem r [ClientIdentity]
welcomeRecipients = traverse (derefKeyPackage . gsNewMember) . welSecrets

sendWelcome :: ByteString -> ClientIdentity -> Sem r ()
sendWelcome _ _ = pure ()

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
