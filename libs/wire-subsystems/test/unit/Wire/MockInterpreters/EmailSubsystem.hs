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

module Wire.MockInterpreters.EmailSubsystem where

import Data.Map qualified as Map
import Imports
import Polysemy
import Polysemy.State
import Wire.API.User
import Wire.EmailSubsystem

data SentMail = SentMail
  { locale :: Maybe Locale,
    content :: SentMailContent
  }
  deriving (Show, Eq)

data SentMailContent = PasswordResetMail PasswordResetPair
  deriving (Show, Eq)

inMemoryEmailSubsystemInterpreter :: (Member (State (Map EmailAddress [SentMail])) r) => InterpreterFor EmailSubsystem r
inMemoryEmailSubsystemInterpreter = interpret \case
  SendPasswordResetMail email keyCodePair mLocale -> modify $ Map.insertWith (<>) email [SentMail mLocale $ PasswordResetMail keyCodePair]
  _ -> error "inMemoryEmailSubsystemInterpreter: implement on demand"

getEmailsSentTo :: (Member (State (Map EmailAddress [SentMail])) r) => EmailAddress -> Sem r [SentMail]
getEmailsSentTo email = gets $ Map.findWithDefault [] email

noopEmailSubsystemInterpreter :: InterpreterFor EmailSubsystem r
noopEmailSubsystemInterpreter = interpret \case
  SendPasswordResetMail {} -> pure ()
  SendVerificationMail {} -> pure ()
  SendCreateScimTokenVerificationMail {} -> pure ()
  SendLoginVerificationMail {} -> pure ()
  SendActivationMail {} -> pure ()
  SendEmailAddressUpdateMail {} -> pure ()
  SendNewClientEmail {} -> pure ()
  SendAccountDeletionEmail {} -> pure ()
  SendTeamActivationMail {} -> pure ()
  SendTeamDeletionVerificationMail {} -> pure ()
  SendTeamInvitationMail {} -> pure ""
  SendTeamInvitationMailPersonalUser {} -> pure ""
