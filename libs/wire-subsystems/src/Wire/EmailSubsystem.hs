{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

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

module Wire.EmailSubsystem where

import Data.Code qualified as Code
import Data.Id
import Data.Json.Util
import Data.X509.Extended (CertDescription)
import Imports
import Polysemy
import SAML2.WebSSO
import URI.ByteString (URI)
import Wire.API.Locale
import Wire.API.User
import Wire.API.User.Activation (ActivationCode, ActivationKey)
import Wire.API.User.Client (Client (..))

-- | A typed description of something that happened to an app in a team. Each
-- constructor corresponds to a distinct admin-notification email (and template).
--
-- See https://www.figma.com/design/AMNqFhTUElOZDJfbUYnMmJ/Apps--Services----Integrations?node-id=431-2452
data AppEvent
  = NewAppCreated -- app-creation
      { actor :: Text,
        appName :: Name,
        date :: UTCTimeMillis,
        permissions :: Text,
        teamId :: TeamId,
        teamName :: Text
        -- TODO: team management url (${URL}) and ${support}?  is that included in branding?
      }
  | AppMetadataChanged
      { actor :: Text,
        date :: UTCTimeMillis,
        newAppName :: Name,
        previousAppName :: Name,
        teamId :: TeamId,
        teamName :: Text
        -- TODO: team management url (${URL}) and ${support}?  is that included in branding?
      }
  | AppTokenChanged
      { actor :: Text,
        appName :: Name,
        date :: UTCTimeMillis,
        teamId :: TeamId,
        teamName :: Text
        -- TODO: team management url (${URL}) and ${support}?  is that included in branding?
      }
  | AppDeleted
      { actor :: Text,
        appName :: Name,
        date :: UTCTimeMillis,
        teamId :: TeamId,
        teamName :: Text
        -- TODO: team management url (${URL}) and ${support}?  is that included in branding?
      }
  | AppAvailabilityChanged
      { actor :: Text,
        appName :: Name,
        date :: UTCTimeMillis,
        newAvailability :: Text,
        previousAvailability :: Text,
        teamId :: TeamId,
        teamName :: Text
        -- TODO: team management url (${URL}) and ${support}?  is that included in branding?
      }
  deriving (Eq, Show)

data EmailSubsystem m a where
  SendPasswordResetMail :: EmailAddress -> PasswordResetPair -> Maybe Locale -> EmailSubsystem m ()
  SendVerificationMail :: EmailAddress -> ActivationKey -> ActivationCode -> Maybe Locale -> EmailSubsystem m ()
  SendCreateScimTokenVerificationMail :: EmailAddress -> Code.Value -> Maybe Locale -> EmailSubsystem m ()
  SendLoginVerificationMail :: EmailAddress -> Code.Value -> Maybe Locale -> EmailSubsystem m ()
  SendActivationMail :: EmailAddress -> Name -> ActivationKey -> ActivationCode -> Maybe Locale -> EmailSubsystem m ()
  SendEmailAddressUpdateMail :: EmailAddress -> Name -> ActivationKey -> ActivationCode -> Maybe Locale -> EmailSubsystem m ()
  SendNewClientEmail :: EmailAddress -> Name -> Client -> Locale -> EmailSubsystem m ()
  SendAccountDeletionEmail :: EmailAddress -> Name -> Code.Key -> Code.Value -> Locale -> EmailSubsystem m ()
  SendTeamActivationMail :: EmailAddress -> Name -> ActivationKey -> ActivationCode -> Maybe Locale -> Text -> EmailSubsystem m ()
  SendTeamDeletionVerificationMail :: EmailAddress -> Code.Value -> Maybe Locale -> EmailSubsystem m ()
  -- | send invitation to an unknown email address.
  SendTeamInvitationMail :: EmailAddress -> TeamId -> EmailAddress -> InvitationCode -> Maybe Locale -> EmailSubsystem m Text
  -- | send invitation to an email address associated with a personal user account.
  SendTeamInvitationMailPersonalUser :: EmailAddress -> TeamId -> EmailAddress -> InvitationCode -> Maybe Locale -> EmailSubsystem m Text
  SendMemberWelcomeEmail :: EmailAddress -> TeamId -> Text -> Maybe Locale -> EmailSubsystem m ()
  SendNewTeamOwnerWelcomeEmail :: EmailAddress -> TeamId -> Text -> Maybe Locale -> Name -> EmailSubsystem m ()
  SendSAMLIdPChanged ::
    EmailAddress ->
    TeamId ->
    Maybe UserId ->
    [CertDescription] ->
    [CertDescription] ->
    IdPId ->
    Maybe Issuer ->
    Maybe URI ->
    Maybe Issuer ->
    Maybe URI ->
    Maybe Locale ->
    EmailSubsystem m ()
  SendAppEventEmail :: EmailAddress -> Name -> TeamId -> AppEvent -> Maybe Locale -> EmailSubsystem m ()

makeSem ''EmailSubsystem
