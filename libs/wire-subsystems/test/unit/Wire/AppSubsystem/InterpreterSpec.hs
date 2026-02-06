{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns #-}

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

module Wire.AppSubsystem.InterpreterSpec (spec) where

import Control.Lens ((.~))
import Control.Lens.At ()
import Data.Bifunctor (first)
import Data.Coerce
import Data.Default (Default (def))
import Data.Domain
import Data.Handle
import Data.HashSet qualified as HashSet
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import Data.Map qualified as Map
import Data.Misc
import Data.Proxy
import Data.Qualified
import Data.Range
import Data.Set (insert, member, notMember)
import Data.Set qualified as S
import Data.String.Conversions (cs)
import Data.Text.Encoding (encodeUtf8)
import Database.Bloodhound.Internal.Client qualified as ES
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Internal
import Polysemy.State
import SAML2.WebSSO qualified as SAML
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.App as App
import Wire.API.EnterpriseLogin
import Wire.API.Federation.Error
import Wire.API.Team.Collaborator
import Wire.API.Team.Feature (FeatureStatus (..), LockStatus (..), LockableFeature (..), MlsE2EIdConfig, npUpdate)
import Wire.API.Team.Member
import Wire.API.Team.Permission
import Wire.API.Team.Role
import Wire.API.User hiding (DeleteUser)
import Wire.API.User.IdentityProvider (IdPList (..), team)
import Wire.API.User.Password
import Wire.API.User.Search
import Wire.API.UserEvent qualified as Event
import Wire.AppSubsystem
import Wire.AppSubsystem.Interpreter
import Wire.AuthenticationSubsystem
import Wire.AuthenticationSubsystem.Error
import Wire.DomainRegistrationStore qualified as DRS
import Wire.IndexedUserStore qualified as IU
import Wire.InvitationStore (InsertInvitation, StoredInvitation)
import Wire.InvitationStore qualified as InvitationStore
import Wire.MiniBackend
import Wire.MockInterpreters
import Wire.RateLimit
import Wire.StoredUser
import Wire.UserKeyStore
import Wire.UserSearch.Types
import Wire.UserStore.IndexUser
import Wire.UserSubsystem
import Wire.UserSubsystem.Error
import Wire.UserSubsystem.HandleBlacklist
import Wire.UserSubsystem.Interpreter (UserSubsystemConfig (..))
import Wire.Util

spec :: Spec
spec = describe "AppSubsystem.Interpreter" do
  focus . prop "admin or owner can create, get, delete apps" $
    \(config :: UserSubsystemConfig)
     (ownDomain :: Domain)
     (owner_ :: StoredUser)
     (tid :: TeamId)
     (EligibleRole role)
     (newApp_ :: App.NewApp) -> do
        let localBackend :: MiniBackend
            localBackend = def {users = [owner]}
            teamMap = Map.singleton tid [mem]
              where
                perms = rolePermissions role
                mem = mkTeamMember owner.id perms Nothing UserLegalHoldDisabled
            owner =
              (owner_ :: StoredUser)
                { teamId = Just tid,
                  email = Just emailAddress
                }
            emailAddress = todo
            lOwner = toLocalUnsafe ownDomain owner.id
            newApp =
              (newApp_ :: App.NewApp)
                { password = plainTextPassword6Unsafe "gihowi87!1A"
                }
         in runNoFederationStack localBackend teamMap config $
              do
                setPassword emailAddress newApp.password
                created :: CreatedApp <- createApp lOwner tid newApp
                --        Unexpected error: AuthenticationSubsystemBadCredentials
                () <- error $ "**********************" <> show created
                -- got <- getApp lOwner tid created.app.id

                -- delete
                pure $ True === False

-- TODO: move it to a helper section in the effect interface module?
setPassword ::
  forall r.
  ( Member AuthenticationSubsystem r,
    Member (State (Map EmailAddress [SentMail])) r
  ) =>
  EmailAddress -> PlainTextPassword6 -> Sem r ()
setPassword email (plainTextPassword8Unsafe . fromPlainTextPassword -> newPassword) = do
  let emailKey = todo email
  createPasswordResetCode emailKey
  (_, resetCode) <- expect1ResetPasswordEmail email
  resetPassword (PasswordResetEmailIdentity email) resetCode newPassword

-- TODO: this is duplicated from Wire.AuthenticationSubsystem.InterpreterSpec, maybe move it to a helper section in the effect interface module?
expect1ResetPasswordEmail :: (Member (State (Map EmailAddress [SentMail])) r) => EmailAddress -> Sem r PasswordResetPair
expect1ResetPasswordEmail email =
  getEmailsSentTo email
    <&> \case
      [] -> error "no emails sent"
      [SentMail _ (PasswordResetMail resetPair)] -> resetPair
      wrongEmails -> error $ "Wrong emails sent: " <> show wrongEmails

eligibleRoles :: [Role]
eligibleRoles = [RoleAdmin, RoleOwner]

newtype EligibleRole = EligibleRole Role
  deriving (Eq, Show)

instance Arbitrary EligibleRole where
  arbitrary = EligibleRole <$> elements eligibleRoles
