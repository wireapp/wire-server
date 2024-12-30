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
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Galley.API.LegalHold.Conflicts
  ( guardQualifiedLegalholdPolicyConflicts,
    guardLegalholdPolicyConflicts,
    LegalholdConflicts (LegalholdConflicts),
    LegalholdConflictsOldClients (LegalholdConflictsOldClients),
  )
where

import Control.Lens (to, view, (^.))
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.Map qualified as Map
import Data.Misc
import Data.Qualified
import Data.Set qualified as Set
import Galley.API.Util
import Galley.Effects
import Galley.Effects.BrigAccess
import Galley.Effects.TeamStore
import Galley.Options
import Galley.Types.Teams
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.TinyLog qualified as P
import System.Logger.Class qualified as Log
import Wire.API.Team.Feature
import Wire.API.Team.LegalHold
import Wire.API.Team.Member
import Wire.API.User
import Wire.API.User.Client as Client

data LegalholdConflicts = LegalholdConflicts

data LegalholdConflictsOldClients = LegalholdConflictsOldClients

guardQualifiedLegalholdPolicyConflicts ::
  ( Member BrigAccess r,
    Member (Error LegalholdConflicts) r,
    Member (Input (Local ())) r,
    Member (Input Opts) r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  LegalholdProtectee ->
  QualifiedUserClients ->
  Sem r ()
guardQualifiedLegalholdPolicyConflicts protectee qclients = do
  localDomain <- tDomain <$> qualifyLocal ()
  guardLegalholdPolicyConflicts protectee
    . UserClients
    . Map.findWithDefault mempty localDomain
    . qualifiedUserClients
    $ qclients

-- | If user has legalhold status `no_consent` or has client devices that have no legalhold
-- capability, and some of the clients she is about to get connected are LH devices, respond
-- with 412 and do not process notification.
--
-- This is a fallback safeguard that shouldn't get triggered if backend and clients work as
-- intended.
guardLegalholdPolicyConflicts ::
  ( Member BrigAccess r,
    Member (Error LegalholdConflicts) r,
    Member (Input Opts) r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  LegalholdProtectee ->
  UserClients ->
  Sem r ()
guardLegalholdPolicyConflicts LegalholdPlusFederationNotImplemented _otherClients = pure ()
guardLegalholdPolicyConflicts UnprotectedBot _otherClients = pure ()
guardLegalholdPolicyConflicts (ProtectedUser self) otherClients = do
  opts <- input
  case view (settings . featureFlags . to npProject) opts of
    FeatureLegalHoldDisabledPermanently -> case FutureWork @'LegalholdPlusFederationNotImplemented () of
      FutureWork () ->
        -- FUTUREWORK: once we support federation and LH in combination, we still need to run
        -- the guard to protect against other federating instances running LH!  see also:
        -- LegalholdPlusFederationNotImplemented
        pure ()
    FeatureLegalHoldDisabledByDefault -> guardLegalholdPolicyConflictsUid self otherClients
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> guardLegalholdPolicyConflictsUid self otherClients

guardLegalholdPolicyConflictsUid ::
  forall r.
  ( Member BrigAccess r,
    Member (Error LegalholdConflicts) r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  UserId ->
  UserClients ->
  Sem r ()
guardLegalholdPolicyConflictsUid self (Map.keys . userClients -> otherUids) = do
  allClients :: UserClientsFull <- lookupClientsFull (nub $ self : otherUids)

  let allClientsMetadata :: [Client.Client]
      allClientsMetadata =
        allClients
          & Client.userClientsFull
          & Map.elems
          & Set.unions
          & Set.toList

      anyClientHasLH :: Bool
      anyClientHasLH = Client.LegalHoldClientType `elem` (Client.clientType <$> allClientsMetadata)

      checkAnyConsentMissing :: Sem r Bool
      checkAnyConsentMissing = do
        users <- getUsers (self : otherUids)
        -- NB: `users` can't be empty!
        let checkUserConsentMissing :: User -> Sem r Bool
            checkUserConsentMissing user =
              case userTeam user of
                Just tid -> do
                  mbMem <- getTeamMember tid (Wire.API.User.userId user)
                  case mbMem of
                    Nothing -> pure True -- it's weird that there is a member id but no member, we better bail
                    Just mem -> pure $ case mem ^. legalHoldStatus of
                      UserLegalHoldDisabled -> False
                      UserLegalHoldPending -> False
                      UserLegalHoldEnabled -> False
                      UserLegalHoldNoConsent -> True
                Nothing -> do
                  pure True -- personal users can not give consent
        or <$> checkUserConsentMissing `mapM` users

  P.debug $
    Log.field "self" (toByteString' self)
      Log.~~ Log.field "allClients" (toByteString' $ show allClients)
      Log.~~ Log.field "allClientsMetadata" (toByteString' $ show allClientsMetadata)
      Log.~~ Log.field "anyClientHasLH" (toByteString' anyClientHasLH)
      Log.~~ Log.msg ("guardLegalholdPolicyConflicts[1]" :: Text)

  -- when no other client is under LH, then we're good and can leave this function.  but...
  when anyClientHasLH $ do
    P.debug $ Log.msg ("guardLegalholdPolicyConflicts[5]: anyClientHasLH" :: Text)
    whenM checkAnyConsentMissing $ do
      P.debug $ Log.msg ("guardLegalholdPolicyConflicts[4]: checkConsentMissing!" :: Text)
      throw LegalholdConflicts
