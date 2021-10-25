-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Galley.API.LegalHold.Conflicts where

import Brig.Types.Intra (accountUser)
import Control.Lens (view)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..), defUserLegalHoldStatus)
import qualified Data.Map as Map
import Data.Misc
import qualified Data.Set as Set
import Galley.API.Util
import Galley.App
import qualified Galley.Data as Data
import Galley.Effects
import qualified Galley.Intra.Client as Intra
import Galley.Intra.User (getUser)
import Galley.Options
import Galley.Types.Teams hiding (self)
import Imports
import qualified System.Logger.Class as Log
import Wire.API.Team.LegalHold
import Wire.API.User
import Wire.API.User.Client as Client

data LegalholdConflicts = LegalholdConflicts

guardQualifiedLegalholdPolicyConflicts ::
  Member BrigAccess r =>
  LegalholdProtectee ->
  QualifiedUserClients ->
  Galley r (Either LegalholdConflicts ())
guardQualifiedLegalholdPolicyConflicts protectee qclients = do
  localDomain <- viewFederationDomain
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
  Member BrigAccess r =>
  LegalholdProtectee ->
  UserClients ->
  Galley r (Either LegalholdConflicts ())
guardLegalholdPolicyConflicts LegalholdPlusFederationNotImplemented _otherClients = pure . pure $ ()
guardLegalholdPolicyConflicts UnprotectedBot _otherClients = pure . pure $ ()
guardLegalholdPolicyConflicts (ProtectedUser self) otherClients = do
  view (options . optSettings . setFeatureFlags . flagLegalHold) >>= \case
    FeatureLegalHoldDisabledPermanently -> case FutureWork @'LegalholdPlusFederationNotImplemented () of
      FutureWork () -> pure . pure $ () -- FUTUREWORK: if federation is enabled, we still need to run the guard!
    FeatureLegalHoldDisabledByDefault -> guardLegalholdPolicyConflictsUid self otherClients
    FeatureLegalHoldWhitelistTeamsAndImplicitConsent -> guardLegalholdPolicyConflictsUid self otherClients

guardLegalholdPolicyConflictsUid ::
  forall r.
  Member BrigAccess r =>
  UserId ->
  UserClients ->
  Galley r (Either LegalholdConflicts ())
guardLegalholdPolicyConflictsUid self otherClients = runExceptT $ do
  let otherCids :: [ClientId]
      otherCids = Set.toList . Set.unions . Map.elems . userClients $ otherClients

      otherUids :: [UserId]
      otherUids = nub $ Map.keys . userClients $ otherClients

  when (nub otherUids /= [self {- if all other clients belong to us, there can be no conflict -}]) $ do
    allClients :: UserClientsFull <- lift $ Intra.lookupClientsFull (nub $ self : otherUids)

    let selfClients :: [Client.Client] =
          allClients
            & Client.userClientsFull
            & Map.lookup self
            & fromMaybe Set.empty
            & Set.toList

        otherClientHasLH :: Bool
        otherClientHasLH =
          let clients =
                allClients
                  & Client.userClientsFull
                  & Map.delete self
                  & Map.elems
                  & Set.unions
                  & Set.toList
                  & filter ((`elem` otherCids) . Client.clientId)
           in Client.LegalHoldClientType `elem` (Client.clientType <$> clients)

        checkSelfHasLHClients :: Bool
        checkSelfHasLHClients =
          any ((== Client.LegalHoldClientType) . Client.clientType) selfClients

        checkSelfHasOldClients :: Bool
        checkSelfHasOldClients =
          any isOld selfClients
          where
            isOld :: Client.Client -> Bool
            isOld =
              (Client.ClientSupportsLegalholdImplicitConsent `Set.notMember`)
                . Client.fromClientCapabilityList
                . Client.clientCapabilities

        checkConsentMissing :: Galley r Bool
        checkConsentMissing = do
          -- (we could also get the profile from brig.  would make the code slightly more
          -- concise, but not really help with the rpc back-and-forth, so, like, why?)
          mbUser <- accountUser <$$> getUser self
          mbTeamMember <- join <$> for (mbUser >>= userTeam) (`Data.teamMember` self)
          let lhStatus = maybe defUserLegalHoldStatus (view legalHoldStatus) mbTeamMember
          pure (lhStatus == UserLegalHoldNoConsent)

    lift $
      Log.debug $
        Log.field "self" (toByteString' self)
          Log.~~ Log.field "otherClients" (toByteString' $ show otherClients)
          Log.~~ Log.field "otherClientHasLH" (toByteString' otherClientHasLH)
          Log.~~ Log.field "checkSelfHasOldClients" (toByteString' checkSelfHasOldClients)
          Log.~~ Log.field "checkSelfHasLHClients" (toByteString' checkSelfHasLHClients)
          Log.~~ Log.msg ("guardLegalholdPolicyConflicts[1]" :: Text)

    -- (I've tried to order the following checks for minimum IO; did it work?  ~~fisx)
    when otherClientHasLH $ do
      when checkSelfHasOldClients $ do
        lift $ Log.debug $ Log.msg ("guardLegalholdPolicyConflicts[2]: old clients" :: Text)
        throwError LegalholdConflicts

      unless checkSelfHasLHClients {- carrying a LH device implies having granted LH consent -} $ do
        whenM (lift checkConsentMissing) $ do
          -- We assume this is impossible, since conversations are automatically
          -- blocked if LH consent is missing of any participant.
          -- We add this check here as an extra failsafe.
          lift $ Log.debug $ Log.msg ("guardLegalholdPolicyConflicts[3]: consent missing" :: Text)
          throwError LegalholdConflicts
