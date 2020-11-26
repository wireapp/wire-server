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

module Brig.API.Util
  ( fetchUserIdentity,
    isFederationEnabled,
    lookupProfilesMaybeFilterSameTeamOnly,
    lookupSelfProfile,
    validateHandle,
    viewFederationDomain,
  )
where

import qualified Brig.API.Error as Error
import Brig.API.Handler
import Brig.API.Types
import Brig.App (AppIO, Env, settings)
import qualified Brig.Data.User as Data
import Brig.Options (FederationAllowList (..), FederationStrategy (..), federationDomain, federationStrategy)
import Brig.Types
import Brig.Types.Intra (accountUser)
import Control.Lens (view)
import Control.Monad.Catch (throwM)
import Control.Monad.Trans.Except (throwE)
import Data.Domain (Domain)
import Data.Handle (Handle, parseHandle)
import Data.Id
import Data.Maybe
import Imports

lookupProfilesMaybeFilterSameTeamOnly :: UserId -> [UserProfile] -> Handler [UserProfile]
lookupProfilesMaybeFilterSameTeamOnly self us = do
  selfTeam <- lift $ Data.lookupUserTeam self
  return $ case selfTeam of
    Just team -> filter (\x -> profileTeam x == Just team) us
    Nothing -> us

fetchUserIdentity :: UserId -> AppIO (Maybe UserIdentity)
fetchUserIdentity uid =
  lookupSelfProfile uid
    >>= maybe
      (throwM $ UserProfileNotFound uid)
      (return . userIdentity . selfUser)

-- | Obtain a profile for a user as he can see himself.
lookupSelfProfile :: UserId -> AppIO (Maybe SelfProfile)
lookupSelfProfile = fmap (fmap mk) . Data.lookupAccount
  where
    mk a = SelfProfile (accountUser a)

validateHandle :: Text -> Handler Handle
validateHandle = maybe (throwE (Error.StdError Error.invalidHandle)) return . parseHandle

--------------------------------------------------------------------------------
-- Federation

viewFederationDomain :: MonadReader Env m => m (Domain)
viewFederationDomain = view (settings . federationDomain)

isFederationEnabled :: MonadReader Env m => Domain -> m Bool
isFederationEnabled targetDomain = do
  ourDomain <- viewFederationDomain
  strategy <- view (settings . federationStrategy)
  pure $ case strategy of
    WithEveryone -> True
    WithAllowList (FederationAllowList _) | targetDomain == ourDomain -> True
    WithAllowList (FederationAllowList domains) | targetDomain `elem` domains -> True
    WithAllowList _ -> False
