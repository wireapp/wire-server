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

module Brig.User.API.Handle
  ( getHandleInfo,
    getLocalHandleInfo,
    filterHandleResults,
    contactFromProfile,
  )
where

import Brig.API.Error (fedError)
import Brig.API.Handler (Handler)
import qualified Brig.API.User as API
import Brig.App
import qualified Brig.Data.User as Data
import Brig.Effects.GalleyProvider (GalleyProvider)
import qualified Brig.Federation.Client as Federation
import Brig.Options (searchSameTeamOnly)
import Control.Lens (view)
import Data.Handle (Handle, fromHandle)
import Data.Id (UserId)
import Data.Qualified
import Imports
import Network.Wai.Utilities ((!>>))
import Polysemy
import qualified System.Logger.Class as Log
import Wire.API.User
import qualified Wire.API.User as Public
import Wire.API.User.Search
import qualified Wire.API.User.Search as Public

getHandleInfo ::
  Members '[GalleyProvider] r =>
  UserId ->
  Qualified Handle ->
  (Handler r) (Maybe Public.UserProfile)
getHandleInfo self handle = do
  lself <- qualifyLocal self
  foldQualified
    lself
    (getLocalHandleInfo lself . tUnqualified)
    getRemoteHandleInfo
    handle

getRemoteHandleInfo :: Remote Handle -> (Handler r) (Maybe Public.UserProfile)
getRemoteHandleInfo handle = do
  lift . Log.info $
    Log.msg (Log.val "getHandleInfo - remote lookup")
      . Log.field "domain" (show (tDomain handle))
  Federation.getUserHandleInfo handle !>> fedError

getLocalHandleInfo ::
  Members '[GalleyProvider] r =>
  Local UserId ->
  Handle ->
  (Handler r) (Maybe Public.UserProfile)
getLocalHandleInfo self handle = do
  lift . Log.info $ Log.msg $ Log.val "getHandleInfo - local lookup"
  maybeOwnerId <- lift . wrapClient $ API.lookupHandle handle
  case maybeOwnerId of
    Nothing -> pure Nothing
    Just ownerId -> do
      domain <- viewFederationDomain
      ownerProfile <- (API.lookupProfile self (Qualified ownerId domain)) !>> fedError
      owner <- filterHandleResults self (maybeToList ownerProfile)
      pure $ listToMaybe owner

-- | Checks search permissions and filters accordingly
filterHandleResults :: Local UserId -> [Public.UserProfile] -> (Handler r) [Public.UserProfile]
filterHandleResults searchingUser us = do
  sameTeamSearchOnly <- fromMaybe False <$> view (settings . searchSameTeamOnly)
  if sameTeamSearchOnly
    then do
      fromTeam <- lift . wrapClient $ Data.lookupUserTeam (tUnqualified searchingUser)
      pure $ case fromTeam of
        Just team -> filter (\x -> Public.profileTeam x == Just team) us
        Nothing -> us
    else pure us

contactFromProfile :: Public.UserProfile -> Public.Contact
contactFromProfile profile =
  Contact
    { contactQualifiedId = profileQualifiedId profile,
      contactName = fromName $ profileName profile,
      contactHandle = fromHandle <$> profileHandle profile,
      contactColorId = Just . fromIntegral . fromColourId $ profileAccentId profile,
      contactTeam = profileTeam profile
    }
