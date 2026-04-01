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
import Brig.API.User qualified as API
import Brig.App
import Brig.Options (searchSameTeamOnly)
import Data.Handle (Handle, fromHandle)
import Data.Id (UserId)
import Data.Qualified
import Imports
import Network.Wai.Utilities ((!>>))
import Polysemy
import Polysemy.Error (Error)
import System.Logger.Class qualified as Log
import Servant.Client.Core (RunClient)
import Wire.API.Component
import Wire.API.Federation.API (FederationMonad, fedClient)
import Wire.API.Federation.Error
import Wire.API.User
import Wire.API.User qualified as Public
import Wire.API.User.Search
import Wire.API.User.Search qualified as Public
import Wire.FederationAPIAccess (FederationAPIAccess, runFederated)
import Wire.UserStore (UserStore)
import Wire.UserStore qualified as UserStore
import Wire.UserSubsystem

getHandleInfo ::
  forall r m.
  ( Member UserSubsystem r,
    Member UserStore r,
    Member (FederationAPIAccess m) r,
    RunClient (m 'Brig),
    FederationMonad m,
    Typeable m,
    Member (Error FederationError) r
  ) =>
  UserId ->
  Qualified Handle ->
  Handler r (Maybe Public.UserProfile)
getHandleInfo self handle = do
  lself <- qualifyLocal self
  foldQualified
    lself
    (getLocalHandleInfo lself . tUnqualified)
    getRemoteHandleInfo
    handle

getRemoteHandleInfo ::
  forall r m.
  ( Member (FederationAPIAccess m) r,
    RunClient (m 'Brig),
    FederationMonad m,
    Typeable m,
    Member (Error FederationError) r
  ) =>
  Remote Handle ->
  Handler r (Maybe Public.UserProfile)
getRemoteHandleInfo handle = do
  lift . Log.info $
    Log.msg (Log.val "getHandleInfo - remote lookup")
      . Log.field "domain" (show (tDomain handle))
  lift . liftSem $
    runFederated handle $
      fedClient @'Brig @"get-user-by-handle" (tUnqualified handle)

getLocalHandleInfo ::
  (Member UserSubsystem r, Member UserStore r) =>
  Local UserId ->
  Handle ->
  Handler r (Maybe Public.UserProfile)
getLocalHandleInfo self handle = do
  lift . Log.info $ Log.msg $ Log.val "getHandleInfo - local lookup"
  maybeOwnerId <- lift . liftSem $ API.lookupHandle handle
  case maybeOwnerId of
    Nothing -> pure Nothing
    Just ownerId -> do
      domain <- viewFederationDomain
      ownerProfile <-
        (lift . liftSem $ getUserProfile self (Qualified ownerId domain))
          !>> fedError
      owner <- filterHandleResults self (maybeToList ownerProfile)
      pure $ listToMaybe owner

-- | Checks search permissions and filters accordingly
filterHandleResults :: (Member UserStore r) => Local UserId -> [Public.UserProfile] -> Handler r [Public.UserProfile]
filterHandleResults searchingUser us = do
  sameTeamSearchOnly <- fromMaybe False <$> asks (.settings.searchSameTeamOnly)
  if sameTeamSearchOnly
    then do
      fromTeam <- lift . liftSem $ UserStore.getUserTeam (tUnqualified searchingUser)
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
      contactTeam = profileTeam profile,
      contactType = profileType profile
    }
