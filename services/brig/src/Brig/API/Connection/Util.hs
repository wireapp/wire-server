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

module Brig.API.Connection.Util
  ( ConnectionM,
    checkLimit,
    ensureIsActivated,
    ensureNotSameAndActivated,
    ensureNotSameTeam,
    ensureNoApps,
  )
where

import Brig.API.Types
import Brig.App
import Brig.Data.Connection qualified as Data
import Brig.Options (Settings (userMaxConnections))
import Control.Error (MaybeT, noteT)
import Control.Monad.Trans.Except
import Data.Id (UserId)
import Data.Qualified
import Imports
import Polysemy
import Wire.API.Connection (Relation (..))
import Wire.API.User
import Wire.GalleyAPIAccess
import Wire.UserStore
import Wire.UserSubsystem

type ConnectionM r = ExceptT ConnectionError (AppT r)

-- Helpers

checkLimit :: Local UserId -> ExceptT ConnectionError (AppT r) ()
checkLimit u = noteT (TooManyConnections (tUnqualified u)) $ do
  n <- lift . wrapClient $ Data.countConnections u [Accepted, Sent]
  l <- asks (.settings.userMaxConnections)
  guard (n < l)

ensureNotSameAndActivated :: (Member UserStore r) => Local UserId -> Qualified UserId -> ConnectionM r ()
ensureNotSameAndActivated self target = do
  when (tUntagged self == target) $
    throwE (InvalidUser target)
  noteT ConnectNoIdentity $
    ensureIsActivated self

ensureIsActivated :: (Member UserStore r) => Local UserId -> MaybeT (AppT r) ()
ensureIsActivated lusr = do
  active <- lift . liftSem $ isActivated (tUnqualified lusr)
  guard active

ensureNotSameTeam :: (Member GalleyAPIAccess r) => Local UserId -> Local UserId -> (ConnectionM r) ()
ensureNotSameTeam self target = do
  selfTeam <- lift $ liftSem $ getTeamId (tUnqualified self)
  targetTeam <- lift $ liftSem $ getTeamId (tUnqualified target)
  when (isJust selfTeam && selfTeam == targetTeam) $
    throwE ConnectSameBindingTeamUsers

ensureNoApps :: (Member UserSubsystem r) => Local UserId -> [Qualified UserId] -> (ConnectionM r) ()
ensureNoApps _ [] = pure ()
ensureNoApps asker uids@(_ : _) = do
  apps :: [Qualified UserId] <-
    catMaybes <$> do
      let go prof = case prof.profileType of
            UserTypeApp -> Just prof.profileQualifiedId
            UserTypeRegular -> Nothing
            UserTypeBot -> Nothing
      lift $ liftSem $ go <$$> getUserProfiles asker uids

  case apps of
    [] -> pure ()
    (hd : _) -> throwE (InvalidUser hd)
