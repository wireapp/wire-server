{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

-- | Client functions for interacting with the Galley API.
module Spar.Intra.Galley where

import Bilge
import Control.Lens
import Control.Monad.Except
import Data.ByteString.Conversion
import Data.Id (TeamId, UserId)
import Data.String.Conversions (cs)
import Galley.Types.Teams
import Imports
import Network.HTTP.Types.Method
import Spar.Error
import qualified System.Logger.Class as Log
import Wire.API.Team.Feature (TeamFeatureName (..), TeamFeatureStatus, TeamFeatureStatusNoConfig (..), TeamFeatureStatusValue (..))

----------------------------------------------------------------------

class (Monad m, Log.MonadLogger m) => MonadSparToGalley m where
  call :: (Request -> Request) -> m ResponseLBS

-- | Get all members of a team.
getTeamMembers ::
  (HasCallStack, MonadError SparError m, MonadSparToGalley m) =>
  TeamId ->
  m [TeamMember]
getTeamMembers tid = do
  resp :: ResponseLBS <-
    call $
      method GET
        . paths ["i", "teams", toByteString' tid, "members"]
  if (statusCode resp == 200)
    then (^. teamMembers) <$> parseResponse @TeamMemberList "galley" resp
    else rethrow "galley" resp

-- | user is member of a given team and has a given permission there.
assertHasPermission ::
  (HasCallStack, MonadSparToGalley m, MonadError SparError m, IsPerm perm, Show perm) =>
  TeamId ->
  perm ->
  UserId ->
  m ()
assertHasPermission tid perm uid = do
  resp <-
    call $
      method GET
        . (paths ["i", "teams", toByteString' tid, "members", toByteString' uid])
  case (statusCode resp, parseResponse @TeamMember "galley" resp) of
    (200, Right member) | hasPermission member perm -> pure ()
    _ -> throwSpar (SparNoPermission (cs $ show perm))

assertSSOEnabled ::
  (HasCallStack, MonadError SparError m, MonadSparToGalley m) =>
  TeamId ->
  m ()
assertSSOEnabled tid = do
  resp :: ResponseLBS <-
    call $
      method GET
        . paths ["i", "teams", toByteString' tid, "features", "sso"]
  unless (statusCode resp == 200) $
    rethrow "galley" resp
  TeamFeatureStatusNoConfig status <- parseResponse "galley" resp
  unless (status == TeamFeatureEnabled) $
    throwSpar SparSSODisabled

isEmailValidationEnabledTeam :: (HasCallStack, MonadSparToGalley m) => TeamId -> m Bool
isEmailValidationEnabledTeam tid = do
  resp <- call $ method GET . paths ["i", "teams", toByteString' tid, "features", "validateSAMLemails"]
  pure
    ( (statusCode resp == 200)
        && ( responseJsonMaybe @(TeamFeatureStatus 'TeamFeatureValidateSAMLEmails) resp
               == Just (TeamFeatureStatusNoConfig TeamFeatureEnabled)
           )
    )

