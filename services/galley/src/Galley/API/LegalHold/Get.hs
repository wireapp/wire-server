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

module Galley.API.LegalHold.Get (getUserStatus) where

import Control.Lens (view)
import Data.ByteString.Conversion (toByteString')
import Data.Id
import Data.LegalHold (UserLegalHoldStatus (..))
import Data.Qualified
import Galley.API.Error
import Galley.Effects
import Galley.Effects.LegalHoldStore qualified as LegalHoldData
import Galley.Effects.TeamStore
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.TinyLog qualified as P
import System.Logger.Class qualified as Log
import Wire.API.Error
import Wire.API.Error.Galley
import Wire.API.Team.LegalHold
import Wire.API.Team.LegalHold qualified as Public
import Wire.API.Team.Member
import Wire.API.User.Client.Prekey

-- | Learn whether a user has LH enabled and fetch pre-keys.
-- Note that this is accessible to ANY authenticated user, even ones outside the team
getUserStatus ::
  forall r.
  ( Member (Error InternalError) r,
    Member (ErrorS 'TeamMemberNotFound) r,
    Member LegalHoldStore r,
    Member TeamStore r,
    Member P.TinyLog r
  ) =>
  Local UserId ->
  TeamId ->
  UserId ->
  Sem r Public.UserLegalHoldStatusResponse
getUserStatus _lzusr tid uid = do
  teamMember <- noteS @'TeamMemberNotFound =<< getTeamMember tid uid
  let status = view legalHoldStatus teamMember
  (mlk, lcid) <- case status of
    UserLegalHoldNoConsent -> pure (Nothing, Nothing)
    UserLegalHoldDisabled -> pure (Nothing, Nothing)
    UserLegalHoldPending -> makeResponseDetails
    UserLegalHoldEnabled -> makeResponseDetails
  pure $ UserLegalHoldStatusResponse status mlk lcid
  where
    makeResponseDetails :: Sem r (Maybe LastPrekey, Maybe ClientId)
    makeResponseDetails = do
      mLastKey <- fmap snd <$> LegalHoldData.selectPendingPrekeys uid
      lastKey <- case mLastKey of
        Nothing -> do
          P.err
            . Log.msg
            $ "expected to find a prekey for user: "
              <> toByteString' uid
              <> " but none was found"
          throw NoPrekeyForUser
        Just lstKey -> pure lstKey
      let clientId = clientIdFromPrekey . unpackLastPrekey $ lastKey
      pure (Just lastKey, Just clientId)
