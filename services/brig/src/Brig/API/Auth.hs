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

module Brig.API.Auth where

import Brig.API.Error (authMissingCookie, throwStd)
import Brig.API.Handler
import qualified Brig.ZAuth as ZAuth
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.ZAuth.Token as ZAuth
import Imports
import Wire.API.Routes.Public.Brig (SomeUserToken)
import Wire.API.Routes.Public.Brig hiding (SomeUserToken)

access :: NonEmpty SomeUserToken -> Maybe SomeAccessToken -> Handler r Text
access ut mat = do
  tokens <- partitionTokens ut
  case (tokens, mat) of
    (Left userTokens, Just (AccessToken mat)) ->
      error "TODO"

  -- traceM $ "user tokens: " <> show ut
  -- traceM $ "access token: " <> show at
  pure "OK"
  where
    renewAccess uts mat = error "TODO"

partitionTokens ::
  NonEmpty SomeUserToken ->
  Handler
    r
    ( Either
        (NonEmpty (ZAuth.Token ZAuth.User))
        (NonEmpty (ZAuth.Token ZAuth.LegalHoldUser))
    )
partitionTokens tokens =
  case partitionEithers (map toEither (NE.toList tokens)) of
    (at : ats, []) -> pure (Left (at :| ats))
    ([], lt : lts) -> pure (Right (lt :| lts))
    ([], []) -> throwStd authMissingCookie -- impossible
    (_ats, _rts) -> throwStd authMissingCookie
  where
    toEither :: SomeUserToken -> Either (ZAuth.Token ZAuth.User) (ZAuth.Token ZAuth.LegalHoldUser)
    toEither = error "TODO"

-- renew = \case
--   Nothing ->
--     const $ throwStd authMissingCookie
--   (Just (Left userTokens)) ->
--     -- normal UserToken, so we want a normal AccessToken
--     fmap Left . wrapHttpClientE . renewAccess userTokens <=< matchingOrNone leftToMaybe
--   (Just (Right legalholdUserTokens)) ->
--     -- LegalholdUserToken, so we want a LegalholdAccessToken
--     fmap Right . wrapHttpClientE . renewAccess legalholdUserTokens <=< matchingOrNone rightToMaybe
--   where
--     renewAccess uts mat =
--       Auth.renewAccess uts mat !>> zauthError
--     matchingOrNone :: (a -> Maybe b) -> Maybe a -> (Handler r) (Maybe b)
--     matchingOrNone matching = traverse $ \accessToken ->
--       case matching accessToken of
--         Just m -> pure m
--         Nothing -> throwStd authTokenMismatch
