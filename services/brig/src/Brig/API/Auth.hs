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

import Brig.API.Error (authTokenMismatch, internalServerError, throwStd, zauthError)
import Brig.API.Handler
import Brig.App (wrapHttpClientE)
import qualified Brig.User.Auth as Auth
import Brig.ZAuth (UserTokenLike (mkSomeAccess))
import qualified Brig.ZAuth as ZAuth
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.List1 (List1 (..))
import qualified Data.ZAuth.Token as ZAuth
import Imports
import Network.Wai.Utilities ((!>>))
import Wire.API.User.Auth

access :: forall r. NonEmpty SomeUserToken -> Maybe SomeAccessToken -> Handler r SomeAccess
access ut mat = do
  partitionTokens ut mat >>= either (uncurry renew) (uncurry renew)
  where
    renew t mt = mkSomeAccess <$> wrapHttpClientE (Auth.renewAccess (List1 t) mt) !>> zauthError

partitionTokens ::
  NonEmpty SomeUserToken ->
  Maybe SomeAccessToken ->
  Handler
    r
    ( Either
        (NonEmpty (ZAuth.Token ZAuth.User), Maybe (ZAuth.Token ZAuth.Access))
        (NonEmpty (ZAuth.Token ZAuth.LegalHoldUser), Maybe (ZAuth.Token ZAuth.LegalHoldAccess))
    )
partitionTokens tokens mat =
  case (partitionEithers (map toEither (NE.toList tokens)), mat) of
    -- only PlainUserToken
    ((at : ats, []), Nothing) -> pure (Left (at :| ats, Nothing))
    ((at : ats, []), Just (PlainAccessToken a)) -> pure (Left (at :| ats, Just a))
    ((_t : _ts, []), Just (LHAccessToken _)) -> throwStd authTokenMismatch
    -- only LHUserToken tokens
    (([], lt : lts), Nothing) -> pure (Right (lt :| lts, Nothing))
    (([], _t : _ts), Just (PlainAccessToken _)) -> throwStd authTokenMismatch
    (([], lt : lts), Just (LHAccessToken l)) -> pure (Right (lt :| lts, Just l))
    -- impossible
    (([], []), _) -> throwStd internalServerError
    -- mixed PlainUserToken and LHUserToken
    ((_ats, _lts), _) -> throwStd authTokenMismatch
  where
    toEither :: SomeUserToken -> Either (ZAuth.Token ZAuth.User) (ZAuth.Token ZAuth.LegalHoldUser)
    toEither (PlainUserToken ut) = Left ut
    toEither (LHUserToken lt) = Right lt
