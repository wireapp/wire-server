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

module Web.Scim.Handler
  ( ScimHandler,
    throwScim,
    fromScimHandler,
  )
where

import Control.Monad ((<=<))
import Control.Monad.Except
import Web.Scim.Schema.Error

-- | Handler type for SCIM. All errors will be thrown via 'ExceptT'.
type ScimHandler m = ExceptT ScimError m

-- | Throw a 'ScimError'.
throwScim :: (Monad m) => ScimError -> ScimHandler m a
throwScim = throwError

-- | A natural transformation for Servant handlers. To use it, you need to
-- provide a way to throw errors in the underlying @m@ monad.
--
-- We can't use something like 'MonadError' to throw errors in @m@ because
-- 'MonadError' allows only one type of errors per monad and @m@ might have
-- one already.
--
-- You can either do something custom for 'ScimError', or use
-- 'scimToServantErr'.
fromScimHandler ::
  (Monad m) =>
  (forall a. ScimError -> m a) ->
  (forall a. ScimHandler m a -> m a)
fromScimHandler fromError = either fromError pure <=< runExceptT
