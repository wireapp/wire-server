{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

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

module Web.Scim.Handler
  ( ScimHandler,
    throwScim,
    runScimHandler,
    relaxErrors,
  )
where

import Control.Monad.Except
import Data.Functor ((<&>))
import Servant (Elem)
import Servant.API (Contains, HasStatus, IsMember, Union, inject, relaxUnion)
import Servant.Server (respond)

newtype ScimHandler es m a = ScimHandler {unScimHandler :: ExceptT (Union es) m a}
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadError (Union es))

relaxErrors :: (Contains es es', Monad m) => ScimHandler es m a -> ScimHandler es' m a
relaxErrors handler =
  ScimHandler . ExceptT $
    (runExceptT . unScimHandler $ handler) <&> either (Left . relaxUnion) Right

-- N.B. we tried writing an MonadError like this, but ghc complained about
-- "functional dependency"
throwInject ::
  forall (e :: *) (es :: [*]) m a.
  UElem e es =>
  e ->
  ScimHandler es m a
throwInject = throwError . inject

-- instance MonadError e m => MonadError e (ScimHandler xs m) where
--   throwError = lift . throwError
--   catchError (ScimHandler act) h =
--     ScimHandler $
--       ExceptT $
--         runExceptT act `catchError` (runExceptT . unScimHandler . h)

-- | This combinator runs 'UVerbT'. It applies 'respond' internally, so the handler
-- may use the usual 'return'.
runScimHandler :: (Monad m, HasStatus x, IsMember x xs) => ScimHandler xs m x -> m (Union xs)
runScimHandler (ScimHandler act) = either id id <$> runExceptT (act >>= respond)

-- | Short-circuit 'UVerbT' computation returning one of the response types.
throwScim :: (Monad m, HasStatus x, IsMember x xs) => x -> ScimHandler xs m a
throwScim = ScimHandler . ExceptT . fmap Left . respond
