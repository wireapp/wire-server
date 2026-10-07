-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

-- | Minimal 'Polysemy.Sem' runner embedding the 'Wire.WebPushStore' effect
-- stack inside gundeck's 'Gundeck.Gundeck' monad.
--
-- Gundeck is not (yet) a full Polysemy application: its monad is a plain
-- @'ReaderT' 'Gundeck.Env.Env' 'Cassandra.Client'@ (see
-- 'Gundeck.Monad.Gundeck'). Rather than rewrite it, we run only the thin effect
-- stack that 'Wire.WebPushStore.Postgres.interpretWebPushStoreToPostgres'
-- demands — exactly the three effects in 'Wire.Postgres.PGConstraints'
-- (@'Polysemy.Input.Input' 'Hasql.Pool.Pool'@, @'Polysemy.Embed.Embed' 'IO'@,
-- @'Polysemy.Error.Error' 'Hasql.Errors.UsageError'@) — plus the store effect
-- on top. This mirrors the "effect in lib, minimal Sem runner in gundeck".
--
-- Callers obtain the pool from 'Gundeck.Env.Env' and surface a 'Left'
-- 'UsageError' as an HTTP 500, e.g.
--
-- @
-- runWebPush pool (insertSubscription uid sub conn) >>= \case
--   Right () -> pure ()
--   Left err -> 'Control.Monad.throwM' ('Network.Wai.Utilities.Server.mkError' 'Network.HTTP.Types.status500' "web-push-store-error" (show err))
-- @
module Gundeck.Push.Web.Runner (runWebPush) where

import Hasql.Pool (Pool, UsageError)
import Imports
import Polysemy
import Polysemy.Error (Error, runError)
import Polysemy.Input (Input, runInputConst)
import Wire.WebPushStore (WebPushStore)
import Wire.WebPushStore.Postgres (interpretWebPushStoreToPostgres)

-- | Run a 'WebPushStore' effect program against the shared Hasql 'Pool',
-- returning the result or a Postgres 'UsageError'.
--
-- The effect order in the signature is significant: 'interpretWebPushStoreToPostgres'
-- requires 'Wire.Postgres.PGConstraints', i.e. @'Input' 'Pool'@, @'Embed' 'IO'@
-- and @'Error' 'UsageError'@ must all be /members below/ 'WebPushStore' in the
-- stack. The body applies the interpreters in composition order
-- (rightmost\/innermost first): the store interpreter consumes 'WebPushStore'
-- while the three constraint effects are still present; 'runInputConst' then
-- feeds the pool; 'runError' surfaces 'UsageError' as an 'Either'; 'runM'
-- embeds the residual 'Embed' 'IO' into 'IO'.
--
-- We use 'runM' rather than the @'runFinal' '.' 'embedToFinal'@ final-style
-- pipeline that galley uses: final-style requires @'Final' 'IO'@ to already
-- inhabit the effect row (so @'embedToFinal'@ has something to lower @'Embed'@
-- onto), which would leak a @'Final' 'IO'@ member into every caller's 'Sem'
-- type. 'runM' is the idiomatic choice for a thin embedded runner like this,
-- where the (minor) performance difference is irrelevant.
runWebPush ::
  (MonadIO m) =>
  Pool ->
  Sem '[WebPushStore, Input Pool, Error UsageError, Embed IO] a ->
  m (Either UsageError a)
runWebPush pool =
  liftIO
    . runM
    . runError @UsageError
    . runInputConst pool
    . interpretWebPushStoreToPostgres
