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

module Brig.API.Util
  ( fetchUserIdentity,
    lookupProfilesMaybeFilterSameTeamOnly,
    logInvitationCode,
    validateHandle,
    logEmail,
    traverseConcurrentlyAppT,
    traverseConcurrentlySem,
    traverseConcurrentlyWithErrors,
    traverseConcurrentlyWithErrorsSem,
    traverseConcurrentlyWithErrorsAppT,
    exceptTToMaybe,
    ensureLocal,
    tryInsertVerificationCode,
  )
where

import Brig.API.Error
import Brig.API.Handler
import Brig.API.Types
import Brig.App
import Brig.Code qualified as Code
import Brig.Data.User qualified as Data
import Brig.Options (set2FACodeGenerationDelaySecs)
import Control.Lens (view)
import Control.Monad.Catch (throwM)
import Control.Monad.Trans.Except
import Data.Bifunctor
import Data.Handle (Handle, parseHandle)
import Data.Id
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Ascii (AsciiText (toText))
import Imports
import Polysemy
import Polysemy.Error qualified as E
import System.Logger (Msg)
import System.Logger qualified as Log
import UnliftIO.Async
import UnliftIO.Exception (throwIO, try)
import Util.Logging (sha256String)
import Wire.API.Error
import Wire.API.Error.Brig
import Wire.API.User
import Wire.Sem.Concurrency qualified as C
import Wire.UserSubsystem

lookupProfilesMaybeFilterSameTeamOnly :: UserId -> [UserProfile] -> (Handler r) [UserProfile]
lookupProfilesMaybeFilterSameTeamOnly self us = do
  selfTeam <- lift $ wrapClient $ Data.lookupUserTeam self
  pure $ case selfTeam of
    Just team -> filter (\x -> profileTeam x == Just team) us
    Nothing -> us

fetchUserIdentity :: (Member UserSubsystem r) => UserId -> AppT r (Maybe UserIdentity)
fetchUserIdentity uid = do
  luid <- qualifyLocal uid
  liftSem (getSelfProfile luid)
    >>= maybe
      (throwM $ UserProfileNotFound uid)
      (pure . userIdentity . selfUser)

validateHandle :: Text -> (Handler r) Handle
validateHandle = maybe (throwStd (errorToWai @'InvalidHandle)) pure . parseHandle

logEmail :: Email -> (Msg -> Msg)
logEmail email =
  Log.field "email_sha256" (sha256String . T.pack . show $ email)

logInvitationCode :: InvitationCode -> (Msg -> Msg)
logInvitationCode code = Log.field "invitation_code" (toText $ fromInvitationCode code)

-- | Traverse concurrently and collect errors.
traverseConcurrentlyAppT ::
  (Traversable t, Member (C.Concurrency 'C.Unsafe) r) =>
  (a -> ExceptT e (AppT r) b) ->
  t a ->
  AppT r [Either (a, e) b]
traverseConcurrentlyAppT f t = do
  env <- temporaryGetEnv
  AppT $
    lift $
      C.unsafePooledMapConcurrentlyN
        8
        (\a -> first (a,) <$> lowerAppT env (runExceptT $ f a))
        t

-- | Traverse concurrently and fail on first error.
traverseConcurrentlyWithErrors ::
  (Traversable t, Exception e, MonadUnliftIO m) =>
  (a -> ExceptT e m b) ->
  t a ->
  ExceptT e m (t b)
traverseConcurrentlyWithErrors f =
  ExceptT
    . try
    . ( traverse (either throwIO pure)
          <=< pooledMapConcurrentlyN 8 (runExceptT . f)
      )

traverseConcurrentlySem ::
  (Traversable t, MonadUnliftIO m) =>
  (a -> ExceptT e m b) ->
  t a ->
  m (t (Either (a, e) b))
traverseConcurrentlySem f =
  pooledMapConcurrentlyN 8 $ \a -> first (a,) <$> runExceptT (f a)

-- | Traverse concurrently and fail on first error.
traverseConcurrentlyWithErrorsSem ::
  forall t e a r b.
  (Traversable t, Member (C.Concurrency 'C.Unsafe) r) =>
  (a -> ExceptT e (Sem r) b) ->
  t a ->
  ExceptT e (Sem r) [b]
traverseConcurrentlyWithErrorsSem f =
  ExceptT
    . E.runError
    . ( traverse (either E.throw pure)
          <=< C.unsafePooledMapConcurrentlyN 8 (raise . runExceptT . f)
      )

traverseConcurrentlyWithErrorsAppT ::
  forall t e a r b.
  (Traversable t, Member (C.Concurrency 'C.Unsafe) r) =>
  (a -> ExceptT e (AppT r) b) ->
  t a ->
  ExceptT e (AppT r) [b]
traverseConcurrentlyWithErrorsAppT f t = do
  env <- lift temporaryGetEnv
  ExceptT $
    AppT $
      lift $
        runExceptT $
          traverseConcurrentlyWithErrorsSem
            (mapExceptT (lowerAppT env) . f)
            t

exceptTToMaybe :: (Monad m) => ExceptT e m () -> m (Maybe e)
exceptTToMaybe = (pure . either Just (const Nothing)) <=< runExceptT

tryInsertVerificationCode :: Code.Code -> (RetryAfter -> e) -> ExceptT e (AppT r) ()
tryInsertVerificationCode code e = do
  ttl <- set2FACodeGenerationDelaySecs <$> view settings
  mRetryAfter <- wrapClientE $ Code.insert code ttl
  mapM_ (throwE . e) mRetryAfter
