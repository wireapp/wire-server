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
    logInvitationCode,
    logEmail,
    traverseConcurrentlySem,
    traverseConcurrentlyWithErrors,
    exceptTToMaybe,
    ensureLocal,
  )
where

import Brig.API.Types
import Brig.App
import Control.Monad.Catch (throwM)
import Control.Monad.Trans.Except
import Data.Bifunctor (first)
import Data.Id (UserId)
import Data.Text qualified as T
import Data.Text.Ascii (AsciiText (toText))
import Imports
import Polysemy (Member)
import System.Logger (Msg)
import System.Logger qualified as Log
import UnliftIO.Async (pooledMapConcurrentlyN)
import UnliftIO.Exception (throwIO, try)
import Util.Logging (sha256String)
import Wire.API.User
import Wire.UserSubsystem (UserSubsystem, getSelfProfile)

fetchUserIdentity :: (Member UserSubsystem r) => UserId -> AppT r (Maybe UserIdentity)
fetchUserIdentity uid = do
  luid <- qualifyLocal uid
  liftSem (getSelfProfile luid)
    >>= maybe
      (throwM $ UserProfileNotFound uid)
      (pure . userIdentity . selfUser)

logEmail :: Email -> (Msg -> Msg)
logEmail email =
  Log.field "email_sha256" (sha256String . T.pack . show $ email)

logInvitationCode :: InvitationCode -> (Msg -> Msg)
logInvitationCode code = Log.field "invitation_code" (toText $ fromInvitationCode code)

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

exceptTToMaybe :: (Monad m) => ExceptT e m () -> m (Maybe e)
exceptTToMaybe = (pure . either Just (const Nothing)) <=< runExceptT
