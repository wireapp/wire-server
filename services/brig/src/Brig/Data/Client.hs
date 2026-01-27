{-# LANGUAGE LambdaCase #-}

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

module Brig.Data.Client
  ( -- * Clients
    ClientDataError (..),
    AuthError (..),
    ReAuthError (..),
    ReAuthPolicy,
    reAuthForNewClients,
    addClientWithReAuthPolicy,
    addClient,
  )
where

import Brig.App
import Control.Error
import Data.Id
import Data.Json.Util (toUTCTimeMillis)
import Data.Qualified
import Imports
import Polysemy (Member)
import Wire.API.User.Client hiding (UpdateClient (..))
import Wire.AuthenticationSubsystem (AuthenticationSubsystem)
import Wire.AuthenticationSubsystem qualified as Authentication
import Wire.AuthenticationSubsystem.Error
import Wire.ClientStore (ClientStore, DuplicateMLSPublicKey (..))
import Wire.ClientStore qualified as ClientStore

data ClientDataError
  = TooManyClients
  | ClientReAuthError !ReAuthError
  | ClientMissingAuth
  | MalformedPrekeys
  | MLSPublicKeyDuplicate
  | MLSNotEnabled
  | KeyPackageDecodingError
  | InvalidKeyPackageRef

-- | Re-authentication policy.
--
-- For a potential new client, a policy is a function that takes as arguments
-- the number of existing clients of the same type, and whether the client
-- already exists, and returns whether the user should be forced to
-- re-authenticate.
type ReAuthPolicy = Int -> Bool -> Bool

-- | Default re-authentication policy.
--
-- Re-authenticate if there is at least one other client.
reAuthForNewClients :: ReAuthPolicy
reAuthForNewClients count upsert = count > 0 && not upsert

addClient ::
  (Member AuthenticationSubsystem r, Member ClientStore r) =>
  Local UserId ->
  ClientId ->
  NewClient ->
  Int ->
  Maybe ClientCapabilityList ->
  ExceptT ClientDataError (AppT r) (Client, [Client], Word)
addClient = addClientWithReAuthPolicy reAuthForNewClients

addClientWithReAuthPolicy ::
  forall r.
  ( MonadReader Brig.App.Env (AppT r),
    Member AuthenticationSubsystem r,
    Member ClientStore r
  ) =>
  ReAuthPolicy ->
  Local UserId ->
  ClientId ->
  NewClient ->
  Int ->
  Maybe ClientCapabilityList ->
  ExceptT ClientDataError (AppT r) (Client, [Client], Word)
addClientWithReAuthPolicy reAuthPolicy u newId c maxPermClients caps = do
  clients <- lift . liftSem $ ClientStore.lookupClients (tUnqualified u)
  let typed = filter ((== newClientType c) . clientType) clients
  let count = length typed
  let upsert = any exists typed
  when (reAuthPolicy count upsert) do
    (lift . liftSem $ Authentication.reauthenticateEither (tUnqualified u) (newClientPassword c))
      >>= either (throwE . ClientReAuthError) pure
  let capacity = fmap (+ (-count)) limit
  unless (maybe True (> 0) capacity || upsert) $
    throwE TooManyClients
  new <- insert (tUnqualified u)
  let !total = fromIntegral (length clients + if upsert then 0 else 1)
  let old = maybe (filter (not . exists) typed) (const []) limit
  pure (new, old, total)
  where
    limit :: Maybe Int
    limit = case newClientType c of
      PermanentClientType -> Just maxPermClients
      TemporaryClientType -> Nothing
      LegalHoldClientType -> Nothing

    exists :: Client -> Bool
    exists = (==) newId . (.clientId)

    insert :: UserId -> ExceptT ClientDataError (AppT r) Client
    insert uid = do
      -- Is it possible to do this somewhere else? Otherwise we could use `MonadClient` instead
      now <- toUTCTimeMillis <$> (liftIO =<< asks (.currentTime))
      mErr <- lift . liftSem $ ClientStore.upsert uid newId now (c {newClientCapabilities = caps})
      case mErr of
        Just DuplicateMLSPublicKey -> throwE MLSPublicKeyDuplicate
        Nothing ->
          pure $!
            Client
              { clientId = newId,
                clientType = newClientType c,
                clientTime = now,
                clientClass = newClientClass c,
                clientLabel = newClientLabel c,
                clientCookie = newClientCookie c,
                clientModel = newClientModel c,
                clientCapabilities = fromMaybe mempty caps,
                clientMLSPublicKeys = mempty,
                clientLastActive = Nothing
              }
