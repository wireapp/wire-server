{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

module Spar.Sem.Utils
  ( viaRunHttp,
    RunHttpEnv (..),
    interpretClientToIO,
    ttlErrorToSparError,
    idpDbErrorToSparError,
  )
where

import Bilge
import Cassandra as Cas
import qualified Control.Monad.Catch as Catch
import Control.Monad.Except (ExceptT (..), MonadError, runExceptT)
import qualified Data.Text.Lazy as LText
import Imports hiding (log)
import Polysemy
import Polysemy.Error
import Polysemy.Final
import qualified SAML2.WebSSO as SAML
import Spar.Error
import Spar.Intra.Brig (MonadSparToBrig (..))
import Spar.Intra.Galley (MonadSparToGalley)
import qualified Spar.Intra.Galley as Intra
import qualified System.Logger as TinyLog
import qualified System.Logger.Class as TinyLog
import Wire.API.User.Saml
import Wire.Sem.Logger (Logger)
import qualified Wire.Sem.Logger as Logger

-- | Run an embedded Cassandra 'Client'  in @Final IO@.
interpretClientToIO ::
  ( Member (Error SparError) r,
    Member (Final IO) r
  ) =>
  ClientState ->
  Sem (Embed Client ': r) a ->
  Sem r a
interpretClientToIO ctx = interpret $ \case
  Embed action -> withStrategicToFinal @IO $ do
    action' <- liftS $ runClient ctx action
    st <- getInitialStateS
    handler' <-
      bindS
        $ throw @SparError
        . SAML.CustomError
        . SparCassandraError
        . LText.pack
        . show @SomeException
    pure $ action' `Catch.catch` \e -> handler' $ e <$ st

ttlErrorToSparError :: (Member (Error SparError) r) => Sem (Error TTLError ': r) a -> Sem r a
ttlErrorToSparError = mapError (SAML.CustomError . SparCassandraTTLError)

idpDbErrorToSparError :: (Member (Error SparError) r) => Sem (Error IdpDbError ': r) a -> Sem r a
idpDbErrorToSparError = mapError (SAML.CustomError . IdpDbError)

data RunHttpEnv r = RunHttpEnv
  { rheManager :: Bilge.Manager,
    rheRequest :: Bilge.Request
  }

newtype RunHttp r a = RunHttp
  { unRunHttp :: ReaderT (RunHttpEnv r) (ExceptT SparError (HttpT (Sem r))) a
  }
  deriving newtype (Functor, Applicative, Monad, MonadError SparError, MonadReader (RunHttpEnv r))

instance (Member (Embed IO) r) => MonadIO (RunHttp r) where
  liftIO = semToRunHttp . embed

instance (Member (Embed IO) r) => MonadHttp (RunHttp r) where
  handleRequestWithCont r fribia =
    RunHttp
      $ lift
      $ lift
      $ handleRequestWithCont r fribia

semToRunHttp :: Sem r a -> RunHttp r a
semToRunHttp = RunHttp . lift . lift . lift

viaRunHttp ::
  (Member (Error SparError) r) =>
  RunHttpEnv r ->
  RunHttp r a ->
  Sem r a
viaRunHttp env m = do
  ma <- runHttpT (rheManager env) $ runExceptT $ flip runReaderT env $ unRunHttp m
  case ma of
    Left err -> throw err
    Right a -> pure a

instance (Member (Logger (TinyLog.Msg -> TinyLog.Msg)) r) => TinyLog.MonadLogger (RunHttp r) where
  log lvl msg = semToRunHttp $ Logger.log (Logger.fromLevel lvl) msg

instance
  ( Member (Logger (TinyLog.Msg -> TinyLog.Msg)) r,
    Member (Embed IO) r
  ) =>
  MonadSparToGalley (RunHttp r)
  where
  call modreq = do
    req <- asks rheRequest
    httpLbs req modreq

instance
  ( Member (Logger (TinyLog.Msg -> TinyLog.Msg)) r,
    Member (Embed IO) r
  ) =>
  MonadSparToBrig (RunHttp r)
  where
  call modreq = do
    req <- asks rheRequest
    httpLbs req modreq
