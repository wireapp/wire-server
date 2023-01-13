{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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

module Spar.Sem.SAML2.Library (saml2ToSaml2WebSso) where

import qualified Control.Monad.Catch as Catch
import Control.Monad.Except
import Data.Id (TeamId)
import Data.String.Conversions (cs)
import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Final
import Polysemy.Input
import Polysemy.Internal.Tactics
import SAML2.WebSSO hiding (Error)
import qualified SAML2.WebSSO as SAML hiding (Error)
import Spar.Error (SparCustomError (..), SparError)
import Spar.Options
import Spar.Sem.AReqIDStore (AReqIDStore)
import qualified Spar.Sem.AReqIDStore as AReqIDStore
import Spar.Sem.AssIDStore (AssIDStore)
import qualified Spar.Sem.AssIDStore as AssIDStore
import Spar.Sem.IdPConfigStore (IdPConfigStore)
import qualified Spar.Sem.IdPConfigStore as IdPConfigStore
import Spar.Sem.SAML2
import Wire.API.User.IdentityProvider (WireIdP)
import Wire.Sem.Logger (Logger)
import qualified Wire.Sem.Logger as Logger

wrapMonadClientSPImpl :: Members '[Error SparError, Final IO] r => Sem r a -> SPImpl r a
wrapMonadClientSPImpl action =
  SPImpl action
    `Catch.catch` (SPImpl . throw . SAML.CustomError . SparCassandraError . cs . show @SomeException)

instance Member (Final IO) r => Catch.MonadThrow (SPImpl r) where
  throwM = SPImpl . embedFinal . Catch.throwM @IO

instance Member (Final IO) r => Catch.MonadCatch (SPImpl r) where
  catch (SPImpl m) handler = SPImpl $
    withStrategicToFinal @IO $ do
      m' <- runS m
      st <- getInitialStateS
      handler' <- bindS $ unSPImpl . handler
      pure $ m' `Catch.catch` \e -> handler' $ e <$ st

newtype SPImpl r a = SPImpl {unSPImpl :: Sem r a}
  deriving (Functor, Applicative, Monad)

instance Member (Input Opts) r => HasConfig (SPImpl r) where
  getConfig = SPImpl $ inputs saml

instance Members '[Input Opts, Logger String] r => HasLogger (SPImpl r) where
  logger lvl = SPImpl . Logger.log (Logger.samlFromLevel lvl)

instance Member (Embed IO) r => MonadIO (SPImpl r) where
  liftIO = SPImpl . embed @IO

instance Member (Embed IO) r => HasCreateUUID (SPImpl r)

instance Member (Embed IO) r => HasNow (SPImpl r)

instance Members '[Error SparError, Final IO, AReqIDStore] r => SPStoreID AuthnRequest (SPImpl r) where
  storeID = (wrapMonadClientSPImpl .) . AReqIDStore.store
  unStoreID = wrapMonadClientSPImpl . AReqIDStore.unStore
  isAliveID = wrapMonadClientSPImpl . AReqIDStore.isAlive

instance Members '[Error SparError, Final IO, AssIDStore] r => SPStoreID Assertion (SPImpl r) where
  storeID = (wrapMonadClientSPImpl .) . AssIDStore.store
  unStoreID = wrapMonadClientSPImpl . AssIDStore.unStore
  isAliveID = wrapMonadClientSPImpl . AssIDStore.isAlive

instance Members '[Error SparError, IdPConfigStore, Final IO] r => SPStoreIdP SparError (SPImpl r) where
  type IdPConfigExtra (SPImpl r) = WireIdP
  type IdPConfigSPId (SPImpl r) = TeamId

  storeIdPConfig = SPImpl . IdPConfigStore.insertConfig
  getIdPConfig = SPImpl . IdPConfigStore.getConfig
  getIdPConfigByIssuerOptionalSPId issuer mbteam = SPImpl $ case mbteam of
    Nothing -> IdPConfigStore.getIdPByIssuerV1 issuer
    Just team -> IdPConfigStore.getIdPByIssuerV2 issuer team

instance Member (Error SparError) r => MonadError SparError (SPImpl r) where
  throwError = SPImpl . throw
  catchError m handler = SPImpl $ catch (unSPImpl m) $ unSPImpl . handler

-- | To learn more about polysemy tactics, read this:
--   * https://reasonablypolymorphic.com/blog/freer-higher-order-effects/
--   * https://reasonablypolymorphic.com/blog/tactics/
saml2ToSaml2WebSso ::
  forall r a.
  Members
    '[ AReqIDStore,
       AssIDStore,
       Error SparError,
       IdPConfigStore,
       Input Opts,
       Logger String,
       Embed IO,
       Final IO
     ]
    r =>
  Sem (SAML2 ': r) a ->
  Sem r a
saml2ToSaml2WebSso =
  interpretH $ \case
    AuthReq n ma i -> do
      get_a <- runT ma
      ins <- getInspectorT
      x <- raise $ unSPImpl $ SAML.authreq @_ @SparError n (inspectOrBomb ins get_a) i
      s <- getInitialStateT
      pure $ x <$ s
    AuthResp mitlt ma mb mc ab -> do
      get_a <- runT ma
      get_b <- runT mb
      get_c <- bindT $ \(a, (b, c)) -> mc a b c
      ins <- getInspectorT
      s <- getInitialStateT
      x <- raise $ unSPImpl $ SAML.authresp mitlt (inspectOrBomb ins get_a) (inspectOrBomb ins get_b) (\x y z -> inspectOrBomb ins $ get_c $ (x, (y, z)) <$ s) ab
      pure $ x <$ s
    Meta t ma mb -> do
      get_a <- runT ma
      get_b <- runT mb
      ins <- getInspectorT
      x <- raise $ unSPImpl $ SAML.meta t (inspectOrBomb ins get_a) (inspectOrBomb ins get_b)
      s <- getInitialStateT
      pure $ x <$ s
    ToggleCookie sbs mp -> do
      liftT $ unSPImpl $ SAML.toggleCookie sbs mp

inspectOrBomb ::
  Members
    '[ AReqIDStore,
       AssIDStore,
       Error SparError,
       IdPConfigStore,
       Logger String,
       Input Opts,
       Embed IO,
       Final IO
     ]
    r =>
  Inspector f ->
  Sem (SAML2 : r) (f b) ->
  SPImpl r b
inspectOrBomb ins get_a = do
  fa <- SPImpl $ saml2ToSaml2WebSso get_a
  maybe
    (SPImpl . throw @SparError $ SAML.CustomError $ SparInternalError "saml2ToSaml2WebSso called with an uninspectable weaving functor")
    pure
    $ inspect ins fa
