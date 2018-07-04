{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Spar.App where

import Bilge
import Cassandra
import Control.Exception (SomeException(SomeException), assert)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Id
import Lens.Micro
import SAML2.WebSSO hiding (UserRef(..))
import Servant
import Spar.Options as Options
import Web.Cookie (SetCookie)

import qualified Cassandra as Cas
import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.UUID.V4 as UUID
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import qualified Spar.Intra.Brig as Brig
import qualified System.Logger as Log
import qualified URI.ByteString as URI


newtype Spar a = Spar { fromSpar :: ReaderT Env Handler a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError ServantErr)

data Env = Env
  { sparCtxOpts         :: Opts
  , sparCtxLogger       :: Log.Logger
  , sparCtxCas          :: Cas.ClientState
  , sparCtxHttpManager  :: Bilge.Manager
  , sparCtxHttpBrig     :: Bilge.Request
  }

instance HasConfig Spar where
  type ConfigExtra Spar = TeamId
  getConfig = asks (saml . sparCtxOpts)

instance SP Spar where
  -- FUTUREWORK: optionally use 'field' to index user or idp ids for easier logfile processing.
  -- (FUTUREWORK: we could also call 'show' on the message instead of manually removing newlines,
  -- then we could cut&paste it into ghci and make everything visible again.)
  logger lv mg = asks sparCtxLogger >>= \lg -> Spar $ Log.log lg (toLevel lv) mg'
    where
      mg' = Log.msg $ flatten <$> mg
      flatten '\n' = ' '
      flatten '\r' = ' '
      flatten '\t' = ' '
      flatten c    = c

toLevel :: SAML.Level -> Log.Level
toLevel = \case
  SAML.Fatal -> Log.Fatal
  SAML.Error -> Log.Error
  SAML.Warn  -> Log.Warn
  SAML.Info  -> Log.Info
  SAML.Debug -> Log.Debug
  SAML.Trace -> Log.Trace

fromLevel :: Log.Level -> SAML.Level
fromLevel = \case
  Log.Fatal -> SAML.Fatal
  Log.Error -> SAML.Error
  Log.Warn  -> SAML.Warn
  Log.Info  -> SAML.Info
  Log.Debug -> SAML.Debug
  Log.Trace -> SAML.Trace

instance SPStore Spar where
  storeRequest i r      = wrapMonadClientWithEnv $ Data.storeRequest i r
  checkAgainstRequest r = wrapMonadClientWithEnv $ Data.checkAgainstRequest r
  storeAssertion i r    = wrapMonadClientWithEnv $ Data.storeAssertion i r

instance SPStoreIdP Spar where
  storeIdPConfig :: IdPConfig TeamId -> Spar ()
  storeIdPConfig idp = wrapMonadClient $ Data.storeIdPConfig idp

  getIdPConfig :: IdPId -> Spar (IdPConfig TeamId)
  getIdPConfig = (>>= maybe (throwError err404) pure) . wrapMonadClient . Data.getIdPConfig

  getIdPConfigByIssuer :: Issuer -> Spar (IdPConfig TeamId)
  getIdPConfigByIssuer = (>>= maybe (throwError err404) pure) . wrapMonadClient . Data.getIdPConfigByIssuer

-- | 'wrapMonadClient' with an 'Env' in a 'ReaderT'.
wrapMonadClientWithEnv :: ReaderT Data.Env Cas.Client a -> Spar a
wrapMonadClientWithEnv action = do
  denv <- Data.mkEnv <$> (sparCtxOpts <$> ask) <*> (fromTime <$> getNow)
  wrapMonadClient (action `runReaderT` denv)

-- | Call a cassandra command in the 'Spar' monad.  Catch all exceptions and re-throw them as 500 in
-- Handler.
wrapMonadClient :: Cas.Client a -> Spar a
wrapMonadClient action = do
  Spar $ do
    ctx <- asks sparCtxCas
    runClient ctx action `Catch.catch`
      \e@(SomeException _) -> throwError err500 { errBody = LBS.pack $ show e }

insertUser :: SAML.UserRef -> UserId -> Spar ()
insertUser suid buid = wrapMonadClient $ Data.insertUser suid buid

-- | Look up user locally, then in brig, then return the 'UserId'.  If either lookup fails, return
-- 'Nothing'.  See also: 'Spar.App.createUser'.
--
-- ASSUMPTIONS: User creation on brig/galley is idempotent.  Any incomplete creation (because of
-- brig or galley crashing) will cause the lookup here to yield invalid user.
getUser :: SAML.UserRef -> Spar (Maybe UserId)
getUser suid = do
  mbuid <- wrapMonadClient $ Data.getUser suid
  case mbuid of
    Nothing -> pure Nothing
    Just buid -> Brig.confirmUserId buid

-- | Create a fresh 'Data.Id.UserId', store it on C* locally together with 'SAML.UserRef', then
-- create user on brig with that 'UserId'.  See also: 'Spar.App.getUser'.
--
-- The manual for the team admin should say this: when deleting a user, delete it on the IdP first,
-- then delete it on the team admin page in wire.  If a user is deleted in wire but not in the IdP,
-- it will be recreated on the next successful login attempt.
--
-- When an sso login succeeds for a user that is marked as deleted in brig, it is recreated by spar.
-- This is necessary because brig does not talk to spar when deleting users, and we may have
-- 'UserId' records on spar that are deleted on brig.  Without this lenient behavior, there would be
-- no way for admins to reuse a 'SAML.UserRef' if it has ever been associated with a deleted user in
-- the past.
--
-- FUTUREWORK: once we support <https://github.com/wireapp/hscim scim>, brig will refuse to delete
-- users that have an sso id, unless the request comes from spar.  then we can make users
-- undeletable in the team admin page, and ask admins to go talk to their IdP system.
createUser :: SAML.UserRef -> Spar UserId
createUser suid = do
  buid <- Id <$> liftIO UUID.nextRandom
  teamid <- (^. idpExtraInfo) <$> getIdPConfigByIssuer (suid ^. uidTenant)
  insertUser suid buid
  buid' <- Brig.createUser suid buid teamid
  assert (buid == buid') $ pure buid

forwardBrigLogin :: UserId -> Spar (SetCookie, URI.URI)
forwardBrigLogin = Brig.forwardBrigLogin


instance SPHandler Spar where
  type NTCTX Spar = Env
  nt ctx (Spar action) = runReaderT action ctx

instance MonadHttp Spar where
  getManager = asks sparCtxHttpManager

instance Brig.MonadSparToBrig Spar where
  call modreq = do
    req <- asks sparCtxHttpBrig
    httpLbs req modreq
