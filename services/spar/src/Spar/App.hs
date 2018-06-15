{-# LANGUAGE FlexibleInstances          #-}
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
import SAML2.WebSSO hiding (UserId(..))
import Servant
import Spar.Options as Options

import qualified Cassandra as Cas
import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.UUID.V4 as UUID
import qualified SAML2.WebSSO as SAML
import qualified Spar.Data as Data
import qualified Spar.Intra.Brig as Brig
import qualified System.Logger as Log


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
  logger lv mg = asks sparCtxLogger >>= \lg -> Spar $ Log.log lg (toLevel lv) mg'
    where
      mg' = Log.msg $ flatten <$> mg
      flatten '\n' = ' '
      flatten '\r' = ' '
      flatten '\t' = ' '
      flatten c    = c

toLevel :: SAML.LogLevel -> Log.Level
toLevel = \case
  SILENT   -> Log.Fatal
  CRITICAL -> Log.Fatal
  ERROR    -> Log.Error
  WARN     -> Log.Warn
  INFO     -> Log.Info
  DEBUG    -> Log.Debug

fromLevel :: Log.Level -> SAML.LogLevel
fromLevel = \case
  Log.Fatal -> CRITICAL
  Log.Error -> ERROR
  Log.Warn  -> WARN
  Log.Info  -> INFO
  Log.Debug -> DEBUG
  Log.Trace -> DEBUG

instance SPStore Spar where
  storeRequest i r      = wrapMonadClient' $ \env -> Data.storeRequest env i r
  checkAgainstRequest r = wrapMonadClient' $ \env -> Data.checkAgainstRequest env r
  storeAssertion i r    = wrapMonadClient' $ \env -> Data.storeAssertion env i r

-- | Call a cassandra command in the 'Spar' monad.  Catch all exceptions and re-throw them as 500 in
-- Handler.
wrapMonadClient' :: (Data.Env -> Cas.Client a) -> Spar a
wrapMonadClient' action = do
  denv <- Data.Env <$> (fromTime <$> getNow) <*> pure (8 * 60 * 60)  -- TODO: make this yaml-configurable.
  Spar $ do
    ctx <- asks sparCtxCas
    runClient ctx (action denv) `Catch.catch`
      \e@(SomeException _) -> throwError err500 { errBody = LBS.pack $ show e }

wrapMonadClient :: Cas.Client a -> Spar a
wrapMonadClient = wrapMonadClient' . const

insertUser :: SAML.UserId -> UserId -> Spar ()
insertUser suid buid = wrapMonadClient $ Data.insertUser suid buid

-- | Look up user locally, then in brig, then return the 'UserId'.  If either lookup fails, return
-- 'Nothing'.  See also: 'Spar.App.createUser'.
--
-- ASSUMPTIONS: User creation on brig/galley is idempotent.  Any incomplete creation (because of
-- brig or galley crashing) will cause the lookup here to yield invalid user.
getUser :: SAML.UserId -> Spar (Maybe UserId)
getUser suid = do
  mbuid <- wrapMonadClient $ Data.getUser suid
  case mbuid of
    Nothing -> pure Nothing
    Just buid -> Brig.confirmUserId buid

-- | Create a fresh 'Data.Id.UserId', store it on C* locally together with 'SAML.UserId', then
-- create user on brig with that 'UserId'.  See also: 'Spar.App.getUser'.
--
-- The manual for the team admin should say this: when deleting a user, delete it on the IdP first,
-- then delete it on the team admin page in wire.  If a user is deleted in wire but not in the IdP,
-- it will be recreated on the next successful login attempt.
--
-- When an sso login succeeds for a user that is marked as deleted in brig, it is recreated by spar.
-- This is necessary because brig does not talk to spar when deleting users, and we may have
-- 'UserId' records on spar that are deleted on brig.  Without this lenient behavior, there would be
-- no way for admins to reuse a 'SAML.UserId' if it has ever been associated with a deleted user in
-- the past.
--
-- FUTUREWORK: once we support <https://github.com/wireapp/hscim scim>, brig will refuse to delete
-- users that have an sso id, unless the request comes from spar.  then we can make users
-- undeletable in the team admin page, and ask admins to go talk to their IdP system.
createUser :: SAML.UserId -> Spar UserId
createUser suid = do
  buid <- Id <$> liftIO UUID.nextRandom
  insertUser suid buid
  buid' <- Brig.createUser suid buid
  assert (buid == buid') $ pure buid

forwardBrigLogin :: UserId -> Spar SAML.Void
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
