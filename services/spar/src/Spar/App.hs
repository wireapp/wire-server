{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Spar.App where

import Cassandra
import Control.Exception (SomeException(SomeException))
import Control.Lens hiding (Level)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Id
import Data.Int
import Data.List.NonEmpty as NE
import SAML2.WebSSO hiding (UserId(..))
import Servant
import Spar.Options as Options
import System.Logger
import Util.Options (casEndpoint, casKeyspace, epHost, epPort)

import qualified Cassandra as Cas
import qualified Cassandra.Schema as Cas
import qualified Cassandra.Settings as Cas
import qualified Control.Monad.Catch as Catch
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as ST
import qualified SAML2.WebSSO as SAML
import qualified Spar.Brig as Brig
import qualified Spar.Data as Data
import qualified System.Logger as Log


----------------------------------------------------------------------

class Monad m => MonadSpar m where
  -- locally read user record from Cassandra
  getUser :: SAML.UserId -> m (Maybe UserId)

  -- create user locally and on brig
  createUser :: SAML.UserId -> m UserId

  -- get session token from brig and redirect user past login process
  forwardBrigLogin :: UserId -> m Void


data SparCtx = SparCtx
  { sparCtxOpts   :: Opts
  , sparCtxLogger :: Log.Logger
  , sparCtxCas    :: Cas.ClientState
  }

newtype Spar a = Spar { fromSpar :: ReaderT SparCtx Handler a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader SparCtx, MonadError ServantErr)

instance HasConfig Spar where
  type ConfigExtra Spar = TeamId
  getConfig = asks (saml . sparCtxOpts)

instance SP Spar where
  logger lv mg = asks sparCtxLogger >>= \lg -> Spar $ Log.log lg (toLevel lv) mg'
    where
      mg' = Log.msg mg  -- TODO: there is probably more we should do to get the SAML log messages
                        -- into the right form?

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
  storeRequest i        = wrapMonadClient . Data.storeRequest i
  checkAgainstRequest r = getNow >>= \(Time now) -> wrapMonadClient $ Data.checkAgainstRequest now r
  storeAssertion i r    = getNow >>= \(Time now) -> wrapMonadClient $ Data.storeAssertion now i r

-- | Call a cassandra command in the 'Spar' monad.  Catch all exceptions and re-throw them as 500 in
-- Handler.
wrapMonadClient :: Cas.Client a -> Spar a
wrapMonadClient action = Spar $ do
  ctx <- asks sparCtxCas
  runClient ctx action `Catch.catch`
    \e@(SomeException _) -> throwError err500 { errBody = LBS.pack $ show e }


instance SPHandler Spar where
  type NTCTX Spar = SparCtx
  nt ctx (Spar action) = runReaderT action ctx


instance Brig.MonadClient Spar

instance MonadSpar Spar where
  getUser          = Brig.getUser
  createUser       = Brig.createUser
  forwardBrigLogin = Brig.forwardBrigLogin


----------------------------------------------------------------------

schemaVersion :: Int32
schemaVersion = 0

initCassandra :: Opts -> Logger -> IO Cas.ClientState
initCassandra opts lgr = do
    let connectString = ST.unpack (Options.cassandra opts ^. casEndpoint . epHost) :| []
    handle <- Cas.init (Log.clone (Just "cassandra.spar") lgr) $ Cas.defSettings
            & Cas.setContacts (NE.head connectString) (NE.tail connectString)
            & Cas.setPortNumber (fromIntegral $ Options.cassandra opts ^. casEndpoint . epPort)
            & Cas.setKeyspace (Keyspace $ Options.cassandra opts ^. casKeyspace)
            & Cas.setMaxConnections 4
            & Cas.setPoolStripes 4
            & Cas.setSendTimeout 3
            & Cas.setResponseTimeout 10
            & Cas.setProtocolVersion Cas.V3
    runClient handle $ Cas.versionCheck schemaVersion
    pure handle
