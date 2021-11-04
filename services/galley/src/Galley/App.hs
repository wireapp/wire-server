{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

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

module Galley.App
  ( -- * Environment
    Env,
    reqId,
    monitor,
    options,
    applog,
    manager,
    federator,
    brig,
    cstate,
    deleteQueue,
    createEnv,
    extEnv,
    aEnv,
    ExtEnv (..),
    extGetManager,

    -- * Galley monad
    Galley,
    GalleyEffects,
    Galley0,
    runGalley,
    evalGalley,
    ask,
    DeleteItem (..),
    toServantHandler,

    -- * Utilities
    ifNothing,
    fromJsonBody,
    fromOptionalJsonBody,
    fromProtoBody,
    initExtEnv,
    fanoutLimit,
    currentFanoutLimit,

    -- * MonadUnliftIO / Sem compatibility
    fireAndForget,
    spawnMany,
    liftGalley0,
    liftSem,
    interpretGalleyToGalley0,
  )
where

import Bilge hiding (Request, header, options, statusCode, statusMessage)
import Bilge.RPC
import Cassandra hiding (Set)
import qualified Cassandra as C
import qualified Cassandra.Settings as C
import Control.Error
import qualified Control.Exception
import Control.Lens hiding ((.=))
import Control.Monad.Catch (MonadCatch (..), MonadMask (..), MonadThrow (..))
import Data.Aeson (FromJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.Default (def)
import Data.Id (ConnId, TeamId, UserId)
import qualified Data.List.NonEmpty as NE
import Data.Metrics.Middleware
import Data.Misc (Fingerprint, Rsa)
import qualified Data.ProtocolBuffers as Proto
import Data.Proxy (Proxy (..))
import Data.Range
import Data.Serialize.Get (runGetLazy)
import Data.Text (unpack)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Galley.API.Error
import qualified Galley.Aws as Aws
import Galley.Effects
import qualified Galley.Effects.FireAndForget as E
import Galley.Options
import qualified Galley.Queue as Q
import qualified Galley.Types.Teams as Teams
import Imports hiding (forkIO)
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.OpenSSL
import Network.HTTP.Media.RenderHeader (RenderHeader (..))
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status (statusCode, statusMessage)
import Network.Wai
import Network.Wai.Utilities hiding (Error)
import qualified Network.Wai.Utilities as WaiError
import qualified Network.Wai.Utilities.Server as Server
import OpenSSL.EVP.Digest (getDigestByName)
import OpenSSL.Session as Ssl
import qualified OpenSSL.X509.SystemStore as Ssl
import Polysemy
import Polysemy.Internal (Append)
import qualified Polysemy.Reader as P
import qualified Servant
import Ssl.Util
import System.Logger.Class hiding (Error, info)
import qualified System.Logger.Extended as Logger
import qualified UnliftIO.Exception as UnliftIO
import Util.Options
import Wire.API.Federation.Client (HasFederatorConfig (..))

data DeleteItem = TeamItem TeamId UserId (Maybe ConnId)
  deriving (Eq, Ord, Show)

-- | Main application environment.
data Env = Env
  { _reqId :: RequestId,
    _monitor :: Metrics,
    _options :: Opts,
    _applog :: Logger,
    _manager :: Manager,
    _federator :: Maybe Endpoint, -- FUTUREWORK: should we use a better type here? E.g. to avoid fresh connections all the time?
    _brig :: Endpoint, -- FUTUREWORK: see _federator
    _cstate :: ClientState,
    _deleteQueue :: Q.Queue DeleteItem,
    _extEnv :: ExtEnv,
    _aEnv :: Maybe Aws.Env
  }

-- | Environment specific to the communication with external
-- service providers.
data ExtEnv = ExtEnv
  { _extGetManager :: (Manager, [Fingerprint Rsa] -> Ssl.SSL -> IO ())
  }

makeLenses ''Env

makeLenses ''ExtEnv

-- MTL-style effects derived from the old implementation of the Galley monad.
-- They will disappear as we introduce more high-level effects into Galley.
type GalleyEffects0 = '[P.Reader ClientState, P.Reader Env, Embed IO, Final IO]

type GalleyEffects = Append GalleyEffects1 GalleyEffects0

type Galley0 = Galley GalleyEffects0

newtype Galley r a = Galley {unGalley :: Members GalleyEffects0 r => Sem r a}

instance Functor (Galley r) where
  fmap f (Galley x) = Galley (fmap f x)

instance Applicative (Galley r) where
  pure x = Galley (pure x)
  (<*>) = ap

instance Monad (Galley r) where
  return = pure
  Galley m >>= f = Galley (m >>= unGalley . f)

instance MonadIO (Galley r) where
  liftIO action = Galley (liftIO action)

instance MonadThrow (Galley r) where
  throwM e = Galley (embed @IO (throwM e))

instance MonadReader Env (Galley r) where
  ask = Galley $ P.ask @Env
  local f m = Galley $ P.local f (unGalley m)

instance MonadClient (Galley r) where
  liftClient m = Galley $ do
    cs <- P.ask @ClientState
    embed @IO $ runClient cs m
  localState f m = Galley $ P.local f (unGalley m)

instance HasFederatorConfig (Galley r) where
  federatorEndpoint = view federator
  federationDomain = view (options . optSettings . setFederationDomain)

fanoutLimit :: Galley r (Range 1 Teams.HardTruncationLimit Int32)
fanoutLimit = view options >>= return . currentFanoutLimit

currentFanoutLimit :: Opts -> Range 1 Teams.HardTruncationLimit Int32
currentFanoutLimit o = do
  let optFanoutLimit = fromIntegral . fromRange $ fromMaybe defFanoutLimit (o ^. optSettings ^. setMaxFanoutSize)
  let maxTeamSize = fromIntegral (o ^. optSettings ^. setMaxTeamSize)
  unsafeRange (min maxTeamSize optFanoutLimit)

-- Define some invariants for the options used
validateOptions :: Logger.Logger -> Opts -> IO ()
validateOptions l o = do
  let settings = view optSettings o
      optFanoutLimit = fromIntegral . fromRange $ currentFanoutLimit o
  when
    ( isJust (o ^. optJournal)
        && settings ^. setMaxTeamSize > optFanoutLimit
        && not (settings ^. setEnableIndexedBillingTeamMembers . to (fromMaybe False))
    )
    $ Logger.warn
      l
      ( msg
          . val
          $ "You're journaling events for teams larger than " <> toByteString' optFanoutLimit
            <> " may have some admin user ids missing. \
               \ This is fine for testing purposes but NOT for production use!!"
      )
  when (settings ^. setMaxConvSize > fromIntegral optFanoutLimit) $
    error "setMaxConvSize cannot be > setTruncationLimit"
  when (settings ^. setMaxTeamSize < optFanoutLimit) $
    error "setMaxTeamSize cannot be < setTruncationLimit"

instance MonadLogger (Galley r) where
  log l m = do
    e <- ask
    Logger.log (e ^. applog) l (reqIdMsg (e ^. reqId) . m)

instance MonadHttp (Galley r) where
  handleRequestWithCont req handler = do
    httpManager <- view manager
    liftIO $ withResponse req httpManager handler

instance HasRequestId (Galley r) where
  getRequestId = view reqId

createEnv :: Metrics -> Opts -> IO Env
createEnv m o = do
  l <- Logger.mkLogger (o ^. optLogLevel) (o ^. optLogNetStrings) (o ^. optLogFormat)
  cass <- initCassandra o l
  mgr <- initHttpManager o
  validateOptions l o
  Env def m o l mgr (o ^. optFederator) (o ^. optBrig) cass
    <$> Q.new 16000
    <*> initExtEnv
    <*> maybe (return Nothing) (fmap Just . Aws.mkEnv l mgr) (o ^. optJournal)

initCassandra :: Opts -> Logger -> IO ClientState
initCassandra o l = do
  c <-
    maybe
      (C.initialContactsPlain (o ^. optCassandra . casEndpoint . epHost))
      (C.initialContactsDisco "cassandra_galley")
      (unpack <$> o ^. optDiscoUrl)
  C.init
    . C.setLogger (C.mkLogger (Logger.clone (Just "cassandra.galley") l))
    . C.setContacts (NE.head c) (NE.tail c)
    . C.setPortNumber (fromIntegral $ o ^. optCassandra . casEndpoint . epPort)
    . C.setKeyspace (Keyspace $ o ^. optCassandra . casKeyspace)
    . C.setMaxConnections 4
    . C.setMaxStreams 128
    . C.setPoolStripes 4
    . C.setSendTimeout 3
    . C.setResponseTimeout 10
    . C.setProtocolVersion C.V4
    $ C.defSettings

initHttpManager :: Opts -> IO Manager
initHttpManager o = do
  ctx <- Ssl.context
  Ssl.contextSetVerificationMode ctx $ Ssl.VerifyPeer True True Nothing
  Ssl.contextAddOption ctx SSL_OP_NO_SSLv2
  Ssl.contextAddOption ctx SSL_OP_NO_SSLv3
  Ssl.contextAddOption ctx SSL_OP_NO_TLSv1
  Ssl.contextSetCiphers ctx rsaCiphers
  Ssl.contextLoadSystemCerts ctx
  newManager
    (opensslManagerSettings (pure ctx))
      { managerResponseTimeout = responseTimeoutMicro 10000000,
        managerConnCount = o ^. optSettings . setHttpPoolSize,
        managerIdleConnectionCount = 3 * (o ^. optSettings . setHttpPoolSize)
      }

-- TODO: somewhat duplicates Brig.App.initExtGetManager
initExtEnv :: IO ExtEnv
initExtEnv = do
  ctx <- Ssl.context
  Ssl.contextSetVerificationMode ctx Ssl.VerifyNone
  Ssl.contextAddOption ctx SSL_OP_NO_SSLv2
  Ssl.contextAddOption ctx SSL_OP_NO_SSLv3
  Ssl.contextAddOption ctx SSL_OP_NO_TLSv1
  Ssl.contextSetCiphers ctx rsaCiphers
  Ssl.contextLoadSystemCerts ctx
  mgr <-
    newManager
      (opensslManagerSettings (pure ctx))
        { managerResponseTimeout = responseTimeoutMicro 10000000,
          managerConnCount = 100
        }
  Just sha <- getDigestByName "SHA256"
  return $ ExtEnv (mgr, mkVerify sha)
  where
    mkVerify sha fprs =
      let pinset = map toByteString' fprs
       in verifyRsaFingerprint sha pinset

runGalley :: Env -> Request -> Galley GalleyEffects a -> IO a
runGalley e r m =
  let e' = reqId .~ lookupReqId r $ e
   in evalGalley e' m

evalGalley0 :: Env -> Sem GalleyEffects0 a -> IO a
evalGalley0 e =
  runFinal @IO
    . embedToFinal @IO
    . P.runReader e
    . P.runReader (e ^. cstate)

evalGalley :: Env -> Galley GalleyEffects a -> IO a
evalGalley e = evalGalley0 e . unGalley . interpretGalleyToGalley0

lookupReqId :: Request -> RequestId
lookupReqId = maybe def RequestId . lookup requestIdName . requestHeaders

reqIdMsg :: RequestId -> Msg -> Msg
reqIdMsg = ("request" .=) . unRequestId
{-# INLINE reqIdMsg #-}

fromJsonBody :: FromJSON a => JsonRequest a -> Galley r a
fromJsonBody r = exceptT (throwM . invalidPayload) return (parseBody r)
{-# INLINE fromJsonBody #-}

fromOptionalJsonBody :: FromJSON a => OptionalJsonRequest a -> Galley r (Maybe a)
fromOptionalJsonBody r = exceptT (throwM . invalidPayload) return (parseOptionalBody r)
{-# INLINE fromOptionalJsonBody #-}

fromProtoBody :: Proto.Decode a => Request -> Galley r a
fromProtoBody r = do
  b <- readBody r
  either (throwM . invalidPayload . fromString) return (runGetLazy Proto.decodeMessage b)
{-# INLINE fromProtoBody #-}

ifNothing :: WaiError.Error -> Maybe a -> Galley r a
ifNothing e = maybe (throwM e) return
{-# INLINE ifNothing #-}

toServantHandler :: Env -> Galley GalleyEffects a -> Servant.Handler a
toServantHandler env galley = do
  eith <- liftIO $ Control.Exception.try (evalGalley env galley)
  case eith of
    Left werr ->
      handleWaiErrors (view applog env) (unRequestId (view reqId env)) werr
    Right result -> pure result
  where
    handleWaiErrors :: Logger -> ByteString -> WaiError.Error -> Servant.Handler a
    handleWaiErrors logger reqId' werr = do
      Server.logError' logger (Just reqId') werr
      Servant.throwError $
        Servant.ServerError (mkCode werr) (mkPhrase werr) (Aeson.encode werr) [(hContentType, renderHeader (Servant.contentType (Proxy @Servant.JSON)))]

    mkCode = statusCode . WaiError.code
    mkPhrase = Text.unpack . Text.decodeUtf8 . statusMessage . WaiError.code

----------------------------------------------------------------------------------
---- temporary MonadUnliftIO support code for the polysemy refactoring

fireAndForget :: Member FireAndForget r => Galley r () -> Galley r ()
fireAndForget (Galley m) = Galley $ E.fireAndForget m

spawnMany :: Member FireAndForget r => [Galley r ()] -> Galley r ()
spawnMany ms = Galley $ E.spawnMany (map unGalley ms)

instance MonadUnliftIO Galley0 where
  askUnliftIO = Galley $ do
    env <- P.ask @Env
    pure $ UnliftIO $ evalGalley0 env . unGalley

instance MonadMask Galley0 where
  mask = UnliftIO.mask
  uninterruptibleMask = UnliftIO.uninterruptibleMask
  generalBracket acquire release useB = Galley $ do
    env <- P.ask @Env
    embed @IO $
      generalBracket
        (evalGalley0 env (unGalley acquire))
        (\resource exitCase -> evalGalley0 env (unGalley (release resource exitCase)))
        (\resource -> evalGalley0 env (unGalley (useB resource)))

instance MonadCatch Galley0 where
  catch = UnliftIO.catch

liftGalley0 :: Galley0 a -> Galley r a
liftGalley0 (Galley m) = Galley $ subsume_ m

liftSem :: Sem r a -> Galley r a
liftSem m = Galley m

interpretGalleyToGalley0 :: Galley GalleyEffects a -> Galley0 a
interpretGalleyToGalley0 =
  Galley
    . interpretFireAndForget
    . interpretIntra
    . interpretBot
    . interpretFederator
    . interpretExternal
    . interpretSpar
    . interpretGundeck
    . interpretBrig
    . unGalley
