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

    -- * Running Galley effects
    GalleyEffects,
    evalGalley,
    ask,
    DeleteItem (..),
    toServantHandler,
  )
where

import Bilge hiding (Request, header, options, statusCode, statusMessage)
import Cassandra hiding (Set)
import qualified Cassandra as C
import qualified Cassandra.Settings as C
import Control.Error
import qualified Control.Exception
import Control.Lens hiding ((.=))
import qualified Data.Aeson as Aeson
import Data.ByteString.Conversion (toByteString')
import Data.Default (def)
import qualified Data.List.NonEmpty as NE
import Data.Metrics.Middleware
import Data.Proxy (Proxy (..))
import Data.Qualified
import Data.Range
import Data.Text (unpack)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Time.Clock
import Galley.API.Error
import qualified Galley.Aws as Aws
import Galley.Cassandra.Client
import Galley.Cassandra.Code
import Galley.Cassandra.Conversation
import Galley.Cassandra.Conversation.Members
import Galley.Cassandra.ConversationList
import Galley.Cassandra.CustomBackend
import Galley.Cassandra.LegalHold
import Galley.Cassandra.SearchVisibility
import Galley.Cassandra.Services
import Galley.Cassandra.Team
import Galley.Cassandra.TeamFeatures
import Galley.Cassandra.TeamNotifications
import Galley.Effects
import Galley.Effects.FireAndForget (interpretFireAndForget)
import Galley.Effects.WaiRoutes.IO
import Galley.Env
import Galley.External
import Galley.Intra.Effects
import Galley.Intra.Federator
import Galley.Options
import Galley.Queue
import qualified Galley.Queue as Q
import qualified Galley.Types.Teams as Teams
import Imports hiding (forkIO)
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Client.OpenSSL
import Network.HTTP.Media.RenderHeader (RenderHeader (..))
import Network.HTTP.Types (hContentType)
import Network.HTTP.Types.Status (statusCode, statusMessage)
import qualified Network.Wai.Utilities as Wai
import qualified Network.Wai.Utilities.Server as Server
import OpenSSL.Session as Ssl
import qualified OpenSSL.X509.SystemStore as Ssl
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Internal (Append)
import qualified Polysemy.TinyLog as P
import qualified Servant
import Ssl.Util
import System.Logger.Class
import qualified System.Logger.Extended as Logger
import qualified UnliftIO.Exception as UnliftIO
import Util.Options

-- Effects needed by the interpretation of other effects
type GalleyEffects0 = '[Input ClientState, Input Env, Embed IO, Final IO]

type GalleyEffects = Append GalleyEffects1 GalleyEffects0

-- Define some invariants for the options used
validateOptions :: Logger -> Opts -> IO ()
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
    . C.setPolicy (C.dcFilterPolicyIfConfigured l (o ^. optCassandra . casFilterNodesByDatacentre))
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

interpretTinyLog ::
  Members '[Embed IO] r =>
  Env ->
  Sem (P.TinyLog ': r) a ->
  Sem r a
interpretTinyLog e = interpret $ \case
  P.Polylog l m -> Logger.log (e ^. applog) l (reqIdMsg (e ^. reqId) . m)

toServantHandler :: Env -> Sem GalleyEffects a -> Servant.Handler a
toServantHandler env galley = do
  eith <- liftIO $ Control.Exception.try (evalGalley env galley)
  case eith of
    Left werr ->
      handleWaiErrors (view applog env) (unRequestId (view reqId env)) werr
    Right result -> pure result
  where
    handleWaiErrors :: Logger -> ByteString -> Wai.Error -> Servant.Handler a
    handleWaiErrors logger reqId' werr = do
      Server.logError' logger (Just reqId') werr
      Servant.throwError $
        Servant.ServerError (mkCode werr) (mkPhrase werr) (Aeson.encode werr) [(hContentType, renderHeader (Servant.contentType (Proxy @Servant.JSON)))]

    mkCode = statusCode . Wai.code
    mkPhrase = Text.unpack . Text.decodeUtf8 . statusMessage . Wai.code

interpretErrorToException ::
  (Exception e, Member (Embed IO) r) =>
  Sem (Error e ': r) a ->
  Sem r a
interpretErrorToException = (either (embed @IO . UnliftIO.throwIO) pure =<<) . runError

evalGalley :: Env -> Sem GalleyEffects a -> IO a
evalGalley e action = do
  runFinal @IO
    . embedToFinal @IO
    . runInputConst e
    . runInputConst (e ^. cstate)
    . interpretErrorToException
    . mapAllErrors
    . interpretTinyLog e
    . interpretQueue (e ^. deleteQueue)
    . runInputSem (embed getCurrentTime) -- FUTUREWORK: could we take the time only once instead?
    . interpretWaiRoutes
    . runInputConst (e ^. options)
    . runInputConst (toLocalUnsafe (e ^. options . optSettings . setFederationDomain) ())
    . interpretInternalTeamListToCassandra
    . interpretTeamListToCassandra
    . interpretLegacyConversationListToCassandra
    . interpretRemoteConversationListToCassandra
    . interpretConversationListToCassandra
    . interpretTeamMemberStoreToCassandra lh
    . interpretTeamStoreToCassandra lh
    . interpretTeamNotificationStoreToCassandra
    . interpretTeamFeatureStoreToCassandra
    . interpretServiceStoreToCassandra
    . interpretSearchVisibilityStoreToCassandra
    . interpretMemberStoreToCassandra
    . interpretLegalHoldStoreToCassandra lh
    . interpretCustomBackendStoreToCassandra
    . interpretConversationStoreToCassandra
    . interpretCodeStoreToCassandra
    . interpretClientStoreToCassandra
    . interpretFireAndForget
    . interpretBotAccess
    . interpretFederatorAccess
    . interpretExternalAccess
    . interpretGundeckAccess
    . interpretSparAccess
    . interpretBrigAccess
    $ action
  where
    lh = view (options . optSettings . setFeatureFlags . Teams.flagLegalHold) e
