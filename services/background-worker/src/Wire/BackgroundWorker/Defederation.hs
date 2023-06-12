module Wire.BackgroundWorker.Defederation where

import Network.AMQP
import Wire.BackgroundWorker.Env
import Imports
import qualified Data.Aeson as Aeson
import System.Logger
import Control.Lens ((^.))
import qualified System.Logger as Log
import Data.Domain
import Wire.API.Federation.BackendNotifications
import qualified Galley.Run as G
import qualified Galley.Env as G
import qualified Brig.Data.Connection as B
import qualified Cassandra as Cass
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Data.Qualified
import qualified Wire.Sem.Logger as L
import qualified Galley.Effects as E
import Galley.API.Error (InternalError (..), internalErrorDescription)
import Wire.API.Federation.Error (FederationError)
import Control.Monad.Trans.Except
import qualified System.Logger.Extended as Logger
import qualified Polysemy.TinyLog as P
import Wire.Sem.Logger
import Galley.Cassandra.Conversation.Members (interpretMemberStoreToCassandra)
import Galley.Cassandra.Code (interpretCodeStoreToCassandra)
import Galley.Cassandra.Conversation (interpretConversationStoreToCassandra)
import Galley.Cassandra.Team (interpretTeamStoreToCassandra)
import Galley.Intra.Effects (interpretBrigAccess, interpretGundeckAccess)
import Galley.External (interpretExternalAccess)
import Data.Text.Lazy (unpack)
import Galley.Options (setFederationDomain, optSettings, setFeatureFlags)
import Galley.Env
import Galley.Types.Teams

-- This type is used to tie the amqp sending and receiving message types together.
type MsgData = Domain

-- This function uses "backend-notifications.defederation" as it's queue name
defederateDomains :: Channel -> AppT IO ()
defederateDomains chan = do
  e <- ask
  let log' = logger e
      cass = cassandra e
  liftIO $ ensureQueue chan defederateQueue
  liftIO $ void $ consumeMsgs chan (routingKey defederateQueue) Ack $ \(message, envelope) ->
      case Aeson.eitherDecode @MsgData (msgBody message) of
      Left err' -> do
        Log.err log' $ Log.msg @Text "Could not decode message from RabbitMQ" . Log.field "error" (show err')
        nackEnv envelope
      Right dom -> do
        let success _ = do
              -- After cleaning out galley, delete remote connections from Brig's tables.
              Cass.runClient cass $ B.deleteRemoteConnectionsDomain dom
              ackEnv envelope
            failure e' = do
              Log.err log' $ Log.msg @Text "Could not decode message from RabbitMQ" . Log.field "error" e'
              nackEnv envelope
            -- TODO test that this doesn't explode EVER, or better yet work out how to avoid the problem altogether
            gEnv = undefined
            localDom = gEnv ^. options . optSettings . setFederationDomain
        either failure success <=< runExceptT . interpretGalley gEnv $ do
          -- Run code from galley to clean up conversations
          G.deleteFederationDomainRemote' localDom dom
          G.deleteFederationDomainLocal' localDom dom

type Effects =
 '[ E.ExternalAccess
  , E.GundeckAccess
  , E.BrigAccess
  , E.TeamStore
  , E.CodeStore
  , E.ConversationStore
  , E.MemberStore
  , Error FederationError
  , Error InternalError
  , L.Logger (Msg -> Msg)
  , Input Cass.ClientState
  , Input (Local ())
  , Input G.Env
  , Embed IO
  , Error String
  , Final IO
  ]

interpretGalley
  :: G.Env
  -> Sem Effects ()
  -> ExceptT String IO ()
interpretGalley env action = do
  ExceptT
    . runFinal @IO
    . runError
    . embedToFinal @IO
    . runInputConst @G.Env env
    . runInputConst (toLocalUnsafe localDomain ())
    . runInputConst cass
    . interpretTinyLog log'
    . mapError @InternalError (unpack . internalErrorDescription)
    . mapError @FederationError show
    . interpretMemberStoreToCassandra
    . interpretConversationStoreToCassandra
    . interpretCodeStoreToCassandra
    . interpretTeamStoreToCassandra lh
    . interpretBrigAccess
    . interpretGundeckAccess
    . interpretExternalAccess
    $ action
  where
    localDomain = env ^. options . optSettings . setFederationDomain
    log' = env ^. applog
    cass = env ^. cstate
    lh = env ^. options . optSettings . setFeatureFlags . flagLegalHold

interpretTinyLog ::
  Member (Embed IO) r =>
  Logger.Logger ->
  Sem (P.TinyLog ': r) a ->
  Sem r a
interpretTinyLog l = interpret $ \case
  P.Log lvl m -> Logger.log l (Wire.Sem.Logger.toLevel lvl) m