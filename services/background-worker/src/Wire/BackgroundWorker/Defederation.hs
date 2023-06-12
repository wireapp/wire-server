module Wire.BackgroundWorker.Defederation where

import Network.AMQP
import Wire.BackgroundWorker.Env
import Imports
import qualified Data.Aeson as Aeson
import System.Logger
import qualified System.Logger as Log
import Data.Domain
import Wire.API.Federation.BackendNotifications
import qualified Galley.Run as G
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

-- This type is used to tie the amqp sending and receiving message types together.
type MsgData = Domain

-- This function uses "backend-notifications.defederation" as it's queue name
defederateDomains :: Channel -> AppT IO ()
defederateDomains chan = do
  e <- ask
  let log' = logger e
      cass = cassandra' e
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
            localDom = localDomain e
        either failure success <=< runExceptT . interpretGalley e $ do
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
  , Input Env
  , Embed IO
  , Error String
  , Final IO
  ]

interpretGalley
  :: Env
  -> Sem Effects ()
  -> ExceptT String IO ()
interpretGalley env action = do
  ExceptT
    . runFinal @IO
    . runError
    . embedToFinal @IO
    . runInputConst env
    . runInputConst (toLocalUnsafe localDom ())
    . runInputConst cass
    . interpretTinyLog log'
    . mapError @InternalError (unpack . internalErrorDescription)
    . mapError @FederationError show
    . interpretMemberStoreToCassandra
    . interpretConversationStoreToCassandra
    . interpretCodeStoreToCassandra @Wire.BackgroundWorker.Env.Env
    . interpretTeamStoreToCassandra @Wire.BackgroundWorker.Env.Env lh
    . interpretBrigAccess @Wire.BackgroundWorker.Env.Env
    . interpretGundeckAccess @Wire.BackgroundWorker.Env.Env
    . interpretExternalAccess @Wire.BackgroundWorker.Env.Env
    $ action
  where
    localDom = localDomain env
    log' = logger env
    cass = cassandra' env
    lh = legalHoldFlag env

interpretTinyLog ::
  Member (Embed IO) r =>
  Logger.Logger ->
  Sem (P.TinyLog ': r) a ->
  Sem r a
interpretTinyLog l = interpret $ \case
  P.Log lvl m -> Logger.log l (Wire.Sem.Logger.toLevel lvl) m