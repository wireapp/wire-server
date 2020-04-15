module Galley.Intra.Federator
  ( joinConversationById,
  )
where

import Control.Lens (view)
import Control.Monad.Catch (throwM)
import Data.Id (ConvId, UserId)
import Data.Qualified (Qualified)
import Data.String.Conversions (cs)
import qualified Data.Text as Text
import Federator.API (API)
import qualified Federator.API as API
import Galley.API.Error (internalErrorWithDescription)
import Galley.App (Galley, manager, options)
import Galley.Options (optFederator)
import Imports
import Servant.Client (ClientError (FailureResponse), ClientM, mkClientEnv, runClientM)
import Servant.Client.Core (BaseUrl (BaseUrl), Scheme (Http))
import Servant.Client.Generic (AsClientT, genericClientHoist)
import Util.Options (Endpoint (Endpoint, _epHost, _epPort))

joinConversationById :: Qualified ConvId -> Qualified UserId -> Galley (API.ConversationUpdateResult API.MemberJoin)
joinConversationById convId userId =
  API._gapiJoinConversationById client convId (API.JoinConversationByIdRequest userId)

client :: API (AsClientT Galley)
client = genericClientHoist nat
  where
    nat :: ClientM a -> Galley a
    nat clientM = do
      env <-
        mkClientEnv
          <$> view manager
          <*> viewBaseUrl
      -- We run the same error handler for all endpoints.
      -- You can handle errors in each function by instead making
      -- `client :: API (AsClientT (ExceptT ClientError Galley))`
      handleClientError =<< liftIO (runClientM clientM env)
    viewBaseUrl = do
      view (options . optFederator) >>= \case
        Just (Endpoint {_epHost, _epPort}) ->
          pure $ BaseUrl Http (Text.unpack _epHost) (fromIntegral _epPort) ""
        Nothing ->
          throwM (internalErrorWithDescription "no endpoint configured for federator")

handleClientError :: Either ClientError a -> Galley a
handleClientError = \case
  Right res ->
    pure res
  Left (FailureResponse _req rsp) ->
    throwM . internalErrorWithDescription . cs $
      "received error response from federator:\n" <> show rsp
  Left otherError ->
    throwM . internalErrorWithDescription . cs $
      "error when calling federator:\n" <> show otherError
