module Federator.Health where

import Data.ByteString (fromStrict)
import Data.ByteString.UTF8 qualified as UTF8
import Imports
import Network.HTTP.Client
import Network.HTTP.Types.Status qualified as HTTP
import Servant

status ::
  Manager ->
  -- | Name of other service
  LByteString ->
  -- | Port number of the other service
  Word16 ->
  -- | standalone flag, when specified only return status of current service
  Bool ->
  Handler NoContent
status _ _ _ True = pure NoContent
status mgr otherName otherPort False = do
  req <- parseRequest $ "http://localhost:" <> show otherPort <> "/i/status?standalone"
  res <- liftIO $ httpNoBody req mgr
  if HTTP.statusIsSuccessful $ responseStatus res
    then pure NoContent
    else
      throwError
        Servant.err500
          { Servant.errBody =
              otherName
                <> " server responded with status code = "
                <> (fromStrict . UTF8.fromString . show . responseStatus $ res)
          }
