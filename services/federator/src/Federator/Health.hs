module Federator.Health where

import Imports
import Network.HTTP.Client
import qualified Network.HTTP.Types.Status as HTTP
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
    else throwError Servant.err500 {Servant.errBody = otherName <> " server responded with status code = " <> cs (show (responseStatus res))}
