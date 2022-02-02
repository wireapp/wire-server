module Wire.API.Federation.Example where

import Control.Monad.Codensity
import Control.Monad.Except
import Data.Handle
import Imports
import Wire.API.Federation.API.Brig
import Wire.API.Federation.Client
import Wire.API.Federation.Endpoint
import Wire.API.Federation.Error
import Wire.API.User

example :: FederatorClientEnv -> Handle -> ExceptT FederatorClientError (Codensity IO) (Maybe UserProfile)
example env h = case runFederatorClientWithNegotiation @(BrigApi @! "get-user-by-handle") env of
  SomeClient _ cli -> cli h
