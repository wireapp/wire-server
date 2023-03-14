module SetupHelpers where

import qualified API
import App
import Data.Aeson
import Imports
import qualified Network.HTTP.Types as HTTP
import Response

randomUser :: API.CreateUser -> App (Maybe Value)
randomUser cu = bindResponse (API.createUser cu) $ \resp -> do
  resp.status @?= HTTP.status201
  pure resp.json
