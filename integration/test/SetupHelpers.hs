module SetupHelpers where

import qualified API
import App
import Data.Aeson
import Imports
import JSON
import qualified Network.HTTP.Types as HTTP
import Response

randomUser :: API.CreateUser -> App Value
randomUser cu = bindResponse (API.createUser cu) $ \resp -> do
  resp.status @?= HTTP.status201
  get resp.json
