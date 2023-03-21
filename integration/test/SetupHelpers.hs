module SetupHelpers where

import qualified API
import App
import Data.Aeson
import Imports

randomUser :: API.CreateUser -> App Value
randomUser cu = bindResponse (API.createUser cu) $ \resp -> do
  resp.status @?= 201
  resp.json
