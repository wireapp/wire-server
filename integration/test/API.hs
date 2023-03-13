module API where

import App
import Config
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Array as Array
import Data.Default
import Imports
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types as HTTP
import System.Random

data Response = Response
  { json :: Maybe Aeson.Value,
    status :: HTTP.Status,
    request :: HTTP.Request
  }

baseRequest :: Service -> String -> App HTTP.Request
baseRequest service path = do
  ctx <- getContext
  liftIO . HTTP.parseRequest $
    "http://localhost:" <> show (servicePort ctx.serviceMap service) <> path

addJSONObject :: [Aeson.Pair] -> HTTP.Request -> HTTP.Request
addJSONObject = addJSON . Aeson.object

addJSON :: Aeson.Value -> HTTP.Request -> HTTP.Request
addJSON obj req =
  req
    { HTTP.requestBody = HTTP.RequestBodyLBS (Aeson.encode obj),
      HTTP.requestHeaders =
        ("Content-Type", "application/json")
          : HTTP.requestHeaders req
    }

submit :: HTTP.Request -> App Response
submit req = do
  manager <- getManager
  res <- liftIO $ HTTP.httpLbs req manager
  pure $
    Response
      { json = Aeson.decode (HTTP.responseBody res),
        request = req,
        status = HTTP.responseStatus res
      }

randomEmail :: App String
randomEmail = liftIO $ do
  n <- randomRIO (8, 15)
  replicateM n pick
  where
    chars :: Array.Array Int Char
    chars = mkArray $ filter (/= '@') ['!' .. '~']

    mkArray :: [a] -> Array.Array Int a
    mkArray l = Array.listArray (0, length l - 1) l

    pick :: IO Char
    pick = do
      i <- randomRIO (Array.bounds chars)
      pure (chars Array.! i)

defPassword :: String
defPassword = "s3cret"

data CreateUser = CreateUser
  { email :: Maybe String,
    password :: Maybe String,
    name :: Maybe String
  }

instance Default CreateUser where
  def =
    CreateUser
      { email = Nothing,
        password = Nothing,
        name = Nothing
      }

createUser :: CreateUser -> App Response
createUser cu = do
  email <- maybe randomEmail pure cu.email
  let password = fromMaybe defPassword cu.password
      name = fromMaybe email cu.name
  req <- baseRequest Brig "/i/users"
  submit $
    addJSONObject
      [ "email" .= email,
        "name" .= name,
        "password" .= password,
        "icon" .= ("default" :: String)
      ]
      req
