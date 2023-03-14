module API where

import App
import Config
import Data.Aeson ((.=))
import qualified Data.Array as Array
import Data.Default
import Imports
import Response
import System.Random

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
