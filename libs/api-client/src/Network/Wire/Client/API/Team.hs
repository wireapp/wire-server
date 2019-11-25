{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Network.Wire.Client.API.Team where

import Imports
import Data.Aeson
import Data.Proxy
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.API
import Servant.Client

import Data.Aeson.TH (deriveJSON)
import Util.Options.Common

import Brig.Types
-- import Galley.Types

data Team = Team
    { tName :: String
    , tIcon :: String
    } deriving (Show, Generic)

deriveJSON toOptionFieldName 'Team

data TeamUser = TeamUser
    { name :: String
    , email :: String
    , password :: String
    , team :: Team
    } deriving (Show, Generic)

instance ToJSON TeamUser
instance FromJSON TeamUser

type InternalAPI = "i" :> "users" :> ReqBody '[JSON] TeamUser :> Post '[JSON] SelfProfile

internalAPI :: Proxy InternalAPI
internalAPI = Proxy

createUser = client internalAPI

createUser :: TeamUser -> ClientM SelfProfile


-- re-using the existing types is rather tedious here
-- teamOwner :: NewUser
-- teamOwner = NewUser 
--             { newUserName = "Owner"
--             , newUserUUID = Nothing
--             , newUserOrigin = NewUserOriginTeamUser (NewTeamCreator (BindingNewTeam ()))
--             }


teamOwner :: String -> TeamUser
teamOwner prefix = TeamUser { name = "Owner"
                     , email = prefix <> "@example.com"
                     , password = "password"
                     , team = Team "Teamname" "icon"
                     }

queries :: ClientM ()
queries = do
    _ <- createUser $ teamOwner "hui"
    return ()


run :: IO ()
run = do
    manager' <- newManager defaultManagerSettings
    -- TODO: don't hardcode to local brig
    res <- runClientM queries (mkClientEnv manager' (BaseUrl Http "localhost" 8082 ""))
    case res of
       Left err -> putStrLn $ "Error: " ++ show err
       Right () -> putStrLn "Yay"
