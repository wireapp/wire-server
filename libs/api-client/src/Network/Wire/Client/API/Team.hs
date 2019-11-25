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

import Brig.Types
-- import Servant.Types.SourceT (foreach)

-- import qualified Servant.Client.Streaming as S
--

-- | URI scheme to use
-- data HttpScheme =
--     Http  -- ^ http://
--   | Https -- ^ https://

-- | Simple data type to represent the target of HTTP requests
--   for servant's automatically-generated clients.
-- data BaseUrl = BaseUrl
--     { baseUrlScheme :: HttpScheme -- ^ URI scheme to use
--     , baseUrlHost :: String   -- ^ host (eg "haskell.org")
--     , baseUrlPort :: Int      -- ^ port (eg 80)
--     , baseUrlPath :: String   -- ^ path (eg "/a/b/c")
--     }


type InternalAPI = "i" :> "users" :> ReqBody '[JSON] TeamUser :> Post '[JSON] SelfProfile

internalAPI :: Proxy InternalAPI
internalAPI = Proxy

createUser = client internalAPI

data TeamUser = TeamUser
    { name :: String
    , email :: String
    , password :: String
    } deriving (Show, Generic)

instance ToJSON TeamUser
instance FromJSON TeamUser


teamOwner :: TeamUser
teamOwner = TeamUser { name = "Owner", email = "user1234@example.com" , password = "password" }

createUser :: TeamUser -> ClientM SelfProfile


-- re-using the existing types is rather tedious here
-- teamOwner :: NewUser
-- teamOwner = NewUser 
--             { newUserName = "Owner"
--             , newUserUUID = Nothing
--             , newUserOrigin = NewUserOriginTeamUser (NewTeamCreator (BindingNewTeam ()))
--             }

queries :: ClientM ()
queries = do
    _ <- createUser teamOwner
    return ()


run :: IO ()
run = do
    manager' <- newManager defaultManagerSettings
    -- TODO: don't hardcode to local brig
    res <- runClientM queries (mkClientEnv manager' (BaseUrl Http "localhost" 8082 ""))
    case res of
       Left err -> putStrLn $ "Error: " ++ show err
       Right () -> putStrLn "Yay"
