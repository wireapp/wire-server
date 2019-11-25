module Network.Wire.Client.API.Team where

import Brig.Types
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import Data.Proxy
import Data.UUID as UUID
import Data.UUID.V4 as UUID
import Imports
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API
import Servant.Client
import Util.Options.Common

-- import Galley.Types

data Team
  = Team
      { tName :: String,
        tIcon :: String
      }
  deriving (Show, Generic)

deriveJSON toOptionFieldName 'Team

data TeamUser
  = TeamUser
      { name :: String,
        email :: String,
        password :: String,
        team :: Team
      }
  deriving (Show, Generic)

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
teamOwner prefix = TeamUser
  { name = "Owner",
    email = prefix <> "@example.com",
    password = "password",
    team = Team "Teamname" "icon"
  }

---------------------------------------------------
-- manual testing. TODO: move elsewhere, refactor
--

queries :: ClientM ()
queries = do
  emailPrefix <- UUID.toString <$> liftIO nextRandom
  _ <- createUser $ teamOwner emailPrefix
  return ()

run :: IO ()
run = do
  manager' <- newManager defaultManagerSettings
  -- TODO: don't hardcode to local brig
  res <- runClientM queries (mkClientEnv manager' (BaseUrl Http "localhost" 8082 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right () -> putStrLn "Yay"
