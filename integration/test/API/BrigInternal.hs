module API.BrigInternal where

import API.Common
import Data.Function
import Data.Maybe
import Testlib.Prelude
import qualified Data.Aeson as Aeson

data CreateUser = CreateUser
  { email :: Maybe String,
    password :: Maybe String,
    name :: Maybe String,
    team :: Bool
  }

instance Default CreateUser where
  def =
    CreateUser
      { email = Nothing,
        password = Nothing,
        name = Nothing,
        team = False
      }

createUser :: (HasCallStack, MakesValue domain) => domain -> CreateUser -> App Response
createUser domain cu = do
  email <- maybe randomEmail pure cu.email
  let password = fromMaybe defPassword cu.password
      name = fromMaybe email cu.name
  req <- baseRequest domain Brig Unversioned "/i/users"
  submit "POST" $
    req
      & addJSONObject
        ( [ "email" .= email,
            "name" .= name,
            "password" .= password,
            "icon" .= "default"
          ]
            <> [ "team"
                   .= object
                     [ "name" .= "integration test team",
                       "icon" .= "default"
                     ]
                 | cu.team
               ]
        )

data FedConn = FedConn
  { domain :: String,
    searchStrategy :: String
  }
  deriving (Eq, Ord)

instance ToJSON FedConn where
  toJSON (FedConn d s) =
    Aeson.object
      [ "domain" .= d
      , "search_policy" .= s
      ]

createFedConn :: HasCallStack => FedConn -> App Response
createFedConn fedConn = do
  dom <- ownDomain
  req <- rawBaseRequest dom Brig Versioned "/i/federation/remotes"
  res <- submit "POST" $ req & addJSON fedConn
  res.status `shouldMatchRange` (200, 299)
  pure res

{-
  post (brig . paths ["i", "federation", "remotes"] . contentJson . json remote . expect2xx)
-}

readFedConns :: HasCallStack => App Response
readFedConns = do
  dom <- ownDomain
  req <- rawBaseRequest dom Brig Versioned "/i/federation/remotes"
  res <- submit "GET" req
  res.status `shouldMatchRange` (200, 299)
  pure re

{-
  remotes . responseJsonUnsafe <$> do
    get (brig . paths ["i", "federation", "remotes"] . contentJson . expect2xx)
-}

updateFedConn :: HasCallStack => String -> FedConn -> App Response
updateFedConn _dom _fedConn = do
  dom <- ownDomain
  req <- rawBaseRequest

deleteFedConn :: HasCallStack => String -> App Response
deleteFedConn _dom = undefined

resetFedConns :: HasCallStack => App Response
resetFedConns = undefined

{-
updateFederationRemote' :: (Request -> Request) -> Brig -> Domain -> FederationDomainConfig -> Http ResponseLBS
updateFederationRemote' mods brig rdom remote =
  put (brig . paths ["i", "federation", "remotes", toByteString' rdom] . contentJson . json remote . mods)

deleteFederationRemote' :: (Request -> Request) -> Brig -> Domain -> Http ResponseLBS
deleteFederationRemote' mods brig rdom =
  delete (brig . paths ["i", "federation", "remotes", toByteString' rdom] . contentJson . mods)

-- this one needs to go elsewhere
resetFederationRemotes :: Opts -> Brig -> Http ()
resetFederationRemotes opts brig = do
  rs <- getFederationRemotes brig
  -- Filter out domains that are in the config file.
  -- These values can't be deleted yet, so don't even try.
  forM_ (notCfgRemotes rs) $ \(FederationDomainConfig rdom _) -> deleteFederationRemote brig rdom
  where
    cfgRemotes = fromMaybe [] . Opt.setFederationDomainConfigs $ Opt.optSettings opts
    notCfgRemotes = filter (`notElem` cfgRemotes)
-}
