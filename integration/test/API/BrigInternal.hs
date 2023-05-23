module API.BrigInternal where

import API.Common
import qualified Data.Aeson as Aeson
import Data.Function
import Data.Maybe
import Testlib.Prelude

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
  deriving (Eq, Ord, Show)

instance ToJSON FedConn where
  toJSON (FedConn d s) =
    Aeson.object
      [ "domain" .= d,
        "search_policy" .= s
      ]

instance FromJSON FedConn where
  parseJSON = withObject "FedConn" $ \obj -> do
    FedConn
      <$> obj .: fromString "domain"
      <*> obj .: fromString "search_policy"

createFedConn :: HasCallStack => FedConn -> App Response
createFedConn fedConn = do
  res <- createFedConn' fedConn
  res.status `shouldMatchRange` (200, 299)
  pure res

createFedConn' :: HasCallStack => FedConn -> App Response
createFedConn' fedConn = do
  req <- rawBaseRequest ownDomain Brig Unversioned "/i/federation/remotes"
  submit "POST" $ req & addJSON fedConn

readFedConns :: HasCallStack => App Response
readFedConns = do
  res <- readFedConns'
  res.status `shouldMatchRange` (200, 299)
  pure res

readFedConns' :: HasCallStack => App Response
readFedConns' = do
  req <- rawBaseRequest ownDomain Brig Unversioned "/i/federation/remotes"
  submit "GET" req

updateFedConn :: HasCallStack => String -> FedConn -> App Response
updateFedConn dom fedConn = do
  res <- updateFedConn' dom fedConn
  res.status `shouldMatchRange` (200, 299)
  pure res

updateFedConn' :: HasCallStack => String -> FedConn -> App Response
updateFedConn' dom fedConn = do
  req <- rawBaseRequest ownDomain Brig Unversioned ("/i/federation/remotes/" <> dom)
  submit "PUT" (fedConn `addJSON` req)

deleteFedConn :: HasCallStack => String -> App Response
deleteFedConn dom = do
  res <- deleteFedConn' dom
  res.status `shouldMatchRange` (200, 299)
  pure res

deleteFedConn' :: HasCallStack => String -> App Response
deleteFedConn' dom = do
  req <- rawBaseRequest ownDomain Brig Unversioned ("/i/federation/remotes/" <> dom)
  submit "DELETE" req
