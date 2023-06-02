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
    team :: Bool,
    supportedProtocols :: Maybe [String]
  }

instance Default CreateUser where
  def =
    CreateUser
      { email = Nothing,
        password = Nothing,
        name = Nothing,
        team = False,
        supportedProtocols = Nothing
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
            <> ["supported_protocols" .= prots | prots <- toList cu.supportedProtocols]
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

instance MakesValue FedConn where
  make = pure . toJSON

instance FromJSON FedConn where
  parseJSON = withObject "FedConn" $ \obj -> do
    FedConn
      <$> obj .: fromString "domain"
      <*> obj .: fromString "search_policy"

createFedConn :: (HasCallStack, MakesValue dom, MakesValue fedConn) => dom -> fedConn -> App Response
createFedConn dom fedConn = do
  res <- createFedConn' dom fedConn
  res.status `shouldMatchRange` (200, 299)
  pure res

createFedConn' :: (HasCallStack, MakesValue dom, MakesValue fedConn) => dom -> fedConn -> App Response
createFedConn' dom fedConn = do
  req <- rawBaseRequest dom Brig Unversioned "/i/federation/remotes"
  make fedConn >>= \v -> submit "POST" $ req & addJSON v

readFedConns :: (HasCallStack, MakesValue dom) => dom -> App Response
readFedConns dom = do
  res <- readFedConns' dom
  res.status `shouldMatchRange` (200, 299)
  pure res

readFedConns' :: (HasCallStack, MakesValue dom) => dom -> App Response
readFedConns' dom = do
  req <- rawBaseRequest dom Brig Unversioned "/i/federation/remotes"
  submit "GET" req

updateFedConn :: (HasCallStack, MakesValue owndom, MakesValue fedConn) => owndom -> String -> fedConn -> App Response
updateFedConn owndom dom fedConn = do
  res <- updateFedConn' owndom dom fedConn
  res.status `shouldMatchRange` (200, 299)
  pure res

updateFedConn' :: (HasCallStack, MakesValue owndom, MakesValue fedConn) => owndom -> String -> fedConn -> App Response
updateFedConn' owndom dom fedConn = do
  req <- rawBaseRequest owndom Brig Unversioned ("/i/federation/remotes/" <> dom)
  make fedConn >>= \v -> submit "PUT" $ addJSON v req

deleteFedConn :: (HasCallStack, MakesValue owndom) => owndom -> String -> App Response
deleteFedConn owndom dom = do
  res <- deleteFedConn' owndom dom
  res.status `shouldMatchRange` (200, 299)
  pure res

deleteFedConn' :: (HasCallStack, MakesValue owndom) => owndom -> String -> App Response
deleteFedConn' owndom dom = do
  req <- rawBaseRequest owndom Brig Unversioned ("/i/federation/remotes/" <> dom)
  submit "DELETE" req

registerOAuthClient :: (HasCallStack, MakesValue user, MakesValue name, MakesValue url) => user -> name -> url -> App Response
registerOAuthClient user name url = do
  req <- baseRequest user Brig Unversioned "i/oauth/clients"
  applicationName <- asString name
  redirectUrl <- asString url
  submit "POST" (req & addJSONObject ["application_name" .= applicationName, "redirect_url" .= redirectUrl])

getOAuthClient :: (HasCallStack, MakesValue user, MakesValue cid) => user -> cid -> App Response
getOAuthClient user cid = do
  clientId <- objId cid
  req <- baseRequest user Brig Unversioned $ "i/oauth/clients/" <> clientId
  submit "GET" req

updateOAuthClient :: (HasCallStack, MakesValue user, MakesValue cid, MakesValue name, MakesValue url) => user -> cid -> name -> url -> App Response
updateOAuthClient user cid name url = do
  clientId <- objId cid
  req <- baseRequest user Brig Unversioned $ "i/oauth/clients/" <> clientId
  applicationName <- asString name
  redirectUrl <- asString url
  submit "PUT" (req & addJSONObject ["application_name" .= applicationName, "redirect_url" .= redirectUrl])

deleteOAuthClient :: (HasCallStack, MakesValue user, MakesValue cid) => user -> cid -> App Response
deleteOAuthClient user cid = do
  clientId <- objId cid
  req <- baseRequest user Brig Unversioned $ "i/oauth/clients/" <> clientId
  submit "DELETE" req
