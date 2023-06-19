module API.BrigInternal where

import API.Common
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

refreshIndex :: (HasCallStack, MakesValue domain) => domain -> App ()
refreshIndex domain = do
  req <- baseRequest domain Brig Unversioned "i/index/refresh"
  res <- submit "POST" req
  res.status `shouldMatchInt` 200
