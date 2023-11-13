module API.BrigInternal where

import API.Common
import Data.Aeson qualified as Aeson
import Data.Function
import Data.Maybe
import Testlib.Prelude

data CreateUser = CreateUser
  { email :: Maybe String,
    password :: Maybe String,
    name :: Maybe String,
    team :: Bool,
    activate :: Bool,
    supportedProtocols :: Maybe [String]
  }

instance Default CreateUser where
  def =
    CreateUser
      { email = Nothing,
        password = Nothing,
        name = Nothing,
        team = False,
        activate = True,
        supportedProtocols = Nothing
      }

createUser :: (HasCallStack, MakesValue domain) => domain -> CreateUser -> App Response
createUser domain cu = do
  re <- randomEmail
  let email :: Maybe String = guard cu.activate $> fromMaybe re cu.email
  let password = fromMaybe defPassword cu.password
      name = fromMaybe "default" (cu.name <|> email)
  req <- baseRequest domain Brig Unversioned "/i/users"
  submit "POST" $
    req
      & addJSONObject
        ( ["email" .= e | e <- toList email]
            <> [ "name" .= name,
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
    searchStrategy :: String,
    restriction :: String
  }
  deriving (Eq, Ord, Show)

instance ToJSON FedConn where
  toJSON (FedConn d s r) =
    Aeson.object
      [ "domain" .= d,
        "search_policy" .= s,
        "restriction" .= r
      ]

instance MakesValue FedConn where
  make = pure . toJSON

createFedConn :: (HasCallStack, MakesValue dom, MakesValue fedConn) => dom -> fedConn -> App Response
createFedConn dom fedConn = do
  bindResponse (createFedConn' dom fedConn) $ \res -> do
    res.status `shouldMatchRange` (200, 299)
    pure res

createFedConn' :: (HasCallStack, MakesValue dom, MakesValue fedConn) => dom -> fedConn -> App Response
createFedConn' dom fedConn = do
  req <- rawBaseRequest dom Brig Unversioned "/i/federation/remotes"
  conn <- make fedConn
  submit "POST" $ req & addJSON conn

readFedConns :: (HasCallStack, MakesValue dom) => dom -> App Response
readFedConns dom = do
  bindResponse (readFedConns' dom) $ \res -> do
    res.status `shouldMatchRange` (200, 299)
    pure res

readFedConns' :: (HasCallStack, MakesValue dom) => dom -> App Response
readFedConns' dom = do
  req <- rawBaseRequest dom Brig Unversioned "/i/federation/remotes"
  submit "GET" req

updateFedConn :: (HasCallStack, MakesValue owndom, MakesValue fedConn) => owndom -> String -> fedConn -> App Response
updateFedConn owndom dom fedConn = do
  bindResponse (updateFedConn' owndom dom fedConn) $ \res -> do
    res.status `shouldMatchRange` (200, 299)
    pure res

updateFedConn' :: (HasCallStack, MakesValue owndom, MakesValue fedConn) => owndom -> String -> fedConn -> App Response
updateFedConn' owndom dom fedConn = do
  req <- rawBaseRequest owndom Brig Unversioned ("/i/federation/remotes/" <> dom)
  conn <- make fedConn
  submit "PUT" $ addJSON conn req

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

getInvitationCode :: (HasCallStack, MakesValue user, MakesValue inv) => user -> inv -> App Response
getInvitationCode user inv = do
  tid <- user %. "team" & asString
  invId <- inv %. "id" & asString
  req <-
    baseRequest user Brig Unversioned $
      "i/teams/invitation-code?team=" <> tid <> "&invitation_id=" <> invId
  submit "GET" req

refreshIndex :: (HasCallStack, MakesValue domain) => domain -> App ()
refreshIndex domain = do
  req <- baseRequest domain Brig Unversioned "i/index/refresh"
  res <- submit "POST" req
  res.status `shouldMatchInt` 200

connectWithRemoteUser :: (MakesValue userFrom, MakesValue userTo) => userFrom -> userTo -> App ()
connectWithRemoteUser userFrom userTo = do
  userFromId <- objId userFrom
  qUserTo <- make userTo
  let body = ["tag" .= "CreateConnectionForTest", "user" .= userFromId, "other" .= qUserTo]
  req <-
    baseRequest userFrom Brig Unversioned $
      joinHttpPath ["i", "connections", "connection-update"]
  res <- submit "PUT" (req & addJSONObject body)
  res.status `shouldMatchInt` 200

addFederationRemoteTeam :: (HasCallStack, MakesValue domain, MakesValue remoteDomain, MakesValue team) => domain -> remoteDomain -> team -> App ()
addFederationRemoteTeam domain remoteDomain team = do
  d <- asString remoteDomain
  t <- make team
  req <- baseRequest domain Brig Unversioned $ joinHttpPath ["i", "federation", "remotes", d, "teams"]
  res <- submit "POST" (req & addJSONObject ["team_id" .= t])
  res.status `shouldMatchInt` 200

getFederationRemoteTeams :: (HasCallStack, MakesValue domain, MakesValue remoteDomain) => domain -> remoteDomain -> App Response
getFederationRemoteTeams domain remoteDomain = do
  d <- asString remoteDomain
  req <- baseRequest domain Brig Unversioned $ joinHttpPath ["i", "federation", "remotes", d, "teams"]
  submit "GET" req

deleteFederationRemoteTeam :: (HasCallStack, MakesValue domain, MakesValue remoteDomain, MakesValue team) => domain -> remoteDomain -> team -> App ()
deleteFederationRemoteTeam domain remoteDomain team = do
  d <- asString remoteDomain
  t <- asString team
  req <- baseRequest domain Brig Unversioned $ joinHttpPath ["i", "federation", "remotes", d, "teams", t]
  res <- submit "DELETE" req
  res.status `shouldMatchInt` 200
