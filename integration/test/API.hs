module API where

import Data.Aeson hiding ((.=))
import qualified Data.Array as Array
import Data.Default
import Imports
import System.Random (randomRIO)
import TestLib.App

randomEmail :: App String
randomEmail = liftIO $ do
  n <- randomRIO (8, 15)
  u <- replicateM n pick
  pure $ u <> "@example.com"
  where
    chars :: Array.Array Int Char
    chars = mkArray $ ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9']

    mkArray :: [a] -> Array.Array Int a
    mkArray l = Array.listArray (0, length l - 1) l

    pick :: IO Char
    pick = do
      i <- randomRIO (Array.bounds chars)
      pure (chars Array.! i)

defPassword :: String
defPassword = "hunter2!"

data CreateUser = CreateUser
  { email :: Maybe String,
    password :: Maybe String,
    name :: Maybe String,
    team :: Bool,
    teamPermissions :: Maybe String
  }

instance Default CreateUser where
  def =
    CreateUser
      { email = Nothing,
        password = Nothing,
        name = Nothing,
        team = False,
        teamPermissions = Nothing
      }

createUser :: CreateUser -> App Response
createUser cu = do
  email <- maybe randomEmail pure cu.email
  let password = fromMaybe defPassword cu.password
      name = fromMaybe email cu.name
  req <- baseRequest Brig Unversioned "/i/users"
  resp <-
    submit "POST" $
      addJSONObject
        ( [ "email" .= email,
            "name" .= name,
            "password" .= password,
            "icon" .= ("default" :: String)
          ]
            <> [ "team"
                   .= object
                     [ "name" .= ("integration test team" :: String),
                       "icon" .= ("default" :: String)
                     ]
                 | cu.team
               ]
        )
        req
  _ <- case cu.teamPermissions of
    Nothing -> pure ()
    Just perms -> do
      uid <- resp.json %. "id" & asString
      tid <- resp.json %. "team" & asString
      req2 <- baseRequest Galley Unversioned ("/i/team/" <> tid <> "/members")
      let call2 =
            submit "PUT" $
              addJSONObject
                [ "user" .= uid,
                  "permissions" .= ["self" .= perms, "copy" .= perms]
                ]
                req2
      bindResponse call2 $ \resp2 -> resp2.status `shouldMatchInt` 200
  pure resp

searchContact :: String -> String -> App Response
searchContact searchingUserId searchTerm = do
  undefined searchingUserId searchTerm

getTeams :: String -> App Response
getTeams userId = do
  req <- baseRequest Galley Versioned "/teams"
  let req' =
        req
          & zUser userId
          & zConnection "conn"
          & zType "access"
  submit "GET" req'

data AddClient = AddClient
  { ctype :: String,
    internal :: Bool,
    clabel :: String,
    model :: String,
    prekeys :: Maybe [Value],
    lastPrekey :: Maybe Value,
    password :: String
  }

instance Default AddClient where
  def =
    AddClient
      { ctype = "permanent",
        internal = False,
        clabel = "Test Device",
        model = "Test Model",
        prekeys = Nothing,
        lastPrekey = Nothing,
        password = defPassword
      }

addClient ::
  (HasCallStack, ProducesJSON user) =>
  user ->
  AddClient ->
  App Response
addClient user args = do
  uid <- objId user
  req <- baseRequest Brig Unversioned $ "/i/clients/" <> uid
  pks <- maybe (fmap pure getPrekey) pure args.prekeys
  lpk <- maybe getLastPrekey pure args.lastPrekey
  submit "POST" $
    req
      & addJSONObject
        [ "prekeys" .= pks,
          "lastkey" .= lpk,
          "type" .= args.ctype,
          "label" .= args.clabel,
          "model" .= args.model,
          "password" .= args.password
        ]

deleteClient ::
  (HasCallStack, ProducesJSON user, ProducesJSON client) =>
  user ->
  Maybe String ->
  client ->
  App Response
deleteClient user mconn client = do
  let conn = fromMaybe "0" mconn
  uid <- user & objId
  cid <- client & asString
  req <- baseRequest Brig Unversioned $ "/clients/" <> cid
  submit "DELETE" $
    req
      & zUser uid
      & zConnection conn
      & addJSONObject
        [ "password" .= defPassword
        ]

getTeamFeatureInternal :: HasCallStack => String -> String -> App Response
getTeamFeatureInternal featureName tid = do
  req <- baseRequest Galley Unversioned $ joinHttpPath ["i", "teams", tid, "features", featureName]
  submit "GET" $ req
