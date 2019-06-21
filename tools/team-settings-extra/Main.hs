{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Brig.Types.Activation
import Brig.Types.Intra
import Brig.Types.User
import Control.Exception
import Data.Aeson
import Data.Id
import Data.String.Conversions
import Galley.Types.Teams
import GHC.Generics (Generic)
import Imports hiding (error)
import qualified Data.Csv as Csv
import qualified Data.UUID as UUID
import qualified Network.HTTP.Client as HTTP
import qualified Servant.API as Servant
import qualified Servant.Client as SC
import qualified Servant.Client.Generic as SCG
import Servant.API.Generic
import Servant.API hiding (Get, Put, Post, Delete, ReqBody, QueryParam, QueryParam')
import System.Environment
import System.IO.Unsafe (unsafePerformIO)
import WithCli


-- * csv data with user info

data UserRecord = UserRecord
  { userName  :: Name
  , userEmail :: Email
  }
  deriving (Eq, Show, Generic)

readUsers :: File -> IO [UserRecord]
readUsers (File fp) = do
  raw <- cs <$> readFile fp
  let opts = Csv.defaultDecodeOptions {
        Csv.decDelimiter = fromIntegral (ord ';')
      }
  either (error . show) (pure . toList) $
    Csv.decodeWith @UserRecord opts Csv.NoHeader raw

instance Csv.FromRecord UserRecord

instance Csv.FromField Name where
  parseField = fmap Name . Csv.parseField

instance Csv.FromField Email where
  parseField fld = maybe mzero pure . parseEmail =<< Csv.parseField fld


-- * shell env (for server url prefices) and servant client runner

data ShellEnv = ShellEnv
  { brigUrl   :: String
  , galleyUrl :: String
  }
  deriving (Eq, Show)

loadShellEnv :: IO ShellEnv
loadShellEnv = do
  env <- getEnvironment
  brigUrl   <- maybe (error "brigUrl")   pure $ lookup "WIRE_BRIG_URL"   env
  galleyUrl <- maybe (error "galleyUrl") pure $ lookup "WIRE_GALLEY_URL" env
  pure ShellEnv{..}

callBrig :: ShellEnv -> SC.ClientM a -> IO a
callBrig = callAny brigUrl

callGalley :: ShellEnv -> SC.ClientM a -> IO a
callGalley = callAny galleyUrl

callAny :: (ShellEnv -> String) -> ShellEnv -> SC.ClientM a -> IO a
callAny serviceUrl env client = do
  mgr <- HTTP.newManager HTTP.defaultManagerSettings
  url <- SC.parseBaseUrl $ serviceUrl env
  result <- SC.runClientM client $ SC.mkClientEnv mgr url
  either (error . show) pure result


-- * cli

data Command
  = CreateTeamWithAdmin
  | BulkCreateTeamMembers
  deriving (Eq, Show, Bounded, Enum, Generic)

instance HasArguments Command where
  argumentsParser = atomicArgumentsParser

instance Argument Command where
  argumentType Proxy = show @[Command] [minBound..]
  parseArgument x = case [ x' | x' <- [minBound..], show x' == x ] of
    [x'] -> Just x'
    bad  -> unsafeError $ "invalid command: " <> show bad


data File = File FilePath
  deriving (Eq, Show, Typeable, Generic)

instance HasArguments File where
  argumentsParser = atomicArgumentsParser

instance Argument File where
  argumentType Proxy = "file"
  parseArgument f = Just (File f)


instance HasArguments Name where
  argumentsParser = atomicArgumentsParser

instance Argument Name where
  argumentType Proxy = "user name"
  parseArgument = Just . Name . cs


instance HasArguments Email where
  argumentsParser = atomicArgumentsParser

instance Argument Email where
  argumentType Proxy = "email"
  parseArgument = parseEmail . cs


-- * main

main :: IO ()
main = withCli run

run :: Command -> Name -> Email -> IO ()
run cmd name email = do
  env <- loadShellEnv
  case cmd of
    CreateTeamWithAdmin -> createTeamWithAdmin env undefined name email
    BulkCreateTeamMembers -> bulkCreateTeamMembers env undefined undefined


-- * commands

{-

call /register with a 'NewUser':

{ "name": "Dr. Carsten Klarstein"
, "email": "ck@ck.com"
, "team": {"name": "team ck"}
}

that's it!  respond status code and potential errors, perhaps.  now user will get an email,
conclude registration process, and be team admin!

can (probably?) be called in front of and behind nginz.

-- TODO: the team icon asset id is marked as non-optional.  perhaps we can create that
-- somehow?  or is that just a lie in the swagger docs?

-}
createTeamWithAdmin :: ShellEnv -> Name -> Name -> Email -> IO ()
createTeamWithAdmin _env _teamName _userName _userEmail = do
  _ <- undefined registerTeamAdmin
  pure ()


{-

internally (behind nginz).

csv-datei mit name, email.  team admin userid.

internal-create-user (wie unten).  dann:
post /i/teams/{id}/members -d'{ "member": { "user": "990fc314-943c-11e9-9364-3f3a746e15d5" } }'

-}
bulkCreateTeamMembers :: ShellEnv -> Email -> File -> IO ()
bulkCreateTeamMembers env teamAdminEmail userRecordsFile = do
  userRecords <- readUsers userRecordsFile
  tid <- getTeamAdminEmail env teamAdminEmail
  forM_ userRecords $ \userRec -> do
    uid <- createUserNoVerify env $ simpleNewUser userRec
    addUserToTeam env uid tid


-- * servant client

createUserNoVerify :: ShellEnv -> NewUser -> IO UserId
createUserNoVerify env newUser = fmap extr . callBrig env $ createUserNoVerify_ newUser
  where
    extr :: Headers '[Header "Location" UserId] SelfProfile -> UserId
    extr = Brig.Types.User.userId . selfUser . getResponse

getTeamAdminEmail :: ShellEnv -> Email -> IO TeamId
getTeamAdminEmail env email = do
  accounts <- callBrig env (getTeamAdminEmail_ $ Just email)
  case [ accountUser acc | acc <- accounts, accountStatus acc == Active ] of
    [usr] -> maybe (error "getTeamAdminEmail: no email") pure (userTeam usr)
    bad -> error ("getTeamAdminEmail: " <> show bad)

registerTeamAdmin :: ShellEnv -> NewUser -> IO ()
registerTeamAdmin env = void . callBrig env . registerTeamAdmin_ . NewUserPublic

addUserToTeam :: ShellEnv -> UserId -> TeamId -> IO ()
addUserToTeam env uid tid = void . callGalley env $ addUserToTeam_ tid (simpleNewTeamMember uid)


createUserNoVerify_ :: NewUser -> SC.ClientM (Headers '[Header "Location" UserId] SelfProfile)
getTeamAdminEmail_  :: Maybe Email -> SC.ClientM [UserAccount]
registerTeamAdmin_  :: NewUserPublic -> SC.ClientM SelfProfile
addUserToTeam_      :: TeamId -> NewTeamMember -> SC.ClientM NoContent

API
  { _postUsers       = createUserNoVerify_
  , _getUsersByEmail = getTeamAdminEmail_
  , _postRegister    = registerTeamAdmin_
  , _addTeamMember   = addUserToTeam_
  }
  = SCG.genericClient


-- * copied from /libs/brig-types/src/Brig/Types/Servant/API/*

api :: Proxy (ToServantApi API)
api = genericApi (Proxy :: Proxy API)

data API route = API

  -- Internal.hs

  { _postUsers :: route :- "i" :> "users"
      :> ReqBody NewUser
      :> Post (Headers '[Servant.Header "Location" UserId] SelfProfile)
     -- handler: createUserNoVerify

  , _getUsersByEmail :: route :- "users"
      :> QueryParamStrict "email" Email
      :> Get [UserAccount]
     -- handler: listAccountsByIdentity


  -- Users.hs

  , _postRegister :: route :- "register"
      :> ReqBody NewUserPublic
      :> Post SelfProfile
     -- handler: createUser


  -- Teams_Internal.hs (or some more appropriately named module; yet to be written)

  , _addTeamMember :: route :- "i" :> "teams" :> Capture "tid" TeamId :> "members"
      :> ReqBody NewTeamMember
      :> Post NoContent
     -- handler: uncheckedAddTeamMember
  }
  deriving (Generic)


-- * should be copied somewhere:

simpleNewTeamMember :: UserId -> NewTeamMember
simpleNewTeamMember uid = newNewTeamMember $ newTeamMember uid (rolePermissions RoleMember) Nothing

simpleNewUser :: UserRecord -> NewUser
simpleNewUser (UserRecord name email) = NewUser
    { newUserName           = name
    , newUserUUID           = Nothing
    , newUserIdentity       = Just (EmailIdentity email)
    , newUserPict           = Nothing
    , newUserAssets         = []
    , newUserAccentId       = Nothing
    , newUserEmailCode      = Nothing
    , newUserPhoneCode      = Nothing
    , newUserOrigin         = Nothing
    , newUserLabel          = Nothing
    , newUserLocale         = Nothing
    , newUserPassword       = Nothing
    , newUserExpiresIn      = Nothing
    , newUserManagedBy      = Nothing
    }

instance ToHttpApiData (Id a) where
  toUrlPiece (Id uuid) = UUID.toText $ uuid

instance ToHttpApiData Email where
  toUrlPiece = fromEmail

instance FromHttpApiData (Id a) where
  parseUrlPiece txt = maybe (Left $ "Bad UUID: " <> cs (show txt)) (Right . Id) $ UUID.fromText txt

instance ToJSON NewUserPublic where
  toJSON (NewUserPublic newuser) = toJSON newuser


-- * copied from /libs/brig-types/src/Brig/Types/Servant/Orphans.hs

type Head = Verb 'HEAD 204 '[JSON]  -- TODO: which status code is this?
type Get = Verb 'GET 200 '[JSON]
type Post = Verb 'POST 201 '[JSON]
type Put204 = Verb 'PUT 204 '[JSON]
type Put200 = Verb 'PUT 200 '[JSON]
type Delete200 = Verb 'DELETE 200 '[JSON]
type Delete202 = Verb 'DELETE 202 '[JSON]

type ReqBody = Servant.ReqBody '[JSON]

type QueryParamStrict = Servant.QueryParam  -- TODO: which one?
type QueryParamOptional = Servant.QueryParam  -- TODO: which one?

type InternalZUser = Header "Z-User" UserId
type InternalZConn = Header "Z-Connection" ConnId

data AuthZUser
data AuthZConn


-- * stuff

error :: String -> IO a
error = throwIO . ErrorCall

unsafeError :: String -> a
unsafeError = unsafePerformIO . error
