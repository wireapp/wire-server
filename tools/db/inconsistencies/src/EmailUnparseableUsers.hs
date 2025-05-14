{-# LANGUAGE RecordWildCards #-}

module EmailUnparseableUsers  where

import Cassandra
import Cassandra.Util
import Conduit
import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as C
import Data.Id
import Imports
import System.Logger (Logger)
import System.Logger qualified as Log
import UnliftIO (pooledMapConcurrentlyN)
import Wire.API.Team.Permission
import Wire.API.User (AccountStatus)

runCommand :: Logger -> ClientState -> FilePath -> IO ()
runCommand l brig inconsistenciesFile = do
  runResourceT $ undefined



pageSize :: Int32
pageSize = 10000

type UserDetailsRow = (UserId, Maybe Bool, Maybe AccountStatus, Maybe (Writetime AccountStatus), Maybe Text)

getUsers :: ConduitM () [UserDetailsRow] Client ()
getUsers = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () UserDetailsRow
    cql = "SELECT id, activated, status, writetime(status), team, writetime(team) from user"


data WithWritetime a = WithWritetime
  { value :: a,
    writetime :: Writetime a
  }
  deriving (Generic)

instance (ToJSON a) => ToJSON (WithWritetime a)


data UserDetails = UserDetails
  { id_ :: UserId,
    activated :: Maybe Bool,
    accountStatus :: Maybe (WithWritetime AccountStatus),
    email :: Maybe Text
  }
  deriving (Generic)

instance ToJSON UserDetails

mkUserDetails :: UserDetailsRow -> UserDetails
mkUserDetails (uid, activated, accountStatus, accountStateWrite, email) =
  UserDetails
    { id_ = uid,
      activated = activated,
      accountStatus = WithWritetime <$> accountStatus <*> accountStateWrite,
      email = email
    }
