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
import Wire.API.User.EmailAddress

-- Problem statement:
-- Upon brig re-index, we look up the user table and create a
-- IndexUser which contains a field of type EmailAddress
-- libs/wire-subsystems/src/Wire/UserStore/IndexUser.hs
--
-- Parsing this fails for some cases, most likely during the conversion of the cassandra type to EmailAddress.
--
-- In August 2024 emails were refactored in https://github.com/wireapp/wire-server/pull/4206. Possibly existing emails in the database were not checked whether they conform to the new library.
--
-- email parsing from cql/bytestring is done in
-- libs/wire-api/src/Wire/API/User/EmailAddress.hs
--
--
runCommand :: Logger -> ClientState -> FilePath -> IO ()
runCommand l brig inconsistenciesFile = do
  runResourceT $
    runConduit $
      zipSources
        (C.sourceList [(1 :: Int32) ..])
        (transPipe (runClient brig) getUsers)
        .| C.mapM
          ( \(i, userDetails) -> do
              Log.info l (Log.field "userIds" (show ((i - 1) * pageSize + fromIntegral (length userDetails))))
              pure $ mapMaybe toDoSomething userDetails
          )
        .| C.mapM (liftIO . pooledMapConcurrentlyN 48 (checkUser brig))
        .| C.map ((<> "\n") . BS.intercalate "\n" . map (BS.toStrict . Aeson.encode) . catMaybes)
        .| sinkFile inconsistenciesFile


pageSize :: Int32
pageSize = 10000

type UserDetailsRow = (UserId, Maybe Bool, Maybe AccountStatus, Maybe (Writetime AccountStatus), Maybe Text)

getUsers :: ConduitM () [UserDetailsRow] Client ()
getUsers = paginateC cql (paramsP LocalQuorum () pageSize) x5
  where
    cql :: PrepQuery R () UserDetailsRow
    cql = "SELECT id, activated, status, writetime(status), email from user"

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
