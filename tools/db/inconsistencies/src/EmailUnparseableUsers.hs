{-# LANGUAGE RecordWildCards #-}

module EmailUnparseableUsers where

import Cassandra
import Cassandra.Util
import Conduit
import Data.Aeson (ToJSON, object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Conduit.Internal (zipSources)
import Data.Conduit.List qualified as C
import Data.Id (UserId)
import Data.Text.Encoding qualified as TE
import Imports
import System.Logger (Logger)
import System.Logger qualified as Log
import Text.Email.Validate qualified as Email
import UnliftIO (pooledMapConcurrentlyN)
import Wire.API.User (AccountStatus)

-- import Wire.API.User.EmailAddress (EmailAddress)

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
              pure $ mapMaybe toOffender userDetails
          )
        .| C.mapM (liftIO . pooledMapConcurrentlyN 48 checkOffender)
        .| C.map ((<> "\n") . BS.intercalate "\n" . map (BS.toStrict . Aeson.encode) . catMaybes)
        .| sinkFile inconsistenciesFile

-- | Parse and keep only rows whose email fails to validate.
toOffender :: UserDetailsRow -> Maybe Offender
toOffender row@(_, _, _, _, Just rawEmail) =
  case Email.validate (TE.encodeUtf8 rawEmail) of
    Left err -> Just (row, err) -- keep error message
    Right _ -> Nothing -- good email → ignore
toOffender _ = Nothing -- no email stored

-- | Transform an offending row into JSON
checkOffender :: Offender -> IO (Maybe Aeson.Value)
checkOffender ((uid, activated, accStat, wt, Just rawEmail), err) =
  pure . Just $
    object
      [ "userId" .= uid,
        "email" .= rawEmail,
        "activated" .= activated,
        "accountStatus" .= accStat,
        "statusWritetime" .= wt,
        "parseError" .= err
      ]
checkOffender _ = pure Nothing -- impossible

pageSize :: Int32
pageSize = 10000

type UserDetailsRow = (UserId, Maybe Bool, Maybe AccountStatus, Maybe (Writetime AccountStatus), Maybe Text)

type Offender =
  -- | row plus parse-error msg
  (UserDetailsRow, String)

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
