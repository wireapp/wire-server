module Wire.UserSearch.Migration where

import Data.Aeson
import Database.Bloodhound.Types qualified as ES
import Imports
import Numeric.Natural
import System.Logger.Class (ToBytes (..))

newtype MigrationVersion = MigrationVersion {migrationVersion :: Natural}
  deriving (Show, Eq, Ord)

instance ToJSON MigrationVersion where
  toJSON (MigrationVersion v) = object ["migration_version" .= v]

instance FromJSON MigrationVersion where
  parseJSON = withObject "MigrationVersion" $ \o -> MigrationVersion <$> o .: "migration_version"

instance ToBytes MigrationVersion where
  bytes = bytes . toInteger . migrationVersion

data MigrationException
  = CreateMigrationIndexFailed String
  | FetchMigrationVersionsFailed String
  | PersistVersionFailed MigrationVersion String
  | PutMappingFailed String
  | TargetIndexAbsent
  | VersionSourceMissing (ES.SearchResult MigrationVersion)
  deriving (Show)

instance Exception MigrationException
