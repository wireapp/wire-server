module Brig.Index.Migrations.V1_ReIndexForSearchTeamMembers (migration) where

import qualified Brig.Index.Migrations.Types as Types
import qualified Brig.User.Search.Index as Search

migration :: Types.Migration
migration = Types.Migration ver txt mig
  where
    ver = Types.MigrationVersion 1
    txt = "Reindex all users for search within team"
    mig = Search.reindexAllIfSameOrNewer
