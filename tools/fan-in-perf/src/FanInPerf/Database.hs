-- | The experiment database and the connection to the maintenance database
-- that creates and drops it.
module FanInPerf.Database
  ( experimentDb,
    adminConnStr,
  )
where

import FanInPerf.NotificationsAdmin (DbName, mkDbName)
import Imports
import PostgresqlConnectionString (ConnectionString)
import PostgresqlConnectionString qualified

-- | The database named in the connection string, if present and a valid
-- identifier.
experimentDb :: ConnectionString -> Either Text DbName
experimentDb c = case PostgresqlConnectionString.toDbname c of
  Nothing -> Left "--db has no database name"
  Just n -> mkDbName n

-- | Same server and credentials, but the @postgres@ maintenance database
-- (@<>@ is right-biased).
adminConnStr :: ConnectionString -> ConnectionString
adminConnStr c = c <> PostgresqlConnectionString.dbname "postgres"
