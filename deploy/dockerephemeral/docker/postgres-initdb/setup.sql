CREATE DATABASE "backendB";
CREATE DATABASE "dyn-1";
CREATE DATABASE "dyn-2";
CREATE DATABASE "dyn-3";

GRANT ALL PRIVILEGES ON DATABASE "backendB" TO "wire-server";
GRANT ALL PRIVILEGES ON DATABASE "dyn-1" TO "wire-server";
GRANT ALL PRIVILEGES ON DATABASE "dyn-2" TO "wire-server";
GRANT ALL PRIVILEGES ON DATABASE "dyn-3" TO "wire-server";

CREATE EXTENSION IF NOT EXISTS pg_stat_statements;

\connect "backendB"
CREATE EXTENSION IF NOT EXISTS pg_stat_statements;

\connect "dyn-1"
CREATE EXTENSION IF NOT EXISTS pg_stat_statements;

\connect "dyn-2"
CREATE EXTENSION IF NOT EXISTS pg_stat_statements;

\connect "dyn-3"
CREATE EXTENSION IF NOT EXISTS pg_stat_statements;
