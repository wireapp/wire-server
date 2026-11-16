CREATE ROLE partman_maintainer WITH LOGIN PASSWORD 'local-partman-password';
CREATE SCHEMA partman AUTHORIZATION "wire-server";
CREATE EXTENSION IF NOT EXISTS pg_partman SCHEMA partman;

GRANT ALL ON SCHEMA partman TO partman_maintainer;
GRANT ALL ON ALL TABLES IN SCHEMA partman TO partman_maintainer;
GRANT ALL ON ALL SEQUENCES IN SCHEMA partman TO partman_maintainer;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA partman TO partman_maintainer;
GRANT EXECUTE ON ALL PROCEDURES IN SCHEMA partman TO partman_maintainer;
GRANT CREATE ON SCHEMA public TO partman_maintainer;
GRANT partman_maintainer TO "wire-server";

ALTER DEFAULT PRIVILEGES FOR ROLE partman_maintainer IN SCHEMA public
    GRANT SELECT, INSERT, UPDATE, DELETE ON TABLES TO "wire-server";

ALTER ROLE partman_maintainer SET search_path = partman, public;
