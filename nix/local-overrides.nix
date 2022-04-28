hsuper: hself: {
  stern = hself.callPackage ../tools/stern/default.nix { };
  rex = hself.callPackage ../tools/rex/default.nix { };
  api-simulations = hself.callPackage ../tools/api-simulations/default.nix { };
  repair-handles = hself.callPackage ../tools/db/repair-handles/default.nix { };
  move-team = hself.callPackage ../tools/db/move-team/default.nix { };
  find-undead = hself.callPackage ../tools/db/find-undead/default.nix { };
  assets = hself.callPackage ../tools/db/assets/default.nix { };
  service-backfill = hself.callPackage ../tools/db/service-backfill/default.nix { };
  migrate-sso-feature-flag = hself.callPackage ../tools/db/migrate-sso-feature-flag/default.nix { };
  billing-team-member-backfill = hself.callPackage ../tools/db/billing-team-member-backfill/default.nix { };
  auto-whitelist = hself.callPackage ../tools/db/auto-whitelist/default.nix { };
  proxy = hself.callPackage ../services/proxy/default.nix { };
  galley = hself.callPackage ../services/galley/default.nix { };
  federator = hself.callPackage ../services/federator/default.nix { };
  cannon = hself.callPackage ../services/cannon/default.nix { };
  cargohold = hself.callPackage ../services/cargohold/default.nix { };
  spar = hself.callPackage ../services/spar/default.nix { };
  brig = hself.callPackage ../services/brig/default.nix { };
  gundeck = hself.callPackage ../services/gundeck/default.nix { };
  zauth = hself.callPackage ../libs/zauth/default.nix { };
  deriving-swagger2 = hself.callPackage ../libs/deriving-swagger2/default.nix { };
  hscim = hself.callPackage ../libs/hscim/default.nix { };
  cassandra-util = hself.callPackage ../libs/cassandra-util/default.nix { };
  types-common-aws = hself.callPackage ../libs/types-common-aws/default.nix { };
  metrics-wai = hself.callPackage ../libs/metrics-wai/default.nix { };
  ropes = hself.callPackage ../libs/ropes/default.nix { };
  brig-types = hself.callPackage ../libs/brig-types/default.nix { };
  schema-profunctor = hself.callPackage ../libs/schema-profunctor/default.nix { };
  tasty-cannon = hself.callPackage ../libs/tasty-cannon/default.nix { };
  wire-api-federation = hself.callPackage ../libs/wire-api-federation/default.nix { };
  types-common-journal = hself.callPackage ../libs/types-common-journal/default.nix { };
  polysemy-wire-zoo = hself.callPackage ../libs/polysemy-wire-zoo/default.nix { };
  types-common = hself.callPackage ../libs/types-common/default.nix { };
  api-client = hself.callPackage ../libs/api-client/default.nix { };
  extended = hself.callPackage ../libs/extended/default.nix { };
  wire-message-proto-lens = hself.callPackage ../libs/wire-message-proto-lens/default.nix { };
  ssl-util = hself.callPackage ../libs/ssl-util/default.nix { };
  api-bot = hself.callPackage ../libs/api-bot/default.nix { };
  gundeck-types = hself.callPackage ../libs/gundeck-types/default.nix { };
  dns-util = hself.callPackage ../libs/dns-util/default.nix { };
  galley-types = hself.callPackage ../libs/galley-types/default.nix { };
  wire-api = hself.callPackage ../libs/wire-api/default.nix { };
  metrics-core = hself.callPackage ../libs/metrics-core/default.nix { };
  bilge = hself.callPackage ../libs/bilge/default.nix { };
  wai-utilities = hself.callPackage ../libs/wai-utilities/default.nix { };
  imports = hself.callPackage ../libs/imports/default.nix { };
  cargohold-types = hself.callPackage ../libs/cargohold-types/default.nix { };
  sodium-crypto-sign = hself.callPackage ../libs/sodium-crypto-sign/default.nix { };
}
