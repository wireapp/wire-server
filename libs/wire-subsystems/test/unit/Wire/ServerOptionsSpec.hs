{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Wire.ServerOptionsSpec (spec) where

import Cassandra.Options
import Data.Bifunctor
import Data.Code qualified as Code
import Data.Domain
import Data.Nonce
import Data.RetryAfter
import Data.Set
import Data.Yaml
import Database.Bloodhound.Types qualified as ES
import Imports
import Network.AMQP.Extended
import System.Logger
import Test.Hspec
import Text.Email.Parser
import Util.Options
import Wire.API.Locale
import Wire.API.Routes.FederationDomainConfig
import Wire.API.Team.Feature
import Wire.API.User
import Wire.API.User.Search
import Wire.ServerOptions.Brig qualified as B
import Wire.ServerOptions.Brig.CookieLimit qualified as CookieLimit
import Wire.ServerOptions.Brig.Queue qualified as Queue
import Wire.ServerOptions.Brig.ZAuth qualified as ZAuth

spec :: Spec
spec = do
  describe "Wire.ServerOptions.CookieLimit" $ do
    it "CookieThrottle" $ do
      let i =
            "stdDev: 5\n\
            \retryAfter: 3"
          o = CookieLimit.StdDevThrottle 5 3
      (prettyPrintParseException `first` decodeEither' i) `shouldBe` Right o

  describe "Wire.ServerOptions.Brig.Queue" $ do
    it "QueueOpts (sqs)" $ do
      let i =
            "queueType: sqs\n\
            \queueName: test"
          o = Queue.SqsQueueOpts "test"
      (prettyPrintParseException `first` decodeEither' i) `shouldBe` Right o

    it "QueueOpts (stomp)" $ do
      let i =
            "queueType: stomp\n\
            \queueName: test"
          o = Queue.StompQueueOpts "test"
      (prettyPrintParseException `first` decodeEither' i) `shouldBe` Right o

  describe "Wire.ServerOptions.Brig.ZAuth" $ do
    it "Settings" $ do
      let i =
            "keyIndex: 1\n\
            \userTokenTimeout: 120\n\
            \sessionTokenTimeout: 20\n\
            \accessTokenTimeout: 30\n\
            \providerTokenTimeout: 60\n\
            \legalHoldUserTokenTimeout: 120\n\
            \legalHoldAccessTokenTimeout: 30"
          o = ZAuth.Settings 1 120 20 30 60 120 30
      (prettyPrintParseException `first` decodeEither' i) `shouldBe` Right o

  describe "Wire.ServerOptions.Brig" $ do
    it "Settings" $ do
      let i =
            "brig:\n\
            \  host: 0.0.0.0\n\
            \  port: 8082\n\
            \cassandra:\n\
            \  endpoint:\n\
            \    host: 127.0.0.1\n\
            \    port: 9042\n\
            \  keyspace: brig_test\n\
            \elasticsearch:\n\
            \  url: https://localhost:9200\n\
            \  index: directory_test\n\
            \  credentials: test/resources/elasticsearch-credentials.yaml\n\
            \  caCert: test/resources/elasticsearch-ca.pem\n\
            \  insecureSkipVerifyTls: false\n\
            \  additionalCredentials: test/resources/elasticsearch-credentials.yaml\n\
            \  additionalCaCert: test/resources/elasticsearch-ca.pem\n\
            \  additionalInsecureSkipVerifyTls: false\n\
            \rabbitmq:\n\
            \  host: 127.0.0.1\n\
            \  port: 5671\n\
            \  vHost: /\n\
            \  enableTls: true\n\
            \  caCert: test/resources/rabbitmq-ca.pem\n\
            \  insecureSkipVerifyTls: false\n\
            \cargohold:\n\
            \  host: 127.0.0.1\n\
            \  port: 8084\n\
            \galley:\n\
            \  host: 127.0.0.1\n\
            \  port: 8085\n\
            \gundeck:\n\
            \  host: 127.0.0.1\n\
            \  port: 8086\n\
            \federatorInternal:\n\
            \  host: 127.0.0.1\n\
            \  port: 8097\n\
            \multiSFT: false\n\
            \aws:\n\
            \  userJournalQueue: integration-user-events.fifo\n\
            \  prekeyTable: integration-brig-prekeys\n\
            \  sqsEndpoint: http://localhost:4568\n\
            \randomPrekeys: true\n\
            \internalEvents:\n\
            \  queueType: sqs\n\
            \  queueName: integration-brig-events-internal\n\
            \emailSMS:\n\
            \  email:\n\
            \    sesQueue: integration-brig-events\n\
            \    sesEndpoint: http://localhost:4569\n\
            \  general:\n\
            \    templateDir: deb/opt/brig/templates\n\
            \    emailSender: backend-integration@wire.com\n\
            \    smsSender: \"+123456789\"\n\
            \    templateBranding:\n\
            \      brand: Wire\n\
            \      brandUrl: https://wire.com\n\
            \      brandLabelUrl: wire.com\n\
            \      brandLogoUrl: https://wire.com/p/img/email/logo-email-black.png\n\
            \      brandService: Wire Service Provider\n\
            \      copyright: WIRE SWISS GmbH\n\
            \      misuse: misuse@wire.com\n\
            \      legal: https://wire.com/legal/\n\
            \      forgot: https://wire.com/forgot/\n\
            \      support: https://support.wire.com/\n\
            \  user:\n\
            \    activationUrl: http://127.0.0.1:8080/activate?key=${key}&code=${code}\n\
            \    smsActivationUrl: http://127.0.0.1:8080/v/${code}\n\
            \    passwordResetUrl: http://127.0.0.1:8080/password-reset/${key}?code=${code}\n\
            \    invitationUrl: http://127.0.0.1:8080/register?invitation_code=${code}\n\
            \    deletionUrl: http://127.0.0.1:8080/users/delete?key=${key}&code=${code}\n\
            \  provider:\n\
            \    homeUrl: https://provider.localhost/\n\
            \    providerActivationUrl: http://127.0.0.1:8080/provider/activate?key=${key}&code=${code}\n\
            \    approvalUrl: http://127.0.0.1:8080/provider/approve?key=${key}&code=${code}\n\
            \    approvalTo: success@simulator.amazonses.com\n\
            \    providerPwResetUrl: http://127.0.0.1:8080/provider/password-reset?key=${key}&code=${code}\n\
            \  team:\n\
            \    tInvitationUrl: http://127.0.0.1:8080/register?team=${team}&team_code=${code}\n\
            \    tExistingUserInvitationUrl: http://127.0.0.1:8080/accept-invitation?team-code=${code}\n\
            \    tActivationUrl: http://127.0.0.1:8080/register?team=${team}&team_code=${code}\n\
            \    tCreatorWelcomeUrl: http://127.0.0.1:8080/creator-welcome-website\n\
            \    tMemberWelcomeUrl: http://127.0.0.1:8080/member-welcome-website\n\
            \zauth:\n\
            \  privateKeys: test/resources/zauth/privkeys.txt\n\
            \  publicKeys: test/resources/zauth/pubkeys.txt\n\
            \  authSettings:\n\
            \    keyIndex: 1\n\
            \    userTokenTimeout: 120\n\
            \    sessionTokenTimeout: 20\n\
            \    accessTokenTimeout: 30\n\
            \    providerTokenTimeout: 60\n\
            \    legalHoldUserTokenTimeout: 120\n\
            \    legalHoldAccessTokenTimeout: 30\n\
            \turn:\n\
            \  serversSource: files\n\
            \  servers: test/resources/turn/servers.txt\n\
            \  serversV2: test/resources/turn/servers-v2.txt\n\
            \  secret: test/resources/turn/secret.txt\n\
            \  configTTL: 3600\n\
            \  tokenTTL: 21600\n\
            \optSettings:\n\
            \  setActivationTimeout: 4\n\
            \  setVerificationTimeout: 4\n\
            \  setTeamInvitationTimeout: 4\n\
            \  setExpiredUserCleanupTimeout: 1\n\
            \  setUserMaxConnections: 16\n\
            \  setCookieInsecure: true\n\
            \  setUserCookieRenewAge: 2\n\
            \  setUserCookieLimit: 5\n\
            \  setUserCookieThrottle:\n\
            \    stdDev: 5\n\
            \    retryAfter: 3\n\
            \  setLimitFailedLogins:\n\
            \    timeout: 5\n\
            \    retryLimit: 5\n\
            \  setSuspendInactiveUsers:\n\
            \    suspendTimeout: 10\n\
            \  setRichInfoLimit: 5000\n\
            \  setDefaultUserLocale: en\n\
            \  setMaxTeamSize: 32\n\
            \  setMaxConvSize: 16\n\
            \  setEmailVisibility: visible_to_self\n\
            \  setPropertyMaxKeyLen: 1024\n\
            \  setPropertyMaxValueLen: 4096\n\
            \  setDeleteThrottleMillis: 0\n\
            \  setSqsThrottleMillis: 1000\n\
            \  setRestrictUserCreation: false\n\
            \  setFederationDomain: example.com\n\
            \  setFeatureFlags:\n\
            \    conferenceCalling:\n\
            \      defaultForNew:\n\
            \        status: disabled\n\
            \      defaultForNull:\n\
            \        status: disabled\n\
            \  setFederationDomainConfigsUpdateFreq: 1\n\
            \  setFederationStrategy: allowAll\n\
            \  setFederationDomainConfigs:\n\
            \    - domain: example.com\n\
            \      search_policy: full_search\n\
            \  setDisabledAPIVersions: []\n\
            \  set2FACodeGenerationDelaySecs: 5\n\
            \  setNonceTtlSecs: 5\n\
            \  setDpopMaxSkewSecs: 1\n\
            \  setDpopTokenExpirationTimeSecs: 300\n\
            \  setPublicKeyBundle: test/resources/jwt/ecdsa_secp256r1_sha256_key.pem\n\
            \  setEnableMLS: true\n\
            \  setOAuthJwkKeyPair: test/resources/oauth/ed25519.jwk\n\
            \  setOAuthAuthCodeExpirationTimeSecs: 3\n\
            \  setOAuthAccessTokenExpirationTimeSecs: 3\n\
            \  setOAuthEnabled: true\n\
            \  setOAuthRefreshTokenExpirationTimeSecs: 14515200\n\
            \  setOAuthMaxActiveRefreshTokens: 10\n\
            \  setPasswordHashingOptions:\n\
            \    algorithm: argon2id\n\
            \    iterations: 1\n\
            \    memory: 128\n\
            \    parallelism: 1\n\
            \logLevel: Warn\n\
            \logNetStrings: false\n"

          o =
            B.Opts
              { brig = Endpoint {host = "0.0.0.0", port = 8082},
                cargohold = Endpoint {host = "127.0.0.1", port = 8084},
                galley = Endpoint {host = "127.0.0.1", port = 8085},
                gundeck = Endpoint {host = "127.0.0.1", port = 8086},
                federatorInternal = Just (Endpoint {host = "127.0.0.1", port = 8097}),
                cassandra =
                  CassandraOpts
                    { endpoint = Endpoint {host = "127.0.0.1", port = 9042},
                      keyspace = "brig_test",
                      filterNodesByDatacentre = Nothing,
                      tlsCa = Nothing
                    },
                elasticsearch =
                  B.ElasticSearchOpts
                    { url = ES.Server "https://localhost:9200",
                      index = ES.IndexName "directory_test",
                      additionalWriteIndex = Nothing,
                      additionalWriteIndexUrl = Nothing,
                      credentials = Just (FilePathSecrets "test/resources/elasticsearch-credentials.yaml"),
                      additionalCredentials = Just (FilePathSecrets "test/resources/elasticsearch-credentials.yaml"),
                      insecureSkipVerifyTls = False,
                      caCert = Just "test/resources/elasticsearch-ca.pem",
                      additionalInsecureSkipVerifyTls = False,
                      additionalCaCert = Just "test/resources/elasticsearch-ca.pem"
                    },
                multiSFT = Just False,
                rabbitmq =
                  Just
                    ( AmqpEndpoint
                        { host = "127.0.0.1",
                          port = 5671,
                          vHost = "/",
                          tls = Just (RabbitMqTlsOpts {caCert = Just "test/resources/rabbitmq-ca.pem", insecureSkipVerifyTls = False})
                        }
                    ),
                aws =
                  B.AWSOpts
                    { userJournalQueue = Just "integration-user-events.fifo",
                      prekeyTable = "integration-brig-prekeys",
                      sqsEndpoint = AWSEndpoint {_awsHost = "localhost", _awsSecure = False, _awsPort = 4568},
                      dynamoDBEndpoint = Nothing
                    },
                randomPrekeys = Just True,
                stompOptions = Nothing,
                emailSMS =
                  B.EmailSMSOpts
                    { email =
                        B.EmailAWS
                          ( B.EmailAWSOpts
                              { sesQueue = "integration-brig-events",
                                sesEndpoint = AWSEndpoint {_awsHost = "localhost", _awsSecure = False, _awsPort = 4569}
                              }
                          ),
                      general =
                        B.EmailSMSGeneralOpts
                          { templateDir = "deb/opt/brig/templates",
                            emailSender = unsafeEmailAddress "backend-integration" "wire.com",
                            smsSender = "+123456789",
                            templateBranding =
                              B.BrandingOpts
                                { brand = "Wire",
                                  brandUrl = "https://wire.com",
                                  brandLabelUrl = "wire.com",
                                  brandLogoUrl = "https://wire.com/p/img/email/logo-email-black.png",
                                  brandService = "Wire Service Provider",
                                  copyright = "WIRE SWISS GmbH",
                                  misuse = "misuse@wire.com",
                                  legal = "https://wire.com/legal/",
                                  forgot = "https://wire.com/forgot/",
                                  support = "https://support.wire.com/"
                                }
                          },
                      user =
                        B.EmailUserOpts
                          { activationUrl = "http://127.0.0.1:8080/activate?key=${key}&code=${code}",
                            smsActivationUrl = "http://127.0.0.1:8080/v/${code}",
                            passwordResetUrl = "http://127.0.0.1:8080/password-reset/${key}?code=${code}",
                            deletionUrl = "http://127.0.0.1:8080/users/delete?key=${key}&code=${code}"
                          },
                      provider =
                        B.ProviderOpts
                          { homeUrl = "https://provider.localhost/",
                            providerActivationUrl = "http://127.0.0.1:8080/provider/activate?key=${key}&code=${code}",
                            approvalUrl = "http://127.0.0.1:8080/provider/approve?key=${key}&code=${code}",
                            approvalTo = unsafeEmailAddress "success" "simulator.amazonses.com",
                            providerPwResetUrl = "http://127.0.0.1:8080/provider/password-reset?key=${key}&code=${code}"
                          },
                      team =
                        B.TeamOpts
                          { tInvitationUrl = "http://127.0.0.1:8080/register?team=${team}&team_code=${code}",
                            tExistingUserInvitationUrl = "http://127.0.0.1:8080/accept-invitation?team-code=${code}",
                            tActivationUrl = "http://127.0.0.1:8080/register?team=${team}&team_code=${code}",
                            tCreatorWelcomeUrl = "http://127.0.0.1:8080/creator-welcome-website",
                            tMemberWelcomeUrl = "http://127.0.0.1:8080/member-welcome-website"
                          }
                    },
                zauth =
                  B.ZAuthOpts
                    { privateKeys = "test/resources/zauth/privkeys.txt",
                      publicKeys = "test/resources/zauth/pubkeys.txt",
                      authSettings = ZAuth.Settings 1 120 20 30 60 120 30
                    },
                discoUrl = Nothing,
                internalEvents = B.InternalEventsOpts {internalEventsQueue = Queue.SqsQueueOpts "integration-brig-events-internal"},
                logLevel = Warn,
                logNetStrings = Just (Last {getLast = False}),
                logFormat = Nothing,
                turn =
                  B.TurnOpts
                    { serversSource =
                        B.TurnSourceFiles
                          ( B.TurnServersFiles
                              { tsfServers = "test/resources/turn/servers.txt",
                                tsfServersV2 = "test/resources/turn/servers-v2.txt"
                              }
                          ),
                      secret = "test/resources/turn/secret.txt",
                      tokenTTL = 21600,
                      configTTL = 3600
                    },
                sft = Nothing,
                settings =
                  B.Settings
                    { activationTimeout = 4,
                      verificationCodeTimeoutInternal = Just (Code.Timeout {Code.timeoutDiffTime = 4}),
                      teamInvitationTimeout = 4,
                      expiredUserCleanupTimeout = Just 1,
                      stomp = Nothing,
                      allowlistEmailDomains = Nothing,
                      userMaxConnections = 16,
                      userMaxPermClients = Nothing,
                      cookieInsecure = True,
                      userCookieRenewAge = 2,
                      userCookieLimit = 5,
                      userCookieThrottle = CookieLimit.StdDevThrottle (CookieLimit.StdDev 5.0) (RetryAfter {retryAfterSeconds = 3}),
                      limitFailedLogins = Just (B.LimitFailedLogins {timeout = 5, retryLimit = 5}),
                      suspendInactiveUsers = Just (B.SuspendInactiveUsers {suspendTimeout = 10}),
                      richInfoLimit = 5000,
                      defaultTemplateLocaleInternal = Nothing,
                      defaultUserLocaleInternal = (Just . fromJust) (parseLocale "en"),
                      maxTeamSize = 32,
                      maxConvSize = 16,
                      providerSearchFilter = Nothing,
                      emailVisibility = EmailVisibleToSelf,
                      propertyMaxKeyLen = Just 1024,
                      propertyMaxValueLen = Just 4096,
                      deleteThrottleMillis = Just 0,
                      searchSameTeamOnly = Nothing,
                      federationDomain = Domain {_domainText = "example.com"},
                      federationStrategy = Just AllowAll,
                      federationDomainConfigs =
                        Just
                          [ B.ImplicitNoFederationRestriction
                              { federationDomainConfig =
                                  FederationDomainConfig
                                    { domain = Domain {_domainText = "example.com"},
                                      searchPolicy = FullSearch,
                                      restriction = FederationRestrictionAllowAll
                                    }
                              }
                          ],
                      federationDomainConfigsUpdateFreq = Just 1,
                      sqsThrottleMillis = Just 1000,
                      restrictUserCreation = Just False,
                      featureFlags =
                        Just
                          ( B.UserFeatureFlags
                              { conferenceCalling =
                                  B.ConferenceCallingUserStatus
                                    { forNew = Just FeatureStatusDisabled,
                                      forNull = FeatureStatusDisabled
                                    }
                              }
                          ),
                      customerExtensions = Nothing,
                      sftStaticUrl = Nothing,
                      sftListAllServers = Nothing,
                      enableMLS = Just True,
                      keyPackageMaximumLifetime = Nothing,
                      disabledAPIVersions = fromList [],
                      twoFACodeGenerationDelaySecsInternal = Just 5,
                      nonceTtlSecsInternal = Just (NonceTtlSecs {unNonceTtlSecs = 5}),
                      dpopMaxSkewSecsInternal = Just 1,
                      dpopTokenExpirationTimeSecsInternal = Just 300,
                      publicKeyBundle = Just "test/resources/jwt/ecdsa_secp256r1_sha256_key.pem",
                      oAuthJwkKeyPair = Just "test/resources/oauth/ed25519.jwk",
                      oAuthAccessTokenExpirationTimeSecsInternal = Just 3,
                      oAuthAuthorizationCodeExpirationTimeSecsInternal = Nothing,
                      oAuthEnabledInternal = Just True,
                      oAuthRefreshTokenExpirationTimeSecsInternal = Just 14515200,
                      oAuthMaxActiveRefreshTokensInternal = Just 10,
                      passwordHashingOptions = PasswordHashingArgon2id (Argon2idOptions {iterations = 1, memory = 128, parallelism = 1})
                    }
              }

      (prettyPrintParseException `first` decodeEither' i) `shouldBe` Right o
