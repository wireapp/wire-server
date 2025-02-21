module Test.FeatureFlags.MlsE2EId where

import qualified API.Galley as Public
import qualified Data.Aeson as A
import SetupHelpers
import Test.FeatureFlags.Util
import Testlib.Prelude

mlsE2EId1 :: Value
mlsE2EId1 =
  object
    [ "status" .= "enabled",
      "config"
        .= object
          [ "crlProxy" .= "https://example.com",
            "verificationExpiration" .= A.Number 86400,
            "useProxyOnMobile" .= False
          ]
    ]

testMLSE2EId :: (HasCallStack) => APIAccess -> App ()
testMLSE2EId access = do
  invalid <-
    mlsE2EId1
      & if (access == InternalAPI)
        then -- the internal API is not as strict as the public one, so we need to tweak the invalid config some more
          setField "config.crlProxy" (object [])
        else removeField "config.crlProxy"
  mlsE2EId2 <-
    mlsE2EId1
      & setField "config.verificationExpiration" (A.Number 86401)
      & setField "config.useProxyOnMobile" True
  mkFeatureTests "mlsE2EId"
    & addUpdate mlsE2EId1
    & addUpdate mlsE2EId2
    & addInvalidUpdate invalid
    & runFeatureTests OwnDomain access

testPatchE2EId :: (HasCallStack) => App ()
testPatchE2EId = do
  checkPatch OwnDomain "mlsE2EId" (object ["lockStatus" .= "locked"])
  checkPatch OwnDomain "mlsE2EId" (object ["status" .= "enabled"])
  checkPatch OwnDomain "mlsE2EId"
    $ object ["lockStatus" .= "locked", "status" .= "enabled"]
  checkPatch OwnDomain "mlsE2EId"
    $ object
      [ "lockStatus" .= "unlocked",
        "config"
          .= object
            [ "crlProxy" .= "https://example.com",
              "verificationExpiration" .= A.Number 86401,
              "useProxyOnMobile" .= True
            ]
      ]

  checkPatch OwnDomain "mlsE2EId"
    $ object
      [ "config"
          .= object
            [ "crlProxy" .= "https://example.com",
              "verificationExpiration" .= A.Number 86401,
              "useProxyOnMobile" .= True
            ]
      ]

testMlsE2EConfigCrlProxyRequired :: (HasCallStack) => App ()
testMlsE2EConfigCrlProxyRequired = do
  (owner, tid, _) <- createTeam OwnDomain 1
  let configWithoutCrlProxy =
        object
          [ "config"
              .= object
                [ "useProxyOnMobile" .= False,
                  "verificationExpiration" .= A.Number 86400
                ],
            "status" .= "enabled"
          ]

  -- From API version 6 onwards, the CRL proxy is required, so the request should fail when it's not provided
  bindResponse (Public.setTeamFeatureConfig owner tid "mlsE2EId" configWithoutCrlProxy) $ \resp -> do
    resp.status `shouldMatchInt` 400
    resp.json %. "label" `shouldMatch` "mls-e2eid-missing-crl-proxy"

  configWithCrlProxy <-
    configWithoutCrlProxy
      & setField "config.useProxyOnMobile" True
      & setField "config.crlProxy" "https://crl-proxy.example.com"
      & setField "status" "enabled"

  -- The request should succeed when the CRL proxy is provided
  bindResponse (Public.setTeamFeatureConfig owner tid "mlsE2EId" configWithCrlProxy) $ \resp -> do
    resp.status `shouldMatchInt` 200

  -- Assert that the feature config got updated correctly
  expectedResponse <- configWithCrlProxy & setField "lockStatus" "unlocked" & setField "ttl" "unlimited"
  checkFeature "mlsE2EId" owner tid expectedResponse

testMlsE2EConfigCrlProxyNotRequiredInV5 :: (HasCallStack) => App ()
testMlsE2EConfigCrlProxyNotRequiredInV5 = do
  (owner, tid, _) <- createTeam OwnDomain 1
  let configWithoutCrlProxy =
        object
          [ "config"
              .= object
                [ "useProxyOnMobile" .= False,
                  "verificationExpiration" .= A.Number 86400
                ],
            "status" .= "enabled"
          ]

  -- In API version 5, the CRL proxy is not required, so the request should succeed
  bindResponse (Public.setTeamFeatureConfigVersioned (ExplicitVersion 5) owner tid "mlsE2EId" configWithoutCrlProxy) $ \resp -> do
    resp.status `shouldMatchInt` 200

  -- Assert that the feature config got updated correctly
  expectedResponse <-
    configWithoutCrlProxy
      & setField "lockStatus" "unlocked"
      & setField "ttl" "unlimited"
      & setField "config.crlProxy" "https://crlproxy.example.com"
  checkFeature "mlsE2EId" owner tid expectedResponse
