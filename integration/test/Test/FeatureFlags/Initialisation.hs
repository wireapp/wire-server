module Test.FeatureFlags.Initialisation where

import API.GalleyInternal
import Control.Monad.Codensity
import Control.Monad.Extra
import Control.Monad.Reader
import SetupHelpers
import Testlib.Prelude
import Testlib.ResourcePool

testMLSInitialisation :: (HasCallStack) => App ()
testMLSInitialisation = do
  let override =
        def
          { galleyCfg =
              setField
                "settings.featureFlags.mls"
                ( object
                    [ "initialConfig"
                        .= object
                          [ "protocolToggleUsers" .= ([] :: [Int]),
                            "defaultProtocol" .= "mls",
                            "allowedCipherSuites" .= [1, 2 :: Int],
                            "defaultCipherSuite" .= (1 :: Int),
                            "supportedProtocols" .= ["mls", "proteus"]
                          ]
                    ]
                )
                >=> removeField "settings.featureFlags.mlsMigration"
          }

  pool <- asks (.resourcePool)
  lowerCodensity do
    [resource] <- acquireResources 1 pool

    (alice, aliceTeam) <- lift $ lowerCodensity do
      -- start a dynamic backend with default configuration
      domain <- startDynamicBackend resource def

      -- create a team
      lift do
        (alice, tid, _) <- createTeam domain 0
        feat <- getTeamFeature alice tid "mls" >>= getJSON 200
        feat %. "config.defaultProtocol" `shouldMatch` "proteus"
        pure (alice, tid)

    lift $ lowerCodensity do
      -- now start the backend again, this time with an initial mls
      -- configuration set
      domain <- startDynamicBackend resource override

      -- a pre-existing team should get the default configuration
      lift do
        feat <- getTeamFeature alice aliceTeam "mls" >>= getJSON 200
        feat %. "config.defaultProtocol" `shouldMatch` "proteus"

      -- a new team should get the initial mls configuration
      lift do
        (bob, tid, _) <- createTeam domain 0
        feat <- getTeamFeature bob tid "mls" >>= getJSON 200
        feat %. "config.defaultProtocol" `shouldMatch` "mls"

        -- if the mls feature is locked, the config reverts back to default
        void
          $ patchTeamFeature bob tid "mls" (object ["lockStatus" .= "locked"])
          >>= getJSON 200
        feat' <- getTeamFeature bob tid "mls" >>= getJSON 200
        feat' %. "config.defaultProtocol" `shouldMatch` "proteus"
