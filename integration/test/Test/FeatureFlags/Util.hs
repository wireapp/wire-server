-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Test.FeatureFlags.Util where

import qualified API.Galley as Public
import qualified API.GalleyInternal as Internal
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as Text
import Notifications
import SetupHelpers
import Testlib.Prelude

data APIAccess = InternalAPI | PublicAPI
  deriving (Show, Eq)

instance TestCases APIAccess where
  mkTestCases =
    pure
      [ MkTestCase "[api=internal]" InternalAPI,
        MkTestCase "[api=public]" PublicAPI
      ]

newtype Feature = Feature String

instance TestCases Feature where
  mkTestCases = pure $ case defAllFeatures of
    Object obj -> do
      feat <- KM.keys obj
      let A.String nameT = toJSON feat
          name = Text.unpack nameT
      pure $ MkTestCase ("[feature=" <> name <> "]") (Feature name)
    _ -> []

setFeature :: APIAccess -> Value -> String -> String -> Value -> App Response
setFeature InternalAPI = Internal.setTeamFeatureConfig
setFeature PublicAPI = Public.setTeamFeatureConfig

disabled :: Value
disabled = object ["lockStatus" .= "unlocked", "status" .= "disabled", "ttl" .= "unlimited"]

disabledLocked :: Value
disabledLocked = object ["lockStatus" .= "locked", "status" .= "disabled", "ttl" .= "unlimited"]

enabled :: Value
enabled = object ["lockStatus" .= "unlocked", "status" .= "enabled", "ttl" .= "unlimited"]

defEnabledObj :: Value -> Value
defEnabledObj conf =
  object
    [ "lockStatus" .= "unlocked",
      "status" .= "enabled",
      "ttl" .= "unlimited",
      "config" .= conf
    ]

defAllFeatures :: Value
defAllFeatures =
  object
    [ "legalhold" .= disabled,
      "sso" .= disabled,
      "searchVisibility" .= disabled,
      "validateSAMLemails" .= enabled,
      "digitalSignatures" .= disabled,
      "appLock" .= defEnabledObj (object ["enforceAppLock" .= False, "inactivityTimeoutSecs" .= A.Number 60]),
      "fileSharing" .= enabled,
      "classifiedDomains" .= defEnabledObj (object ["domains" .= ["example.com"]]),
      "conferenceCalling" .= confCalling def {lockStatus = Just "locked"},
      "selfDeletingMessages"
        .= defEnabledObj (object ["enforcedTimeoutSeconds" .= A.Number 0]),
      "conversationGuestLinks" .= enabled,
      "sndFactorPasswordChallenge" .= disabledLocked,
      "mls"
        .= object
          [ "lockStatus" .= "unlocked",
            "status" .= "disabled",
            "ttl" .= "unlimited",
            "config"
              .= object
                [ "protocolToggleUsers" .= ([] :: [String]),
                  "defaultProtocol" .= "proteus",
                  "supportedProtocols" .= ["proteus", "mls"],
                  "allowedCipherSuites" .= ([2] :: [Int]),
                  "defaultCipherSuite" .= A.Number 2
                ]
          ],
      "searchVisibilityInbound" .= disabled,
      "exposeInvitationURLsToTeamAdmin" .= disabledLocked,
      "outlookCalIntegration" .= disabledLocked,
      "mlsE2EId"
        .= object
          [ "lockStatus" .= "unlocked",
            "status" .= "disabled",
            "ttl" .= "unlimited",
            "config"
              .= object
                [ "verificationExpiration" .= A.Number 86400,
                  "useProxyOnMobile" .= False,
                  "crlProxy" .= "https://crlproxy.example.com"
                ]
          ],
      "mlsMigration"
        .= object
          [ "lockStatus" .= "locked",
            "status" .= "enabled",
            "ttl" .= "unlimited",
            "config"
              .= object
                [ "startTime" .= "2029-05-16T10:11:12.123Z",
                  "finaliseRegardlessAfter" .= "2029-10-17T00:00:00Z"
                ]
          ],
      "enforceFileDownloadLocation"
        .= object
          [ "lockStatus" .= "unlocked",
            "status" .= "disabled",
            "ttl" .= "unlimited",
            "config"
              .= object
                [ "enforcedDownloadLocation" .= "downloads"
                ]
          ],
      "limitedEventFanout" .= disabled,
      "domainRegistration" .= disabledLocked,
      "channels"
        .= object
          [ "lockStatus" .= "locked",
            "status" .= "disabled",
            "ttl" .= "unlimited",
            "config"
              .= object
                [ "allowed_to_create_channels" .= "team-members",
                  "allowed_to_open_channels" .= "team-members"
                ]
          ],
      "cells" .= enabled
    ]

hasExplicitLockStatus :: String -> Bool
hasExplicitLockStatus "fileSharing" = True
hasExplicitLockStatus "conferenceCalling" = True
hasExplicitLockStatus "selfDeletingMessages" = True
hasExplicitLockStatus "guestLinks" = True
hasExplicitLockStatus "sndFactorPasswordChallenge" = True
hasExplicitLockStatus "outlookCalIntegration" = True
hasExplicitLockStatus "enforceFileDownloadLocation" = True
hasExplicitLockStatus "domainRegistration" = True
hasExplicitLockStatus _ = False

checkFeature :: (HasCallStack, MakesValue user, MakesValue tid) => String -> user -> tid -> Value -> App ()
checkFeature feature user tid expected = do
  tidStr <- asString tid
  domain <- objDomain user
  bindResponse (Internal.getTeamFeature domain tidStr feature) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatch` expected
  bindResponse (Public.getTeamFeatures user tid) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. feature `shouldMatch` expected
  bindResponse (Public.getTeamFeature user tid feature) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatch` expected
  bindResponse (Public.getFeatureConfigs user) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. feature `shouldMatch` expected

assertForbidden :: (HasCallStack) => Response -> App ()
assertForbidden = assertLabel 403 "no-team-member"

data ConfCalling = ConfCalling
  { lockStatus :: Maybe String,
    status :: String,
    sft :: Value
  }

instance Default ConfCalling where
  def =
    ConfCalling
      { lockStatus = Nothing,
        status = "disabled",
        sft = toJSON False
      }

confCalling :: ConfCalling -> Value
confCalling args =
  object
    $ ["lockStatus" .= s | s <- toList args.lockStatus]
    <> ["ttl" .= "unlimited"]
    <> [ "status" .= args.status,
         "config"
           .= object ["useSFTForOneToOneCalls" .= args.sft]
       ]

setFlag :: (HasCallStack) => APIAccess -> WebSocket -> String -> String -> Value -> App ()
setFlag access ws tid featureName value = do
  update <- removeField "ttl" value
  void
    $ setFeature access ws.user tid featureName update
    >>= getJSON 200
  expected <-
    setField "ttl" "unlimited"
      =<< setField "lockStatus" "unlocked" value

  -- should receive an event
  do
    notif <- awaitMatch isFeatureConfigUpdateNotif ws
    notif %. "payload.0.name" `shouldMatch` featureName
    notif %. "payload.0.data" `shouldMatch` expected

  checkFeature featureName ws.user tid expected

checkPatchWithComputeExpected ::
  (HasCallStack, MakesValue domain) =>
  domain ->
  String ->
  Value ->
  (String -> Value -> Value -> App Value) ->
  App ()
checkPatchWithComputeExpected domain featureName patch computeExpectedValue = do
  (owner, tid, _) <- createTeam domain 0
  defFeature <- defAllFeatures %. featureName

  checkFeature featureName owner tid defFeature
  void
    $ Internal.patchTeamFeature domain tid featureName patch
    >>= getJSON 200
  patched <- Internal.getTeamFeature domain tid featureName >>= getJSON 200
  checkFeature featureName owner tid patched
  lockStatus <- patched %. "lockStatus" >>= asString
  if lockStatus == "locked"
    then do
      -- if lock status is locked the feature status should fall back to the default
      patched `shouldMatch` (defFeature & setField "lockStatus" "locked")
      -- if lock status is locked, it was either locked before or changed by the patch
      mPatchedLockStatus <- lookupField patch "lockStatus"
      case mPatchedLockStatus of
        Just ls -> ls `shouldMatch` "locked"
        Nothing -> defFeature %. "lockStatus" `shouldMatch` "locked"
    else do
      patched %. "status" `shouldMatch` computeExpectedValue "status" defFeature patch
      mPatchedConfig <- lookupField patched "config"
      case mPatchedConfig of
        Just patchedConfig -> patchedConfig `shouldMatch` computeExpectedValue "config" defFeature patch
        Nothing -> do
          mDefConfig <- lookupField defFeature "config"
          assertBool "patch had an unexpected config field" (isNothing mDefConfig)

      when (hasExplicitLockStatus featureName) $ do
        -- if lock status is unlocked, it was either unlocked before or changed
        -- by the patch
        mPatchedLockStatus <- lookupField patch "lockStatus"
        case mPatchedLockStatus of
          Just ls -> ls `shouldMatch` "unlocked"
          Nothing -> defFeature %. "lockStatus" `shouldMatch` "unlocked"

checkPatch ::
  (HasCallStack, MakesValue domain) =>
  domain ->
  String ->
  Value ->
  App ()
checkPatch domain featureName patch = checkPatchWithComputeExpected domain featureName patch computeExpectedValue
  where
    computeExpectedValue key defFeature p = do
      mValue <- lookupField p key
      maybe (defFeature %. key) pure mValue

data FeatureTests = FeatureTests
  { name :: String,
    -- | valid config values used to update the feature setting (should not
    -- include the lock status and ttl, as these are not part of the request
    -- payload)
    updates :: [Value],
    invalidUpdates :: [Value],
    owner :: Maybe Value
  }

mkFeatureTests :: String -> FeatureTests
mkFeatureTests name = FeatureTests name [] [] Nothing

addUpdate :: Value -> FeatureTests -> FeatureTests
addUpdate up ft = ft {updates = ft.updates <> [up]}

addInvalidUpdate :: Value -> FeatureTests -> FeatureTests
addInvalidUpdate up ft = ft {invalidUpdates = ft.invalidUpdates <> [up]}

setOwner :: (MakesValue user) => user -> FeatureTests -> App FeatureTests
setOwner owner ft = do
  x <- make owner
  pure ft {owner = Just x}

runFeatureTests ::
  (HasCallStack, MakesValue domain) =>
  domain ->
  APIAccess ->
  FeatureTests ->
  App ()
runFeatureTests domain access ft = do
  defFeature <- defAllFeatures %. ft.name
  -- personal user
  do
    user <- randomUser domain def
    bindResponse (Public.getFeatureConfigs user) $ \resp -> do
      resp.status `shouldMatchInt` 200
      feat <- resp.json %. ft.name
      lockStatus <- feat %. "lockStatus"
      expected <- setField "lockStatus" lockStatus defFeature
      feat `shouldMatch` expected

  -- make team
  (owner, tid) <- case ft.owner of
    Nothing -> do
      (owner, tid, _) <- createTeam domain 0
      pure (owner, tid)
    Just owner -> do
      tid <- owner %. "team" & asString
      pure (owner, tid)
  checkFeature ft.name owner tid defFeature

  -- lock the feature
  Internal.setTeamFeatureLockStatus owner tid ft.name "locked"
  bindResponse (Public.getTeamFeature owner tid ft.name) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json %. "lockStatus" `shouldMatch` "locked"
    expected <- setField "lockStatus" "locked" defFeature
    checkFeature ft.name owner tid expected

  for_ ft.updates $ (setFeature access owner tid ft.name >=> getJSON 409)

  -- unlock the feature
  Internal.setTeamFeatureLockStatus owner tid ft.name "unlocked"
  void $ withWebSocket owner $ \ws -> do
    for_ ft.updates $ \update -> do
      setFlag access ws tid ft.name update

    for_ ft.invalidUpdates $ \update -> do
      void $ setFeature access owner tid ft.name update >>= getJSON 400
      assertNoEvent 2 ws

  -- lock again, should be set to default value
  Internal.setTeamFeatureLockStatus owner tid ft.name "locked"
  do
    expected <- setField "lockStatus" "locked" defFeature
    checkFeature ft.name owner tid expected

  -- unlock again, should be set to the last update
  Internal.setTeamFeatureLockStatus owner tid ft.name "unlocked"
  for_ (take 1 (reverse ft.updates)) $ \update -> do
    expected <-
      setField "ttl" "unlimited"
        =<< setField "lockStatus" "unlocked" update
    checkFeature ft.name owner tid expected
