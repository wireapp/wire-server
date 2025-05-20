module Test.FeatureFlags.ConferenceCalling where

import qualified API.GalleyInternal as Internal
import qualified Data.Aeson as A
import SetupHelpers (createTeam)
import Test.FeatureFlags.Util
import Testlib.Prelude

testPatchConferenceCalling :: (HasCallStack) => App ()
testPatchConferenceCalling = do
  checkPatchConferenceCalling OwnDomain
    $ object ["lockStatus" .= "locked"]
  checkPatchConferenceCalling OwnDomain
    $ object ["status" .= "disabled"]
  checkPatchConferenceCalling OwnDomain
    $ object ["lockStatus" .= "locked", "status" .= "disabled"]
  checkPatchConferenceCalling OwnDomain
    $ object
      [ "lockStatus" .= "unlocked",
        "config" .= object ["useSFTForOneToOneCalls" .= toJSON True]
      ]

testConferenceCalling :: (HasCallStack) => APIAccess -> App ()
testConferenceCalling access = do
  runFeatureTests OwnDomain access
    $ mkFeatureTests "conferenceCalling"
    & addUpdate (confCalling def {sft = toJSON True})
    & addUpdate (confCalling def {sft = toJSON False})
    & addInvalidUpdate (confCalling def {sft = toJSON (0 :: Int)})

checkPatchConferenceCalling ::
  (HasCallStack, MakesValue domain) =>
  domain ->
  Value ->
  App ()
checkPatchConferenceCalling domain patch = do
  let conferenceCalling = "conferenceCalling"
  (owner, tid, _) <- createTeam domain 0
  defFeature <- defAllFeatures %. conferenceCalling

  let valueOrDefault :: String -> App Value
      valueOrDefault key = do
        mValue <- lookupField patch key
        maybe (defFeature %. key) pure mValue

  checkFeature conferenceCalling owner tid defFeature
  void $ Internal.patchTeamFeature domain tid conferenceCalling patch >>= getJSON 200
  patched <- Internal.getTeamFeature domain tid conferenceCalling >>= getJSON 200
  checkFeature conferenceCalling owner tid patched
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
      statusFromPatch <- lookupField patch "status"
      -- conference calling behaves differently than other features
      -- if unlocked, the status is set to enabled even thought the default is disabled
      let expectedStatus = fromMaybe (A.String (fromString "enabled")) statusFromPatch
      patched %. "status" `shouldMatch` expectedStatus
      mPatchedConfig <- lookupField patched "config"
      case mPatchedConfig of
        Just patchedConfig -> patchedConfig `shouldMatch` valueOrDefault "config"
        Nothing -> do
          mDefConfig <- lookupField defFeature "config"
          assertBool "patch had an unexpected config field" (isNothing mDefConfig)

      -- if lock status is unlocked, it was either unlocked before or changed
      -- by the patch
      mPatchedLockStatus <- lookupField patch "lockStatus"
      case mPatchedLockStatus of
        Just ls -> ls `shouldMatch` "unlocked"
        Nothing -> defFeature %. "lockStatus" `shouldMatch` "unlocked"
