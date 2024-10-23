module Test.Property where

import API.Brig
import API.Common
import qualified Data.Map as Map
import SetupHelpers
import Testlib.Prelude

testSetGetDeleteProperty :: App ()
testSetGetDeleteProperty = do
  user <- randomUser OwnDomain def
  setProperty user "foo" "bar" `bindResponse` \resp ->
    resp.status `shouldMatchInt` 200

  getProperty user "foo" `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatch` toJSON "bar"

  deleteProperty user "foo" `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200

  getProperty user "foo" `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 404

testGetProperties :: App ()
testGetProperties = do
  user <- randomUser OwnDomain def
  -- Property names can only be printable ascii, using the handle function here
  -- as a little shortcut.
  propertyNames <- replicateM 16 $ randomHandleWithRange 8 20
  propertyVals <- replicateM 16 $ randomJSON
  let properties = zip propertyNames propertyVals
  forM_ properties $ \(prop, val) ->
    setProperty user prop val `bindResponse` \resp ->
      resp.status `shouldMatchInt` 200

  getAllPropertyNames user `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatchSet` propertyNames

  getAllPropertyValues user `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatch` Map.fromList properties

testClearProperties :: App ()
testClearProperties = do
  user <- randomUser OwnDomain def

  propertyNames <- replicateM 16 $ randomHandleWithRange 8 20
  propertyVals <- replicateM 16 $ randomJSON
  let properties = zip propertyNames propertyVals
  forM_ properties $ \(prop, val) ->
    setProperty user prop val `bindResponse` \resp ->
      resp.status `shouldMatchInt` 200

  clearProperties user `bindResponse` \resp ->
    resp.status `shouldMatchInt` 200

  getAllPropertyNames user `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatchSet` mempty @[String]

  getAllPropertyValues user `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatch` Map.empty @String @Value

testMaxProperties :: App ()
testMaxProperties = do
  user <- randomUser OwnDomain def

  -- This is hardcoded in the prod code.
  let maxProperties = 16

  propertyNames <- replicateM maxProperties $ randomHandleWithRange 8 20
  propertyVals <- replicateM maxProperties $ randomJSON
  let properties = zip propertyNames propertyVals
  forM_ properties $ \(prop, val) ->
    setProperty user prop val `bindResponse` \resp ->
      resp.status `shouldMatchInt` 200

  seventeenthPropName <- randomHandleWithRange 8 20
  seventeenthPropVal <- randomJSON

  -- cannot set seventeenth property
  setProperty user seventeenthPropName seventeenthPropVal `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "too-many-properties"

  -- Old properties are maintained
  getAllPropertyValues user `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatch` Map.fromList properties

  -- Can still update the old properties
  newPropertyVals <- replicateM 16 $ randomJSON
  let newProperties = zip propertyNames newPropertyVals
  forM_ newProperties $ \(prop, val) ->
    setProperty user prop val `bindResponse` \resp ->
      resp.status `shouldMatchInt` 200

  getAllPropertyValues user `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatch` Map.fromList newProperties

testPropertyNameNotAscii :: App ()
testPropertyNameNotAscii = do
  user <- randomUser OwnDomain def
  setProperty user "döner" "yes" `bindResponse` \resp ->
    resp.status `shouldMatchInt` 400

testMaxLength :: App ()
testMaxLength = do
  user <- randomUser OwnDomain def

  maxKeyLength <- asInt $ readServiceConfig Brig %. "optSettings.setPropertyMaxKeyLen"
  maxValLength <- asInt $ readServiceConfig Brig %. "optSettings.setPropertyMaxValueLen"

  tooLongProperty <- randomHandleWithRange (maxKeyLength + 1) (maxKeyLength + 1)
  acceptableProperty <- randomHandleWithRange maxKeyLength maxKeyLength

  -- Two chars are taken by the quotes for string values.
  --
  -- We use the `randomHandleWithRange` function because having non-ascii
  -- characters or unprintable characters will increase the length of the JSON.
  tooLongValue <- randomHandleWithRange (maxValLength - 1) (maxValLength - 1)
  acceptableValue <- randomHandleWithRange (maxValLength - 2) (maxValLength - 2)

  setProperty user tooLongProperty acceptableValue `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "property-key-too-large"

  setProperty user acceptableProperty tooLongValue `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 403
    resp.json %. "label" `shouldMatch` "property-value-too-large"

  setProperty user acceptableProperty acceptableValue `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200

  getProperty user acceptableProperty `bindResponse` \resp -> do
    resp.status `shouldMatchInt` 200
    resp.json `shouldMatch` toJSON acceptableValue
