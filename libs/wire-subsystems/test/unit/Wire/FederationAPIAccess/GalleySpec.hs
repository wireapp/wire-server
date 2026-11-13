module Wire.FederationAPIAccess.GalleySpec (spec) where

import Data.Json.Util
import Imports
import Polysemy
import Polysemy.State
import Test.Hspec
import Wire.API.Federation.API.Galley
import Wire.FederationAPIAccess.Galley
import Wire.MockInterpreters.Now (defaultTime)
import Wire.Sem.Now qualified as Now

spec :: Spec
spec = describe "MLS federation response normalization" $ do
  let timestamp = toUTCTimeMillis (read "2026-11-11 10:54:51.693 UTC")
      success = MLSMessageSendSuccess [] timestamp
      failure = MLSMessageResponseProtocolError "invalid message"

  it "reads the local clock once for a legacy success" $ do
    normalizeWithClockReads (Left (MLSMessageResponseUpdates []))
      `shouldBe` (1, Right (MLSMessageSendSuccess [] (toUTCTimeMillis defaultTime)))

  it "preserves an authoritative timestamp without reading the local clock" $ do
    normalizeWithClockReads (Right (MLSMessageResponseV5Success success))
      `shouldBe` (0, Right success)

  it "preserves legacy errors without reading the local clock" $ do
    normalizeWithClockReads (Left failure)
      `shouldBe` (0, Left failure)

  it "preserves V5 errors without reading the local clock" $ do
    normalizeWithClockReads (Right (MLSMessageResponseV5Error failure))
      `shouldBe` (0, Left failure)

  it "rejects a success wrapped as a V5 error without reading the local clock" $ do
    normalizeWithClockReads (Right (MLSMessageResponseV5Error (MLSMessageResponseUpdates [])))
      `shouldBe` (0, Left (MLSMessageResponseProtocolError "unexpected success in federation error response"))

normalizeWithClockReads ::
  Either MLSMessageResponse MLSMessageResponseV5 ->
  (Int, Either MLSMessageResponse MLSMessageSendSuccess)
normalizeWithClockReads response =
  run
    . runState @Int 0
    . interpret
      ( \case
          Now.Get -> modify @Int (+ 1) $> defaultTime
      )
    $ normalizeMLSMessageResponse response
