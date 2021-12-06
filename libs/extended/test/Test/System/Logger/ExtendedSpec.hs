module Test.System.Logger.ExtendedSpec where

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import Data.String.Conversions (cs)
import Imports
import System.IO.Temp
import System.Logger.Extended hiding ((.=))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "System.Loggger.Extended" $ do
    describe "LogFormat: StructuredJSON" $ do
      it "should encode logs as new line separated structured JSON with log level, messages and fields" $ do
        withSystemTempFile "structured-json" $ \f h -> do
          hClose h -- The handle is not required
          l <-
            new
              . setRenderer structuredJSONRenderer
              . setOutput (Path f)
              . setFormat Nothing -- date format, not having it makes it easier to test.
              $ defSettings

          warn l $
            msg ("first message" :: ByteString)
              . field "field1" ("val 1.1" :: ByteString)
              . field "field2" ("val 2" :: ByteString)
              . field "field1" ("val 1.2" :: ByteString)
              . msg ("second message" :: ByteString)
          info l $ msg ("just a message" :: ByteString)

          flush l
          close l
          actualLogs <- map (Aeson.eitherDecode @Aeson.Value . cs) . lines <$> readFile f

          let expectedLogs =
                [ Aeson.object
                    [ "level" .= Warn,
                      "msgs" .= ["first message" :: Text, "second message"],
                      "field1" .= ["val 1.1" :: Text, "val 1.2"],
                      "field2" .= ("val 2" :: Text)
                    ],
                  Aeson.object
                    [ "level" .= Info,
                      "msgs" .= ["just a message" :: Text]
                    ]
                ]
          actualLogs `shouldBe` (Right <$> expectedLogs)
