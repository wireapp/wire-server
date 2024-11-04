-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Test.System.Logger.ExtendedSpec where

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.String.Conversions
import Imports
import System.IO.Temp
import System.Logger.Extended hiding ((.=))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec =
  describe "System.Loggger.Extended" $ do
    it "instance {To,From}JSON LogFormat" $ do
      Aeson.eitherDecode' "[\"JSON\", \"Plain\", \"Netstring\", \"StructuredJSON\"]" `shouldBe` Right [minBound @LogFormat ..]
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
