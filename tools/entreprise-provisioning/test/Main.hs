-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2025 Wire Swiss GmbH <opensource@wire.com>
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

module Main (main) where

import Data.Aeson (eitherDecode, encode, object, (.=))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as LBS
import Data.Id
import Data.Map.Strict qualified as Map
import Data.Range
import Imports
import Test.Tasty
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit
import Types

main :: IO ()
main =
  defaultMain $
    testGroup
      "Tests"
      [ testGroup
          "Unit Tests"
          [ testCase "Parse valid input JSON" testParseValidInput,
            testCase "Serialize success output" testSerializeSuccessOutput,
            testCase "Serialize partial failure output" testSerializePartialFailureOutput,
            testCase "Serialize association failure output" testSerializeAssociationFailureOutput
          ],
        testGroup
          "Golden Tests"
          [ goldenVsString
              "Success output format"
              "test/golden/output-success.json"
              (pure $ encodePretty mockSuccessOutput),
            goldenVsString
              "Partial failure output format"
              "test/golden/output-partial-failure.json"
              (pure $ encodePretty mockPartialFailureOutput),
            goldenVsString
              "Association failure output format"
              "test/golden/output-association-failure.json"
              (pure $ encodePretty mockAssociationFailureOutput)
          ]
      ]

-- | Test parsing valid input JSON
testParseValidInput :: IO ()
testParseValidInput = do
  content <- LBS.readFile "test/golden/input-valid.json"
  case eitherDecode content of
    Left err -> assertFailure $ "Failed to parse input: " ++ err
    Right (UserGroupChannelsProvisionningSpec input) -> do
      length (Map.keys input) @?= 4
      assertBool "Input should contain user groups" (not (Map.null input))

-- | Test serializing success output
testSerializeSuccessOutput :: IO ()
testSerializeSuccessOutput = do
  let output = encode mockSuccessOutput
  assertBool "Output should not be empty" (LBS.length output > 0)

-- | Test serializing partial failure output
testSerializePartialFailureOutput :: IO ()
testSerializePartialFailureOutput = do
  let output = encode mockPartialFailureOutput
  assertBool "Output should not be empty" (LBS.length output > 0)

-- | Test serializing association failure output
testSerializeAssociationFailureOutput :: IO ()
testSerializeAssociationFailureOutput = do
  let output = encode mockAssociationFailureOutput
  assertBool "Output should not be empty" (LBS.length output > 0)

-- * Fixtures

-- | Mock successful output
mockSuccessOutput :: UserGroupChannelsProvisionningResult
mockSuccessOutput =
  UserGroupChannelsProvisionningResult $
    Map.fromList
      [ ( mkId "00000000-0000-0000-0000-0000000000a0",
          UserGroupResult
            { channel =
                [ ChannelSuccess (mkChannelName "channel name 0") (mkId "00000000-0000-0000-0000-000000000000"),
                  ChannelSuccess (mkChannelName "channel name 1") (mkId "00000000-0000-0000-0000-000000000001")
                ],
              association = AssociationSuccess
            }
        ),
        ( mkId "00000000-0000-0000-0000-0000000000a1",
          UserGroupResult
            { channel =
                [ ChannelSuccess (mkChannelName "channel name 0") (mkId "00000000-0000-0000-0000-000000000010")
                ],
              association = AssociationSuccess
            }
        )
      ]

-- | Mock partial failure output (some channels fail to create)
mockPartialFailureOutput :: UserGroupChannelsProvisionningResult
mockPartialFailureOutput =
  UserGroupChannelsProvisionningResult $
    Map.fromList
      [ ( mkId "00000000-0000-0000-0000-0000000000a0",
          UserGroupResult
            { channel =
                [ ChannelSuccess (mkChannelName "channel name 0") (mkId "00000000-0000-0000-0000-000000000000"),
                  ChannelFailure
                    (mkChannelName "channel name 1")
                    ( ErrorDetail
                        409
                        (object ["label" .= ("invalid-op" :: Text), "message" .= ("Channel already exists" :: Text)])
                    )
                ],
              association = AssociationSuccess
            }
        )
      ]

-- | Mock association failure output (channels created but association fails)
mockAssociationFailureOutput :: UserGroupChannelsProvisionningResult
mockAssociationFailureOutput =
  UserGroupChannelsProvisionningResult $
    Map.fromList
      [ ( mkId "00000000-0000-0000-0000-0000000000a0",
          UserGroupResult
            { channel =
                [ ChannelSuccess (mkChannelName "channel name 0") (mkId "00000000-0000-0000-0000-000000000000"),
                  ChannelSuccess (mkChannelName "channel name 1") (mkId "00000000-0000-0000-0000-000000000001")
                ],
              association =
                AssociationFailureResult
                  ( ErrorDetail
                      404
                      (object ["label" .= ("not-found" :: Text), "message" .= ("User group not found" :: Text)])
                  )
            }
        )
      ]

-- * Unsafe helpers

mkId :: String -> Id a
mkId s = Id (read s)

mkChannelName :: Text -> ChannelName
mkChannelName t = ChannelName (unsafeRange t)
