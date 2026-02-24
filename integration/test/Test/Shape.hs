{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

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

-- | Self-tests for the 'Shape' DSL and 'shouldMatchShape' assertion.
module Test.Shape where

import Testlib.Prelude

-- | A matching object shape succeeds.
testShapeObjectMatch :: (HasCallStack) => App ()
testShapeObjectMatch = do
  let v = object ["foo" .= (42 :: Int), "bar" .= ("hello" :: String)]
  v `shouldMatchShape` SObject [("foo", SNumber), ("bar", SString)]

-- | An unexpected key in the actual object causes a failure.
testShapeUnexpectedKey :: (HasCallStack) => App ()
testShapeUnexpectedKey = do
  let v = object ["foo" .= (1 :: Int), "extra" .= (2 :: Int)]
  expectFailure (\_ -> pure ()) $
    v `shouldMatchShape` SObject [("foo", SNumber)]

-- | A missing key in the actual object causes a failure.
testShapeMissingKey :: (HasCallStack) => App ()
testShapeMissingKey = do
  let v = object ["foo" .= (1 :: Int)]
  expectFailure (\_ -> pure ()) $
    v `shouldMatchShape` SObject [("foo", SNumber), ("bar", SString)]

-- | Providing a non-object value when 'SObject' is expected causes a failure.
testShapeWrongTypeObject :: (HasCallStack) => App ()
testShapeWrongTypeObject = do
  let v = toJSON ("hello" :: String)
  expectFailure (\_ -> pure ()) $
    v `shouldMatchShape` SObject [("foo", SNumber)]

-- | Providing a non-string when 'SString' is expected causes a failure.
testShapeWrongTypeString :: (HasCallStack) => App ()
testShapeWrongTypeString = do
  let v = Number 42
  expectFailure (\_ -> pure ()) $
    v `shouldMatchShape` SString

-- | An array element with the wrong type causes a failure, and the error
-- message includes the element index.
testShapeArrayElementMismatch :: (HasCallStack) => App ()
testShapeArrayElementMismatch = do
  -- First two elements are strings (match), third is a number (mismatch at [2])
  let v = toJSON [toJSON ("a" :: String), toJSON ("b" :: String), toJSON (3 :: Int)]
  expectFailure (\e -> e.msg `shouldContainString` "[2]") $
    v `shouldMatchShape` SArray SString

-- | A nested mismatch deep in an object/array reports the full JSON path.
testShapeNestedPathReported :: (HasCallStack) => App ()
testShapeNestedPathReported = do
  let v =
        object
          [ "assets"
              .= [ object
                     [ "key" .= (42 :: Int), -- wrong: should be SString
                       "size" .= ("preview" :: String),
                       "type" .= ("image" :: String)
                     ]
                 ]
          ]
  expectFailure (\e -> e.msg `shouldContainString` ".assets[0].key") $
    v
      `shouldMatchShape` SObject
        [ ( "assets",
            SArray
              ( SObject
                  [ ("key", SString),
                    ("size", SString),
                    ("type", SString)
                  ]
              )
          )
        ]

-- | 'SAny' is a wildcard that matches every JSON value.
testShapeSAny :: (HasCallStack) => App ()
testShapeSAny = do
  let vals :: [Value]
      vals = [Null, Bool True, toJSON ("x" :: String), Number 1, toJSON ([] :: [Int]), object []]
  mapM_ (`shouldMatchShape` SAny) vals

-- | An empty array matches 'SArray' with any element shape.
testShapeEmptyArray :: (HasCallStack) => App ()
testShapeEmptyArray = do
  let v = toJSON ([] :: [Int])
  v `shouldMatchShape` SArray SString
  v `shouldMatchShape` SArray SNumber
  v `shouldMatchShape` SArray (SObject [])

-- | 'valueShape' computes the correct shape of a JSON value.
testValueShape :: (HasCallStack) => App ()
testValueShape = do
  let v =
        object
          [ "name" .= ("Alice" :: String),
            "age" .= (30 :: Int),
            "active" .= True,
            "scores" .= [1 :: Int, 2, 3],
            "address" .= object ["city" .= ("London" :: String)]
          ]
  shape <- valueShape v
  -- The computed shape should itself pass the shape-match on v
  v `shouldMatchShape` shape
