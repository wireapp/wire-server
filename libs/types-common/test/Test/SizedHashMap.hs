-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Test.SizedHashMap
  ( tests,
  )
where

import qualified Data.HashMap.Strict as HM
import Data.SizedHashMap as SHM
import Imports
import Test.Tasty
import Test.Tasty.HUnit

zro, one, two :: HM.HashMap Char Char
zro = HM.empty
one = HM.insert '0' '0' zro
two = HM.insert '1' '1' one

zro', one', two' :: SHM.SizedHashMap Char Char
zro' = SHM.empty
one' = SHM.insert '0' '0' zro'
two' = SHM.insert '1' '1' one'

tests :: TestTree
tests =
  testGroup
    "SizedHashMap"
    [ testCase "empty" $ do
        SHM.size zro' @=? 0
        fromSizedHashMap zro' @=? zro,
      testCase "insert" $ do
        SHM.size one' @=? HM.size one
        SHM.size two' @=? HM.size two
        fromSizedHashMap one' @=? one
        fromSizedHashMap two' @=? two,
      testCase "keys" $ do
        SHM.keys zro' @=? HM.keys zro
        SHM.keys one' @=? HM.keys one
        SHM.keys two' @=? HM.keys two,
      testCase "elems" $ do
        SHM.elems zro' @=? HM.elems zro
        SHM.elems one' @=? HM.elems one
        SHM.elems two' @=? HM.elems two,
      testCase "toList" $ do
        SHM.toList zro' @=? HM.toList zro
        SHM.toList one' @=? HM.toList one
        SHM.toList two' @=? HM.toList two,
      testCase "lookup" $ do
        SHM.lookup '0' zro' @=? HM.lookup '0' zro
        SHM.lookup '0' one' @=? HM.lookup '0' one
        SHM.lookup '0' two' @=? HM.lookup '0' two,
      testCase "delete" $ do
        size (SHM.delete '0' zro') @=? 0
        size (SHM.delete '0' one') @=? 0
        size (SHM.delete '0' two') @=? 1
        fromSizedHashMap (SHM.delete '0' zro') @=? HM.delete '0' zro
        fromSizedHashMap (SHM.delete '0' one') @=? HM.delete '0' one
        fromSizedHashMap (SHM.delete '0' two') @=? HM.delete '0' two
    ]
