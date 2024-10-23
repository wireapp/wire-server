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

module Test.Wire.API.Golden.Manual.Presence
  ( testObject_Presence_1,
    testObject_Presence_2,
    testObject_Presence_3,
  )
where

import Data.Id
import Data.UUID qualified as UUID
import Imports
import Wire.API.Presence

testObject_Presence_1 :: Presence
testObject_Presence_1 =
  Presence
    (Id . fromJust $ UUID.fromString "174ccaea-7f26-11ef-86cc-27bb6bf3b319")
    (ConnId "wef")
    (fromJust $ parse "http://example.com/")
    Nothing
    0
    ""

testObject_Presence_2 :: Presence
testObject_Presence_2 =
  Presence
    (Id . fromJust $ UUID.fromString "174ccaea-7f26-11ef-86cc-37bb6bf3b319")
    (ConnId "wef3")
    (fromJust $ parse "http://example.com/3")
    (Just (ClientId 1))
    12323
    "" -- __field always has to be "", see ToSchema instance.

testObject_Presence_3 :: Presence
testObject_Presence_3 =
  Presence
    (Id . fromJust $ UUID.fromString "174ccaea-7f26-11ef-86cc-37bb6bf3b319")
    (ConnId "wef3")
    (fromJust $ parse "http://example.com/3")
    (Just (ClientId 1))
    0
    "" -- __field always has to be "", see ToSchema instance.
