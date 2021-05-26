{-# LANGUAGE OverloadedLists #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2021 Wire Swiss GmbH <opensource@wire.com>
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
module Test.Wire.API.Golden.Generated.TurnURI_user where

import Data.Misc (IpAddr (IpAddr))
import Imports (Maybe (Just, Nothing), read)
import Wire.API.Call.Config
  ( Scheme (SchemeTurn, SchemeTurns),
    Transport (TransportTCP, TransportUDP),
    TurnHost (TurnHostIp, TurnHostName),
    TurnURI,
    turnURI,
  )

testObject_TurnURI_user_1 :: TurnURI
testObject_TurnURI_user_1 = (turnURI (SchemeTurns) (TurnHostName "007.com") (read "4") (Just TransportTCP))

testObject_TurnURI_user_2 :: TurnURI
testObject_TurnURI_user_2 =
  (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "102.69.197.199"))) (read "0") (Just TransportUDP))

testObject_TurnURI_user_3 :: TurnURI
testObject_TurnURI_user_3 =
  (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "203.95.154.51"))) (read "0") (Just TransportTCP))

testObject_TurnURI_user_4 :: TurnURI
testObject_TurnURI_user_4 = (turnURI (SchemeTurns) (TurnHostName "123") (read "3") (Nothing))

testObject_TurnURI_user_5 :: TurnURI
testObject_TurnURI_user_5 = (turnURI (SchemeTurns) (TurnHostName "a-c") (read "8") (Just TransportUDP))

testObject_TurnURI_user_6 :: TurnURI
testObject_TurnURI_user_6 =
  (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "8") (Just TransportUDP))

testObject_TurnURI_user_7 :: TurnURI
testObject_TurnURI_user_7 =
  (turnURI (SchemeTurn) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "8") (Just TransportUDP))

testObject_TurnURI_user_8 :: TurnURI
testObject_TurnURI_user_8 =
  (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "150.156.243.74"))) (read "8") (Just TransportUDP))

testObject_TurnURI_user_9 :: TurnURI
testObject_TurnURI_user_9 = (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "6.222.51.171"))) (read "1") (Nothing))

testObject_TurnURI_user_10 :: TurnURI
testObject_TurnURI_user_10 = (turnURI (SchemeTurns) (TurnHostName "123") (read "0") (Just TransportTCP))

testObject_TurnURI_user_11 :: TurnURI
testObject_TurnURI_user_11 =
  (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "44.65.131.165"))) (read "0") (Just TransportUDP))

testObject_TurnURI_user_12 :: TurnURI
testObject_TurnURI_user_12 = (turnURI (SchemeTurn) (TurnHostName "007.com") (read "0") (Just TransportTCP))

testObject_TurnURI_user_13 :: TurnURI
testObject_TurnURI_user_13 =
  (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "37.234.77.74"))) (read "1") (Just TransportTCP))

testObject_TurnURI_user_14 :: TurnURI
testObject_TurnURI_user_14 = (turnURI (SchemeTurns) (TurnHostName "a-c") (read "4") (Just TransportUDP))

testObject_TurnURI_user_15 :: TurnURI
testObject_TurnURI_user_15 =
  (turnURI (SchemeTurn) (TurnHostIp (IpAddr (read "5.194.243.81"))) (read "8") (Just TransportTCP))

testObject_TurnURI_user_16 :: TurnURI
testObject_TurnURI_user_16 =
  (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "0") (Just TransportUDP))

testObject_TurnURI_user_17 :: TurnURI
testObject_TurnURI_user_17 =
  (turnURI (SchemeTurns) (TurnHostIp (IpAddr (read "217.142.35.220"))) (read "4") (Just TransportUDP))

testObject_TurnURI_user_18 :: TurnURI
testObject_TurnURI_user_18 = (turnURI (SchemeTurns) (TurnHostName "007.com") (read "6") (Just TransportUDP))

testObject_TurnURI_user_19 :: TurnURI
testObject_TurnURI_user_19 =
  (turnURI (SchemeTurns) (TurnHostName "xn--mgbh0fb.xn--kgbechtv") (read "4") (Just TransportTCP))

testObject_TurnURI_user_20 :: TurnURI
testObject_TurnURI_user_20 = (turnURI (SchemeTurns) (TurnHostName "host.name") (read "7") (Just TransportTCP))
