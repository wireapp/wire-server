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
    Transport (TransportTCP),
    TurnHost (TurnHostIp, TurnHostName),
    TurnURI,
    turnURI,
  )

testObject_TurnURI_user_1 :: TurnURI
testObject_TurnURI_user_1 = turnURI SchemeTurns (TurnHostName "007.com") (read "4") (Just TransportTCP)

testObject_TurnURI_user_9 :: TurnURI
testObject_TurnURI_user_9 = turnURI SchemeTurn (TurnHostIp (IpAddr (read "6.222.51.171"))) (read "1") Nothing
