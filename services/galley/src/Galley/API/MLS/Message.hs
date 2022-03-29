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

module Galley.API.MLS.Message where

import Data.Id
import Data.Qualified
import Imports
import Polysemy
import Wire.API.MLS.Message
import Wire.API.MLS.Serialisation

postMLSMessage ::
  Local UserId ->
  ConnId ->
  RawMLS SomeMessage ->
  Sem r ()
postMLSMessage _lusr _con smsg = case rmValue smsg of
  SomeMessage _tag _msg -> undefined
