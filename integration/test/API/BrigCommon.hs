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

module API.BrigCommon where

import API.Common
import Data.Aeson.Types (Pair)
import Data.Maybe
import Testlib.Prelude as Prelude

data AddClient = AddClient
  { ctype :: String, -- "temporary", "permanent", "legalhold"
    internal :: Bool,
    clabel :: String,
    model :: String,
    prekeys :: Maybe [Value],
    lastPrekey :: Maybe Value,
    password :: String,
    acapabilities :: Maybe [String]
  }

instance Default AddClient where
  def =
    AddClient
      { ctype = "permanent",
        internal = False,
        clabel = "Test Device",
        model = "Test Model",
        prekeys = Nothing,
        lastPrekey = Nothing,
        password = defPassword,
        acapabilities = Just ["legalhold-implicit-consent"]
      }

mkAddClientValue :: AddClient -> App [Pair]
mkAddClientValue args = do
  pks <- maybe (fmap pure getPrekey) pure args.prekeys
  lpk <- maybe getLastPrekey pure args.lastPrekey
  pure
    [ "prekeys" .= pks,
      "lastkey" .= lpk,
      "type" .= args.ctype,
      "label" .= args.clabel,
      "model" .= args.model,
      "password" .= args.password,
      "capabilities" .= args.acapabilities
    ]
