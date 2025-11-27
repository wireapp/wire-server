{-# LANGUAGE TemplateHaskellQuotes #-}

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

module Cassandra.QQ (sql) where

import Imports
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))

-- | a simple quasi quoter to allow for tree-sitter syntax highlight injection.
-- This uses the name sql because that is known to tree-sitter, unlike cql
sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quotePat = error "Cassandra.QQ: sql quasiquoter cannot be used as pattern",
      quoteType = error "Cassandra.QQ: sql quasiquoter cannot be used as type",
      quoteDec = error "Cassandra.QQ: sql quasiquoter cannot be used as declaration",
      quoteExp = appE [|fromString|] . stringE
    }
