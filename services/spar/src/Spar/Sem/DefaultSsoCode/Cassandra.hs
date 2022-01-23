{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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

module Spar.Sem.DefaultSsoCode.Cassandra where

import Cassandra
import Imports
import Polysemy
import qualified Spar.Data as Data
import Spar.Sem.DefaultSsoCode

defaultSsoCodeToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (DefaultSsoCode ': r) a ->
  Sem r a
defaultSsoCodeToCassandra =
  interpret $
    embed @m . \case
      Get -> Data.getDefaultSsoCode
      Store ip -> Data.storeDefaultSsoCode ip
      Delete -> Data.deleteDefaultSsoCode
