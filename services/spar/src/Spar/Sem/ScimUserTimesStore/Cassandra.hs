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

module Spar.Sem.ScimUserTimesStore.Cassandra where

import Cassandra (MonadClient)
import Imports
import Polysemy
import qualified Spar.Data as Data
import Spar.Sem.ScimUserTimesStore

scimUserTimesStoreToCassandra :: forall m r a. (MonadClient m, Member (Embed m) r) => Sem (ScimUserTimesStore ': r) a -> Sem r a
scimUserTimesStoreToCassandra =
  interpret $
    embed @m . \case
      Write wm -> Data.writeScimUserTimes wm
      Read uid -> Data.readScimUserTimes uid
      Delete uid -> Data.deleteScimUserTimes uid
