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

module Wire.BlockListStore.Cassandra
  ( interpretBlockListStoreToCassandra,
  )
where

import Cassandra
import Imports
import Polysemy
import Wire.BlockListStore (BlockListStore (..))
import Wire.UserKeyStore

interpretBlockListStoreToCassandra ::
  forall r.
  (Member (Embed IO) r) =>
  ClientState ->
  InterpreterFor BlockListStore r
interpretBlockListStoreToCassandra casClient =
  interpret $
    embed @IO . runClient casClient . \case
      Insert uk -> insert uk
      Exists uk -> exists uk
      Delete uk -> delete uk

--------------------------------------------------------------------------------
-- UserKey block listing

insert :: (MonadClient m) => EmailKey -> m ()
insert uk = retry x5 $ write keyInsert (params LocalQuorum (Identity $ emailKeyUniq uk))

exists :: (MonadClient m) => EmailKey -> m Bool
exists uk =
  (pure . isJust) . fmap runIdentity
    =<< retry x1 (query1 keySelect (params LocalQuorum (Identity $ emailKeyUniq uk)))

delete :: (MonadClient m) => EmailKey -> m ()
delete uk = retry x5 $ write keyDelete (params LocalQuorum (Identity $ emailKeyUniq uk))

keyInsert :: PrepQuery W (Identity Text) ()
keyInsert = "INSERT INTO blacklist (key) VALUES (?)"

keySelect :: PrepQuery R (Identity Text) (Identity Text)
keySelect = "SELECT key FROM blacklist WHERE key = ?"

keyDelete :: PrepQuery W (Identity Text) ()
keyDelete = "DELETE FROM blacklist WHERE key = ?"
