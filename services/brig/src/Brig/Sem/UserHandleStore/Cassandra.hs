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

module Brig.Sem.UserHandleStore.Cassandra (userHandleStoreToCassandra) where

import Brig.Data.Instances ()
import Brig.Sem.UserHandleStore
import Cassandra hiding (Consistency (..))
import qualified Cassandra as C
import Data.Handle
import Data.Id
import Imports hiding (All)
import Polysemy

userHandleStoreToCassandra ::
  forall m r a.
  (MonadClient m, Member (Embed m) r) =>
  Sem (UserHandleStore ': r) a ->
  Sem r a
userHandleStoreToCassandra =
  interpret $
    embed @m . \case
      InsertHandle h uid -> insertHandleQuery h uid
      GetHandleWithConsistency c h -> lookupHandleWithPolicy (mapConsistency c) h
      DeleteHandle h -> deleteHandleQuery h

deleteHandleQuery :: MonadClient m => Handle -> m ()
deleteHandleQuery h = retry x5 $ write handleDelete (params C.LocalQuorum (Identity h))

mapConsistency :: Consistency -> C.Consistency
mapConsistency = \case
  One -> C.One
  LocalQuorum -> C.LocalQuorum
  All -> C.All

{-# INLINE lookupHandleWithPolicy #-}

insertHandleQuery :: MonadClient m => Handle -> UserId -> m ()
insertHandleQuery newHandle uid =
  retry x5 $ write handleInsert (params C.LocalQuorum (newHandle, uid))

-- | Sending an empty 'Handle' here causes C* to throw "Key may not be empty"
-- error.
--
-- FUTUREWORK: This should ideally be tackled by hiding constructor for 'Handle'
-- and only allowing it to be parsed.
lookupHandleWithPolicy :: MonadClient m => C.Consistency -> Handle -> m (Maybe UserId)
lookupHandleWithPolicy policy h = do
  (runIdentity =<<)
    <$> retry x1 (query1 handleSelect (params policy (Identity h)))

handleSelect :: PrepQuery R (Identity Handle) (Identity (Maybe UserId))
handleSelect = "SELECT user FROM user_handle WHERE handle = ?"

handleInsert :: PrepQuery W (Handle, UserId) ()
handleInsert = "INSERT INTO user_handle (handle, user) VALUES (?, ?)"

handleDelete :: PrepQuery W (Identity Handle) ()
handleDelete = "DELETE FROM user_handle WHERE handle = ?"
