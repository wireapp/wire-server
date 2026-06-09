{-# LANGUAGE QuasiQuotes #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.UserActivityStore.Postgres
  ( interpretUserActivityStoreToPostgres,
  )
where

import Data.Id
import Data.Time.Clock
import Hasql.TH
import Imports
import Polysemy
import Wire.Postgres
import Wire.UserActivityStore

interpretUserActivityStoreToPostgres ::
  (PGConstraints r) =>
  InterpreterFor UserActivityStore r
interpretUserActivityStoreToPostgres = interpret $ \case
  GetLastActivity uid -> getLastActivityImpl uid
  UpdateLastActivity uid t -> updateLastActivityImpl uid t
  DeleteLastActivity uid -> deleteLastActivityImpl uid

getLastActivityImpl :: (PGConstraints r) => UserId -> Sem r (Maybe UTCTime)
getLastActivityImpl uid =
  runStatement (toUUID uid) $
    [maybeStatement|
      SELECT active_at :: timestamptz
      FROM last_user_activity
      WHERE user_id = $1 :: uuid
    |]

updateLastActivityImpl :: (PGConstraints r) => UserId -> UTCTime -> Sem r ()
updateLastActivityImpl uid t =
  runStatement (toUUID uid, t) $
    [resultlessStatement|
      INSERT INTO last_user_activity (user_id, active_at)
      VALUES ($1 :: uuid, $2 :: timestamptz)
      ON CONFLICT (user_id) DO UPDATE SET active_at = EXCLUDED.active_at
    |]

deleteLastActivityImpl :: (PGConstraints r) => UserId -> Sem r ()
deleteLastActivityImpl uid =
  runStatement (toUUID uid) $
    [resultlessStatement|
      DELETE FROM last_user_activity WHERE user_id = $1 :: uuid
    |]
