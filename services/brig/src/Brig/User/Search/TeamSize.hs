{-# LANGUAGE StrictData #-}

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

module Brig.User.Search.TeamSize
  ( teamSize,
  )
where

import Brig.Types.Team (TeamSize (..))
import Brig.User.Search.Index
import Control.Monad.Catch (throwM)
import Data.Id
import Database.Bloodhound qualified as ES
import Imports hiding (log, searchable)
-- TODO: Not ideal to import interpreters
import Wire.IndexedUserStore.ElasticSearch (IndexedUserStoreError (..))

teamSize :: (MonadIndexIO m) => TeamId -> m TeamSize
teamSize t = liftIndexIO $ do
  indexName <- asks idxName
  countResEither <- ES.countByIndex indexName (ES.CountQuery query)
  countRes <- either (throwM . IndexLookupError) pure countResEither
  pure . TeamSize $ ES.crCount countRes
  where
    query =
      ES.TermQuery
        ES.Term
          { ES.termField = "team",
            ES.termValue = idToText t
          }
        Nothing
