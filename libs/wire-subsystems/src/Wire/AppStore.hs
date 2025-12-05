{-# LANGUAGE TemplateHaskell #-}

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

module Wire.AppStore where

import Data.Aeson
import Data.Id
import Data.Range
import Data.UUID
import GHC.TypeNats
import Imports
import Polysemy
import Wire.API.App
import Wire.API.PostgresMarshall

data StoredApp = StoredApp
  { id :: UserId,
    teamId :: TeamId,
    meta :: Object,
    category :: Category,
    description :: Range 0 300 Text,
    creator :: UserId
  }
  deriving (Eq, Ord, Show)

instance PostgresMarshall StoredApp (UUID, UUID, Value, Text, Text, UUID) where
  postgresMarshall app =
    ( postgresMarshall app.id,
      postgresMarshall app.teamId,
      postgresMarshall app.meta,
      postgresMarshall (categoryToText app.category),
      postgresMarshall (fromRange app.description),
      postgresMarshall app.creator
    )

instance PostgresUnmarshall (UUID, UUID, Value, Text, Text, UUID) StoredApp where
  postgresUnmarshall (uid, teamId, meta, category, description, creator) =
    StoredApp
      <$> postgresUnmarshall uid
      <*> postgresUnmarshall teamId
      <*> postgresUnmarshall meta
      <*> (postgresUnmarshall =<< maybe (Left $ "Category " <> category <> " not found") Right (categoryFromText category))
      <*> (textRange @0 @300 "description" =<< postgresUnmarshall description)
      <*> postgresUnmarshall creator
    where
      textRange :: forall n m. (Within Text n m, KnownNat m, KnownNat n) => Text -> Text -> Either Text (Range n m Text)
      textRange what text = maybe (Left $ what <> " out of bounds") Right (checked @n @m text)

data AppStore m a where
  CreateApp :: StoredApp -> AppStore m ()
  GetApp :: UserId -> TeamId -> AppStore m (Maybe StoredApp)

makeSem ''AppStore
