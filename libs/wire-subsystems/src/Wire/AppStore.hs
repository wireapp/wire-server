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

import GHC.TypeNats
import Data.Aeson
import Data.Id
import Data.UUID
import Imports
import Polysemy
import Data.Range
import Wire.API.App
import Wire.API.PostgresMarshall

data StoredApp = StoredApp
  { id :: UserId,
    teamId :: TeamId,
    meta :: Object,
    category :: Category,
    description :: Range 1 300 Text,
    author :: Range 1 256 Text
  }
  deriving (Eq, Ord, Show)

instance PostgresMarshall StoredApp (UUID, UUID, Value, Text, Text, Text) where
  postgresMarshall app =
    ( postgresMarshall app.id,
      postgresMarshall app.teamId,
      postgresMarshall app.meta,
      postgresMarshall (categoryToText app.category),
      postgresMarshall (fromRange app.description),
      postgresMarshall (fromRange app.author)
    )

instance PostgresUnmarshall (UUID, UUID, Value, Text, Text, Text) StoredApp where
  postgresUnmarshall (uid, teamId, meta, category, description, author) =
    StoredApp
      <$> postgresUnmarshall uid
      <*> postgresUnmarshall teamId
      <*> postgresUnmarshall meta
      <*> (postgresUnmarshall =<< categoryFromText category)
      <*> (inbound @1 @300 "description" =<< postgresUnmarshall description)
      <*> (inbound @1 @256 "author" =<< postgresUnmarshall author)
    where
      inbound :: forall m n . (Within Text m n, KnownNat m, KnownNat n) => Text -> Text -> Either Text (Range m n Text)
      inbound what text = maybe (Left $ what <> " out of bounds") Right (checked @m @n text)

data AppStore m a where
  CreateApp :: StoredApp -> AppStore m ()
  GetApp :: UserId -> AppStore m (Maybe StoredApp)

makeSem ''AppStore
