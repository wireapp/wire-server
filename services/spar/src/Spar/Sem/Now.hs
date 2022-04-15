{-# LANGUAGE TemplateHaskell #-}

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

module Spar.Sem.Now
  ( Now (..),
    get,
    boolTTL,
  )
where

import Imports
import Polysemy
import Polysemy.Check (deriveGenericK)
import qualified SAML2.WebSSO as SAML

data Now m a where
  Get :: Now m SAML.Time

makeSem ''Now
deriveGenericK ''Now

deriving instance Show (Now m a)

-- | Check a time against 'Now', checking if it's still alive (hasn't occurred yet.)
boolTTL ::
  Member Now r =>
  -- | The value to return if the TTL is expired
  a ->
  -- | The value to return if the TTL is alive
  a ->
  SAML.Time -> -- The time to check
  Sem r a
boolTTL f t time = do
  now <- get
  pure $ bool f t $ now <= time
