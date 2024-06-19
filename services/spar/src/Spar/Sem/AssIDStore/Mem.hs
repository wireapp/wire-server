{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

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

module Spar.Sem.AssIDStore.Mem
  ( assIdStoreToMem,
  )
where

import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State
import qualified SAML2.WebSSO.Types as SAML
import Spar.Sem.AssIDStore
import Wire.API.User.Saml (AssId)
import Wire.Sem.Now

assIdStoreToMem ::
  (Member Now r) =>
  Sem (AssIDStore ': r) a ->
  Sem r (Map AssId SAML.Time, a)
assIdStoreToMem = (runState mempty .) $
  reinterpret $ \case
    Store assid ti -> modify $ M.insert assid ti
    UnStore assid -> modify $ M.delete assid
    IsAlive assid ->
      gets (M.lookup assid) >>= \case
        Just time -> boolTTL False True time
        Nothing -> pure False
