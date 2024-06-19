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

module Spar.Sem.AReqIDStore.Mem
  ( aReqIDStoreToMem,
  )
where

import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State
import qualified SAML2.WebSSO.Types as SAML
import Spar.Sem.AReqIDStore
import Wire.API.User.Saml (AReqId)
import Wire.Sem.Now

aReqIDStoreToMem ::
  (Member Now r) =>
  Sem (AReqIDStore ': r) a ->
  Sem r (Map AReqId SAML.Time, a)
aReqIDStoreToMem = (runState mempty .) $
  reinterpret $ \case
    Store areqid ti -> modify $ M.insert areqid ti
    UnStore areqid -> modify $ M.delete areqid
    IsAlive areqid ->
      gets (M.lookup areqid) >>= \case
        Just time -> do
          boolTTL False True time
        Nothing -> pure False
