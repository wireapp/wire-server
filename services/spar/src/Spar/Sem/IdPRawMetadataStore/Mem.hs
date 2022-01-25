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

module Spar.Sem.IdPRawMetadataStore.Mem (idpRawMetadataStoreToMem, RawState) where

import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State (State, gets, modify, runState)
import qualified SAML2.WebSSO.Types as SAML
import Spar.Sem.IdPRawMetadataStore

type RawState = Map SAML.IdPId Text

idpRawMetadataStoreToMem ::
  forall r a.
  Sem (IdPRawMetadataStore ': r) a ->
  Sem r (RawState, a)
idpRawMetadataStoreToMem = runState mempty . evEff
  where
    evEff :: Sem (IdPRawMetadataStore ': r) a -> Sem (State RawState ': r) a
    evEff = reinterpret @_ @(State RawState) $ \case
      Store i txt ->
        modify $ M.insert i txt
      Get i ->
        gets $ M.lookup i
      Delete idpid ->
        modify $ M.filterWithKey (\idpid' _ -> idpid' /= idpid)
