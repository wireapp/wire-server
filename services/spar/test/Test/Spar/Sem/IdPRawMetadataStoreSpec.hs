{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
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

module Test.Spar.Sem.IdPRawMetadataStoreSpec where

import Arbitrary ()
import Imports
import Polysemy
import qualified Spar.Sem.IdPRawMetadataStore as E
import Spar.Sem.IdPRawMetadataStore.Mem
import Spar.Sem.IdPRawMetadataStore.Spec
import Test.Hspec
import Test.Hspec.QuickCheck

testInterpreter :: Sem '[E.IdPRawMetadataStore] a -> IO (RawState, a)
testInterpreter = pure . run . idpRawMetadataStoreToMem

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  propsForInterpreter snd testInterpreter
