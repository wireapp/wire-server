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

module Spar.Sem.SAMLUserStore.Mem (
  UserRefOrd(..),
  samlUserStoreToMem ) where

import Control.Lens (view)
import Data.Coerce (coerce)
import Data.Id
import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State (gets, modify, runState)
import SAML2.WebSSO (uidTenant)
import qualified SAML2.WebSSO as SAML
import Spar.Sem.SAMLUserStore

newtype UserRefOrd = UserRefOrd {unUserRefOrd :: SAML.UserRef}
  deriving (Eq)

instance Ord UserRefOrd where
  compare (UserRefOrd (SAML.UserRef is ni)) (UserRefOrd (SAML.UserRef is' ni')) =
    compare is is' <> compare ni ni'

samlUserStoreToMem :: Sem (SAMLUserStore ': r) a -> Sem r (Map UserRefOrd UserId, a)
samlUserStoreToMem = (runState @(Map UserRefOrd UserId) mempty .) $
  reinterpret $ \case
    Insert ur uid -> modify $ M.insert (UserRefOrd ur) uid
    Get ur -> gets $ M.lookup $ UserRefOrd ur
    GetAnyByIssuer is -> gets $ fmap snd . find (eqIssuer is . fst) . M.toList
    GetSomeByIssuer is -> gets $ coerce . filter (eqIssuer is . fst) . M.toList
    DeleteByIssuer is -> modify $ M.filterWithKey (\ref _ -> not $ eqIssuer is ref)
    Delete _uid ur -> modify $ M.delete $ UserRefOrd ur
  where
    eqIssuer :: SAML.Issuer -> UserRefOrd -> Bool
    eqIssuer is = (== is) . view uidTenant . unUserRefOrd
