{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.SAMLUserStore.Mem where

import Control.Lens (view)
import Data.Coerce (coerce)
import Data.Id
import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State (evalState, gets, modify)
import SAML2.WebSSO (uidTenant)
import qualified SAML2.WebSSO as SAML
import Spar.Sem.SAMLUserStore

newtype UserRefOrd = UserRefOrd {unUserRefOrd :: SAML.UserRef}
  deriving (Eq)

instance Ord UserRefOrd where
  compare (UserRefOrd (SAML.UserRef is ni)) (UserRefOrd (SAML.UserRef is' ni')) =
    compare is is' <> compare ni ni'

samlUserStoreToMem :: Sem (SAMLUserStore ': r) a -> Sem r a
samlUserStoreToMem = (evalState @(Map UserRefOrd UserId) mempty .) $
  reinterpret $ \case
    Insert ur uid -> modify $ M.insert (UserRefOrd ur) uid
    Get ur -> gets $ M.lookup $ UserRefOrd ur
    GetAnyByIssuer is -> gets $ fmap snd . find (eqIssuer is . fst) . M.toList
    GetSomeByIssuer is -> gets $ coerce . filter (eqIssuer is . fst) . M.toList
    DeleteByIssuer is -> modify $ M.filterWithKey (\ref _ -> not $ eqIssuer is ref)
    -- TODO(sandy): Is it OK to ignore uid here?
    Delete _uid ur -> modify $ M.delete $ UserRefOrd ur
  where
    eqIssuer :: SAML.Issuer -> UserRefOrd -> Bool
    eqIssuer is = (== is) . view uidTenant . unUserRefOrd
