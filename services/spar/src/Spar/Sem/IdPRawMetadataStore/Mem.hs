{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.IdPRawMetadataStore.Mem (idpRawMetadataStoreToMem) where

import qualified Data.Map as M
import Imports
import Polysemy
import Polysemy.State (State, modify, gets, evalState)
import qualified SAML2.WebSSO.Types as SAML
import Spar.Sem.IdPRawMetadataStore

type RawState = Map SAML.IdPId Text

idpRawMetadataStoreToMem ::
  forall r a.
  Sem (IdPRawMetadataStore ': r) a ->
  Sem r a
idpRawMetadataStoreToMem = evState . evEff
  where
    evState :: Sem (State RawState : r) a -> Sem r a
    evState = evalState mempty

    evEff :: Sem (IdPRawMetadataStore ': r) a -> Sem (State RawState ': r) a
    evEff = reinterpret @_ @(State RawState) $ \case
      Store i txt ->
        modify $ M.insert i txt
      Get i ->
        gets $ M.lookup i
      Delete idpid ->
        modify $ M.filterWithKey (\idpid' _ -> idpid' /= idpid)
