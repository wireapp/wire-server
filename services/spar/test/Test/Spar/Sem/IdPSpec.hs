{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS_GHC -Wno-orphans             #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Test.Spar.Sem.IdPSpec where

import Arbitrary ()
import Control.Lens
import Imports
import Polysemy
import Polysemy.Check
import SAML2.WebSSO.Types
import qualified Spar.Sem.IdP as E
import Spar.Sem.IdP.Mem
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

deriveGenericK ''E.IdP

propsForInterpreter ::
  Member E.IdP r =>
  String ->
  (forall a. Sem r a -> IO (TypedState, a)) ->
  Spec
propsForInterpreter interpreter lower = do
  describe interpreter $ do
    prop "storeConfig/getConfig" $ prop_storeGet lower
    prop "storeConfig/getIdByIssuerWithoutTeam" $ prop_storeGetByIssuer lower

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  propsForInterpreter "idPToMem" $ pure . run . idPToMem

-- | All the constraints we need to generalize properties in this module.
-- A regular type synonym doesn't work due to dreaded impredicative
-- polymorphism.
class (Member E.IdP r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z))
   => PropConstraints r f
instance (Member E.IdP r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z))
   => PropConstraints r f

prop_storeGet ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_storeGet =
  prepropLaw @'[E.IdP] $
    do
        s <- arbitrary
        pure
          ( do
              E.storeConfig s
              E.getConfig $ s ^. idpId,
            do
              E.storeConfig s
              pure (Just s)
          )

prop_storeGetByIssuer ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_storeGetByIssuer =
  prepropLaw @'[E.IdP] $
     do
        s <- arbitrary
        pure
          ( do
              E.storeConfig s
              E.getIdByIssuerWithoutTeam $ s ^. idpMetadata . edIssuer,
            do
              E.storeConfig s
              pure $ E.GetIdPFound $ s ^. idpId
          )

