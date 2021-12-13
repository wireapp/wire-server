{-# OPTIONS_GHC -Wno-orphans #-}
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

prop_storeGet ::
  Member E.IdP r =>
  (forall a. Sem r a -> IO (TypedState, a)) ->
  Property
prop_storeGet x =
  prepropLaw @'[E.IdP]
    ( do
        s <- arbitrary
        pure
          ( do
              E.storeConfig s
              E.getConfig $ s ^. idpId,
            do
              E.storeConfig s
              pure (Just s)
          )
    )
    x

prop_storeGetByIssuer ::
  Member E.IdP r =>
  (forall a. Sem r a -> IO (TypedState, a)) ->
  Property
prop_storeGetByIssuer x =
  prepropLaw @'[E.IdP]
    ( do
        s <- arbitrary
        pure
          ( do
              E.storeConfig s
              E.getIdByIssuerWithoutTeam $ s ^. idpMetadata . edIssuer,
            do
              E.storeConfig s
              pure $ E.GetIdPFound $ s ^. idpId
          )
    )
    x

testInterpreter :: Sem '[E.IdP] a -> IO (TypedState, a)
testInterpreter = pure . run . idPToMem

propsForInterpreter ::
  Member E.IdP r =>
  (forall a. Sem r a -> IO (TypedState, a)) ->
  Spec
propsForInterpreter lower = do
  describe "Config Actions" $ do
    prop "storeConfig/getConfig" $ prop_storeGet lower
    prop "storeConfig/getIdByIssuerWithoutTeam" $ prop_storeGetByIssuer lower

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  propsForInterpreter testInterpreter
