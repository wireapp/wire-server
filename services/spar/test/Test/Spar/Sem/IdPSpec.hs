{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Spar.Sem.IdPSpec where

import Arbitrary ()
import Imports
import qualified Spar.Sem.IdP as E
import Polysemy.Check
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Polysemy
import SAML2.WebSSO.Types
import Spar.Sem.IdP.Mem
import qualified Wire.API.User.IdentityProvider as IdP
import qualified SAML2.WebSSO as SAML
import Control.Lens


deriveGenericK ''E.IdP

prop_storeGet
    :: Member E.IdP r
    => (forall a. Sem r a -> IO (IS, a))
    -> Property
prop_storeGet x =
  prepropLaw @'[E.IdP]
    ( do
        s <- arbitrary
        pure
          ( do
              E.storeConfig s
              E.getConfig $ s ^. idpId
          , do
              E.storeConfig s
              pure (Just s)
          )
    )
    x

prop_storeGetRaw
    :: Member E.IdP r
    => (forall a. Sem r a -> IO (IS, a))
    -> Property
prop_storeGetRaw x =
  prepropLaw @'[E.IdP]
    ( do
      idpid <- arbitrary
      t <- arbitrary
      pure
        ( do
            E.storeRawMetadata idpid t
            E.getRawMetadata idpid
        , do
            E.storeRawMetadata idpid t
            pure (Just t)
        )
    )
    x

prop_storeStoreRaw
    :: Member E.IdP r
    => (forall a. Sem r a -> IO (IS, a))
    -> Property
prop_storeStoreRaw x =
  prepropLaw @'[E.IdP]
    ( do
      idpid <- arbitrary
      t1 <- arbitrary
      t2 <- arbitrary
      pure
        ( do
            E.storeRawMetadata idpid t1
            E.storeRawMetadata idpid t2
        , do
            E.storeRawMetadata idpid t2
        )
    )
    x

prop_storeDeleteRaw
    :: Member E.IdP r
    => (forall a. Sem r a -> IO (IS, a))
    -> Property
prop_storeDeleteRaw x =
  prepropLaw @'[E.IdP]
    ( do
      idpid <- arbitrary
      t <- arbitrary
      pure
        ( do
            E.storeRawMetadata idpid t
            E.deleteRawMetadata idpid
        , do
            E.deleteRawMetadata idpid
        )
    )
    x

prop_deleteGetRaw
    :: Member E.IdP r
    => (forall a. Sem r a -> IO (IS, a))
    -> Property
prop_deleteGetRaw x =
  prepropLaw @'[E.IdP]
    ( do
      idpid <- arbitrary
      pure
        ( do
            E.deleteRawMetadata idpid
            E.getRawMetadata idpid
        , do
            E.deleteRawMetadata idpid
            pure Nothing
        )
    )
    x

prop_storeGetByIssuer
    :: Member E.IdP r
    => (forall a. Sem r a -> IO (IS, a))
    -> Property
prop_storeGetByIssuer x =
  prepropLaw @'[E.IdP]
    ( do
      s <- arbitrary
      pure
        ( do
            E.storeConfig s
            E.getIdByIssuerWithoutTeam $ s ^. idpMetadata . edIssuer
        , do
            E.storeConfig s
            pure $ E.GetIdPFound $ s ^. idpId
        )
    )
    x

testInterpreter :: Sem '[E.IdP] a -> IO ((Map SAML.IdPId IdP.IdP, Map SAML.IdPId Text), a)
testInterpreter = pure . run . idPToMem

propsForInterpreter
    :: Member E.IdP r
    => (forall a. Sem r a -> IO (IS, a))
    -> Spec
propsForInterpreter lower = do
  describe "Config Actions" $ do
    prop "storeConfig/getConfig" $ prop_storeGet lower
    prop "storeConfig/getIdByIssuerWithoutTeam" $ prop_storeGetByIssuer lower

  describe "Raw Metadata Actions" $ do
    prop "storeRawMetadata/storeRawMetadata" $ prop_storeStoreRaw lower
    prop "storeRawMetadata/getRawMetadata" $ prop_storeGetRaw lower
    prop "storeRawMetadata/deleteRawMetadata" $ prop_storeDeleteRaw lower
    prop "deleteRawMetadata/getRawMetadata" $ prop_deleteGetRaw lower


spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  propsForInterpreter testInterpreter

