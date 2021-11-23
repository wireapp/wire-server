{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS_GHC -Wno-orphans             #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Test.Spar.Sem.IdPSpec where

import qualified SAML2.WebSSO.Types as SAML
import qualified Wire.API.User.IdentityProvider as IP
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
    prop "deleteConfig/deleteConfig" $ prop_deleteDelete lower
    prop "deleteConfig/getConfig" $ prop_deleteGet lower
    prop "getConfig/StoreConfig" $ prop_getStore lower
    prop "getConfig/getConfig" $ prop_getGet lower
    prop "setReplacedBy/cleatReplacedBy" $ prop_setClear lower
    prop "setReplacedBy/getReplacedBy" $ prop_setGet lower
    prop "setReplacedBy/setReplacedBy" $ prop_setSet lower
    prop "storeConfig/getConfig" $ prop_storeGet lower
    prop "storeConfig/getIdByIssuerWithoutTeam" $ prop_storeGetByIssuer lower
    prop "storeConfig/storeConfig (different keys)" $ prop_storeStore lower
    prop "storeConfig/storeConfig (same keys)" $ prop_storeStore lower

spec :: Spec
spec = modifyMaxSuccess (const 1000) $ do
  propsForInterpreter "idPToMem" $ pure . run . idPToMem

getReplacedBy :: Member E.IdP r => SAML.IdPId -> Sem r (Maybe (Maybe SAML.IdPId))
getReplacedBy idpid = fmap (view $ SAML.idpExtraInfo . IP.wiReplacedBy) <$> E.getConfig idpid

-- | All the constraints we need to generalize properties in this module.
-- A regular type synonym doesn't work due to dreaded impredicative
-- polymorphism.
class (Member E.IdP r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z))
   => PropConstraints r f
instance (Member E.IdP r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z))
   => PropConstraints r f

prop_storeStore ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_storeStore =
  prepropLaw @'[E.IdP] $ do
        s <- arbitrary
        s' <- arbitrary
        pure
          ( do
              E.storeConfig $ s & SAML.idpId .~ s' ^. SAML.idpId
              E.storeConfig s',
            do
              E.storeConfig s'
          )

prop_storeStoreInterleave ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_storeStoreInterleave =
  prepropLaw @'[E.IdP] $ do
        s <- arbitrary
        s' <- arbitrary
        !_ <-
          if s ^. SAML.idpId == s' ^. SAML.idpId
             then discard
             else pure ()
        pure
          ( do
              E.storeConfig s
              E.storeConfig s',
            do
              E.storeConfig s'
              E.storeConfig s
          )

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

prop_deleteGet ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_deleteGet =
  prepropLaw @'[E.IdP] $ do
        s <- arbitrary
        pure
          ( do
              E.deleteConfig s
              E.getConfig $ s ^. SAML.idpId,
            do
              E.deleteConfig s
              pure Nothing
          )

prop_deleteDelete ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_deleteDelete =
  prepropLaw @'[E.IdP] $ do
        s <- arbitrary
        pure
          ( do
              E.deleteConfig s
              E.deleteConfig s,
            do
              E.deleteConfig s
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

prop_setClear ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_setClear =
  prepropLaw @'[E.IdP] $
     do
        replaced <- arbitrary
        replacing <- arbitrary
        pure
          ( do
              E.setReplacedBy replaced replacing
              E.clearReplacedBy replaced,
            do
              E.clearReplacedBy replaced
          )

prop_getGet ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_getGet =
  prepropLaw @'[E.IdP] $
     do
        idpid <- arbitrary
        pure
          ( do
              liftA2 (,) (E.getConfig idpid) (E.getConfig idpid),
            do
              cfg <- E.getConfig idpid
              pure (cfg, cfg)
          )

prop_getStore ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_getStore =
  prepropLaw @'[E.IdP] $
     do
        idpid <- arbitrary
        pure
          ( do
              E.getConfig idpid >>= maybe (pure ()) E.storeConfig,
            do
              pure ()
          )

prop_setSet ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_setSet =
  prepropLaw @'[E.IdP] $
     do
        replaced <- arbitrary
        replacing <- arbitrary
        replacing' <- arbitrary
        pure
          ( do
              E.setReplacedBy replaced replacing
              E.setReplacedBy replaced replacing',
            do
              E.setReplacedBy replaced replacing'
          )

prop_setGet ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_setGet =
  prepropLaw @'[E.IdP] $
     do
        replaced_id <- arbitrary
        let replaced = E.Replaced replaced_id
        replacing_id <- arbitrary
        let replacing = E.Replacing replacing_id
        pure
          ( do
              E.setReplacedBy replaced replacing
              getReplacedBy replaced_id,
            do
              E.setReplacedBy replaced replacing
              (Just replacing_id <$) <$> E.getConfig replaced_id
          )

