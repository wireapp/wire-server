{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Spar.Sem.IdP.Spec (propsForInterpreter) where

import Control.Arrow
import Control.Lens
import Data.Data (Data)
import Imports
import Polysemy
import Polysemy.Check
import SAML2.WebSSO.Types
import qualified SAML2.WebSSO.Types as SAML
import qualified Spar.Sem.IdP as E
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Wire.API.User.IdentityProvider as IP

deriving instance Data IdPId

deriving instance Data (E.GetIdPResult IdPId)

propsForInterpreter ::
  (Member E.IdP r, PropConstraints r f) =>
  String ->
  (forall x. f x -> x) ->
  (forall x. Show x => Maybe (f x -> String)) ->
  (forall x. Sem r x -> IO (f x)) ->
  Spec
propsForInterpreter interpreter extract labeler lower = do
  describe interpreter $ do
    prop "deleteConfig/deleteConfig" $ prop_deleteDelete Nothing lower
    prop "deleteConfig/getConfig" $ prop_deleteGet labeler lower
    prop "getConfig/storeConfig" $ prop_getStore (Just $ show . (() <$) . extract) lower
    prop "getConfig/getConfig" $ prop_getGet (Just $ show . ((() <$) *** (() <$)) . extract) lower
    prop "setReplacedBy/clearReplacedBy" $ prop_setClear labeler lower
    prop "setReplacedBy/getReplacedBy" $ prop_setGet (Just $ show . (fmap (() <$)) . extract) lower
    prop "setReplacedBy/setReplacedBy" $ prop_setSet (Just $ show . (fmap (() <$)) . extract) lower
    prop "storeConfig/getConfig" $ prop_storeGet (Just $ show . (() <$) . extract) lower
    xit "storeConfig/getIdByIssuerWithoutTeam" $ property $ prop_storeGetByIssuer (Just $ constructorLabel . extract) lower
    prop "storeConfig/storeConfig (different keys)" $ prop_storeStoreInterleave Nothing lower
    prop "storeConfig/storeConfig (same keys)" $ prop_storeStore Nothing lower

getReplacedBy :: Member E.IdP r => SAML.IdPId -> Sem r (Maybe (Maybe SAML.IdPId))
getReplacedBy idpid = fmap (view $ SAML.idpExtraInfo . IP.wiReplacedBy) <$> E.getConfig idpid

-- | All the constraints we need to generalize properties in this module.
-- A regular type synonym doesn't work due to dreaded impredicative
-- polymorphism.
class
  (Arbitrary Issuer, CoArbitrary Issuer, Arbitrary E.Replaced, Arbitrary E.Replaced, Arbitrary E.Replacing, Arbitrary IdPId, CoArbitrary IdPId, Arbitrary IP.IdP, CoArbitrary IP.IdP, CoArbitrary (E.GetIdPResult IdPId), Functor f, Member E.IdP r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z)) =>
  PropConstraints r f

instance
  (Arbitrary Issuer, CoArbitrary Issuer, Arbitrary E.Replaced, Arbitrary E.Replaced, Arbitrary E.Replacing, Arbitrary IdPId, CoArbitrary IdPId, Arbitrary IP.IdP, CoArbitrary IP.IdP, CoArbitrary (E.GetIdPResult IdPId), Functor f, Member E.IdP r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z)) =>
  PropConstraints r f

prop_storeStore ::
  PropConstraints r f =>
  Maybe (f () -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_storeStore =
  prepropLaw @'[E.IdP] $ do
    s <- arbitrary
    s' <- arbitrary
    pure $
      Law
        { lawLhs = do
            E.storeConfig $ s & SAML.idpId .~ s' ^. SAML.idpId
            E.storeConfig s',
          lawRhs = do
            E.storeConfig s',
          lawPrelude = [],
          lawPostlude = [E.getConfig $ s' ^. SAML.idpId]
        }

prop_storeStoreInterleave ::
  PropConstraints r f =>
  Maybe (f () -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_storeStoreInterleave =
  prepropLaw @'[E.IdP] $ do
    s <- arbitrary
    s' <- arbitrary
    !_ <-
      when (s ^. SAML.idpId == s' ^. SAML.idpId) discard
    pure $
      Law
        { lawLhs = do
            E.storeConfig s
            E.storeConfig s',
          lawRhs = do
            E.storeConfig s'
            E.storeConfig s,
          lawPrelude = [],
          lawPostlude = [E.getConfig $ s ^. SAML.idpId, E.getConfig $ s' ^. SAML.idpId]
        }

prop_storeGet ::
  PropConstraints r f =>
  Maybe (f (Maybe IP.IdP) -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_storeGet =
  prepropLaw @'[E.IdP] $
    do
      s <- arbitrary
      pure $
        simpleLaw
          ( do
              E.storeConfig s
              E.getConfig $ s ^. idpId
          )
          ( do
              E.storeConfig s
              pure (Just s)
          )

prop_deleteGet ::
  PropConstraints r f =>
  Maybe (f (Maybe IP.IdP) -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_deleteGet =
  prepropLaw @'[E.IdP] $ do
    s <- arbitrary
    pure $
      Law
        { lawLhs = do
            E.deleteConfig s
            E.getConfig $ s ^. SAML.idpId,
          lawRhs = do
            E.deleteConfig s
            pure Nothing,
          lawPrelude =
            [ E.storeConfig s
            ],
          lawPostlude = [] :: [Sem r ()]
        }

prop_deleteDelete ::
  PropConstraints r f =>
  Maybe (f () -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_deleteDelete =
  prepropLaw @'[E.IdP] $ do
    s <- arbitrary
    pure $
      simpleLaw
        ( do
            E.deleteConfig s
            E.deleteConfig s
        )
        ( do
            E.deleteConfig s
        )

prop_storeGetByIssuer ::
  PropConstraints r f =>
  Maybe (f (E.GetIdPResult IdPId) -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_storeGetByIssuer =
  prepropLaw @'[E.IdP] $
    do
      s <- arbitrary
      pure $
        simpleLaw
          ( do
              E.storeConfig s
              E.getIdByIssuerWithoutTeam $ s ^. idpMetadata . edIssuer
          )
          ( do
              E.storeConfig s
              -- NOT TRUE! This can also return E.GetIdPNonUnique with nonzero probability!
              pure $ E.GetIdPFound $ s ^. idpId
          )

prop_setClear ::
  PropConstraints r f =>
  Maybe (f (Maybe (Maybe IdPId)) -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_setClear =
  prepropLaw @'[E.IdP] $
    do
      idp <- arbitrary
      replaced_id <- arbitrary
      let replaced = E.Replaced replaced_id
      replacing <- arbitrary
      pure $
        Law
          { lawLhs = do
              E.setReplacedBy replaced replacing
              E.clearReplacedBy replaced
              getReplacedBy replaced_id,
            lawRhs = do
              E.clearReplacedBy replaced
              getReplacedBy replaced_id,
            lawPrelude =
              [ E.storeConfig $ idp & SAML.idpId .~ replaced_id
              ],
            lawPostlude = [] @(Sem _ ())
          }

prop_getGet ::
  forall r f.
  PropConstraints r f =>
  Maybe (f (Maybe IP.IdP, Maybe IP.IdP) -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_getGet =
  prepropLaw @'[E.IdP] $
    do
      idpid <- arbitrary
      idp <- arbitrary
      pure $
        Law
          { lawLhs = do
              liftA2 (,) (E.getConfig idpid) (E.getConfig idpid),
            lawRhs = do
              cfg <- E.getConfig idpid
              pure (cfg, cfg),
            lawPrelude =
              [ E.storeConfig $ idp & SAML.idpId .~ idpid
              ],
            lawPostlude = [] :: [Sem r ()]
          }

prop_getStore ::
  PropConstraints r f =>
  Maybe (f (Maybe IP.IdP) -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_getStore =
  prepropLaw @'[E.IdP] $
    do
      idpid <- arbitrary
      s <- arbitrary
      let s' = s & SAML.idpId .~ idpid
      pure $
        Law
          { lawLhs = do
              r <- E.getConfig idpid
              maybe (pure ()) E.storeConfig r
              pure r,
            lawRhs = do
              E.getConfig idpid,
            lawPrelude =
              [E.storeConfig s'],
            lawPostlude =
              [E.getConfig idpid]
          }

prop_setSet ::
  PropConstraints r f =>
  Maybe (f (Maybe (Maybe IdPId)) -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_setSet =
  prepropLaw @'[E.IdP] $
    do
      replaced_id <- arbitrary
      s <- arbitrary
      let s' = s & SAML.idpId .~ replaced_id
      let replaced = E.Replaced replaced_id
      replacing <- arbitrary
      replacing' <- arbitrary
      pure $
        Law
          { lawLhs = do
              E.setReplacedBy replaced replacing
              E.setReplacedBy replaced replacing'
              getReplacedBy replaced_id,
            lawRhs = do
              E.setReplacedBy replaced replacing'
              getReplacedBy replaced_id,
            lawPrelude =
              [E.storeConfig s'],
            lawPostlude = [] @(Sem _ ())
          }

prop_setGet ::
  PropConstraints r f =>
  Maybe (f (Maybe (Maybe IdPId)) -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_setGet =
  prepropLaw @'[E.IdP] $
    do
      idp <- arbitrary
      replaced_id <- arbitrary
      let replaced = E.Replaced replaced_id
      replacing_id <- arbitrary
      let replacing = E.Replacing replacing_id
      pure $
        Law
          { lawLhs = do
              E.setReplacedBy replaced replacing
              getReplacedBy replaced_id,
            lawRhs = do
              E.setReplacedBy replaced replacing
              (Just replacing_id <$) <$> E.getConfig replaced_id,
            lawPrelude =
              [ E.storeConfig $ idp & SAML.idpId .~ replaced_id
              ],
            lawPostlude = [] :: [Sem r ()]
          }
