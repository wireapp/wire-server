{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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

module Spar.Sem.IdPConfigStore.Spec (propsForInterpreter) where

import Control.Arrow
import Control.Lens
import Data.Data (Data)
import Imports
import Polysemy
import Polysemy.Check
import SAML2.WebSSO.Types
import qualified SAML2.WebSSO.Types as SAML
import Spar.Sem.IdPConfigStore
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Wire.API.User.IdentityProvider as IP

deriving instance Data IdPId

deriving instance Data (GetIdPResult IdPId)

propsForInterpreter ::
  (Member IdPConfigStore r, PropConstraints r f) =>
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

getReplacedBy :: Member IdPConfigStore r => SAML.IdPId -> Sem r (Maybe (Maybe SAML.IdPId))
getReplacedBy idpid = fmap (view $ SAML.idpExtraInfo . IP.wiReplacedBy) <$> getConfig idpid

-- | All the constraints we need to generalize properties in this module.
-- A regular type synonym doesn't work due to dreaded impredicative
-- polymorphism.
class
  (Arbitrary Issuer, CoArbitrary Issuer, Arbitrary Replaced, Arbitrary Replaced, Arbitrary Replacing, Arbitrary IdPId, CoArbitrary IdPId, Arbitrary IP.IdP, CoArbitrary IP.IdP, CoArbitrary (GetIdPResult IdPId), Functor f, Member IdPConfigStore r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z)) =>
  PropConstraints r f

instance
  (Arbitrary Issuer, CoArbitrary Issuer, Arbitrary Replaced, Arbitrary Replaced, Arbitrary Replacing, Arbitrary IdPId, CoArbitrary IdPId, Arbitrary IP.IdP, CoArbitrary IP.IdP, CoArbitrary (GetIdPResult IdPId), Functor f, Member IdPConfigStore r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z)) =>
  PropConstraints r f

prop_storeStore ::
  PropConstraints r f =>
  Maybe (f () -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_storeStore =
  prepropLaw @'[IdPConfigStore] $ do
    s <- arbitrary
    s' <- arbitrary
    pure $
      Law
        { lawLhs = do
            storeConfig $ s & SAML.idpId .~ s' ^. SAML.idpId
            storeConfig s',
          lawRhs = do
            storeConfig s',
          lawPrelude = [],
          lawPostlude = [getConfig $ s' ^. SAML.idpId]
        }

prop_storeStoreInterleave ::
  PropConstraints r f =>
  Maybe (f () -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_storeStoreInterleave =
  prepropLaw @'[IdPConfigStore] $ do
    s <- arbitrary
    s' <- arbitrary
    !_ <-
      when (s ^. SAML.idpId == s' ^. SAML.idpId) discard
    pure $
      Law
        { lawLhs = do
            storeConfig s
            storeConfig s',
          lawRhs = do
            storeConfig s'
            storeConfig s,
          lawPrelude = [],
          lawPostlude = [getConfig $ s ^. SAML.idpId, getConfig $ s' ^. SAML.idpId]
        }

prop_storeGet ::
  PropConstraints r f =>
  Maybe (f (Maybe IP.IdP) -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_storeGet =
  prepropLaw @'[IdPConfigStore] $
    do
      s <- arbitrary
      pure $
        simpleLaw
          ( do
              storeConfig s
              getConfig $ s ^. idpId
          )
          ( do
              storeConfig s
              pure (Just s)
          )

prop_deleteGet ::
  PropConstraints r f =>
  Maybe (f (Maybe IP.IdP) -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_deleteGet =
  prepropLaw @'[IdPConfigStore] $ do
    s <- arbitrary
    pure $
      Law
        { lawLhs = do
            deleteConfig s
            getConfig $ s ^. SAML.idpId,
          lawRhs = do
            deleteConfig s
            pure Nothing,
          lawPrelude =
            [ storeConfig s
            ],
          lawPostlude = [] :: [Sem r ()]
        }

prop_deleteDelete ::
  PropConstraints r f =>
  Maybe (f () -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_deleteDelete =
  prepropLaw @'[IdPConfigStore] $ do
    s <- arbitrary
    pure $
      simpleLaw
        ( do
            deleteConfig s
            deleteConfig s
        )
        ( do
            deleteConfig s
        )

prop_storeGetByIssuer ::
  PropConstraints r f =>
  Maybe (f (GetIdPResult IdPId) -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_storeGetByIssuer =
  prepropLaw @'[IdPConfigStore] $
    do
      s <- arbitrary
      pure $
        simpleLaw
          ( do
              storeConfig s
              getIdByIssuerWithoutTeam $ s ^. idpMetadata . edIssuer
          )
          ( do
              storeConfig s
              -- NOT TRUE! This can also return GetIdPNonUnique with nonzero probability!
              pure $ GetIdPFound $ s ^. idpId
          )

prop_setClear ::
  PropConstraints r f =>
  Maybe (f (Maybe (Maybe IdPId)) -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_setClear =
  prepropLaw @'[IdPConfigStore] $
    do
      idp <- arbitrary
      replaced_id <- arbitrary
      let replaced = Replaced replaced_id
      replacing <- arbitrary
      pure $
        Law
          { lawLhs = do
              setReplacedBy replaced replacing
              clearReplacedBy replaced
              getReplacedBy replaced_id,
            lawRhs = do
              clearReplacedBy replaced
              getReplacedBy replaced_id,
            lawPrelude =
              [ storeConfig $ idp & SAML.idpId .~ replaced_id
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
  prepropLaw @'[IdPConfigStore] $
    do
      idpid <- arbitrary
      idp <- arbitrary
      pure $
        Law
          { lawLhs = do
              liftA2 (,) (getConfig idpid) (getConfig idpid),
            lawRhs = do
              cfg <- getConfig idpid
              pure (cfg, cfg),
            lawPrelude =
              [ storeConfig $ idp & SAML.idpId .~ idpid
              ],
            lawPostlude = [] :: [Sem r ()]
          }

prop_getStore ::
  PropConstraints r f =>
  Maybe (f (Maybe IP.IdP) -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_getStore =
  prepropLaw @'[IdPConfigStore] $
    do
      idpid <- arbitrary
      s <- arbitrary
      let s' = s & SAML.idpId .~ idpid
      pure $
        Law
          { lawLhs = do
              r <- getConfig idpid
              maybe (pure ()) storeConfig r
              pure r,
            lawRhs = do
              getConfig idpid,
            lawPrelude =
              [storeConfig s'],
            lawPostlude =
              [getConfig idpid]
          }

prop_setSet ::
  PropConstraints r f =>
  Maybe (f (Maybe (Maybe IdPId)) -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_setSet =
  prepropLaw @'[IdPConfigStore] $
    do
      replaced_id <- arbitrary
      s <- arbitrary
      let s' = s & SAML.idpId .~ replaced_id
      let replaced = Replaced replaced_id
      replacing <- arbitrary
      replacing' <- arbitrary
      pure $
        Law
          { lawLhs = do
              setReplacedBy replaced replacing
              setReplacedBy replaced replacing'
              getReplacedBy replaced_id,
            lawRhs = do
              setReplacedBy replaced replacing'
              getReplacedBy replaced_id,
            lawPrelude =
              [storeConfig s'],
            lawPostlude = [] @(Sem _ ())
          }

prop_setGet ::
  PropConstraints r f =>
  Maybe (f (Maybe (Maybe IdPId)) -> String) ->
  (forall x. Sem r x -> IO (f x)) ->
  Property
prop_setGet =
  prepropLaw @'[IdPConfigStore] $
    do
      idp <- arbitrary
      replaced_id <- arbitrary
      let replaced = Replaced replaced_id
      replacing_id <- arbitrary
      let replacing = Replacing replacing_id
      pure $
        Law
          { lawLhs = do
              setReplacedBy replaced replacing
              getReplacedBy replaced_id,
            lawRhs = do
              setReplacedBy replaced replacing
              (Just replacing_id <$) <$> getConfig replaced_id,
            lawPrelude =
              [ storeConfig $ idp & SAML.idpId .~ replaced_id
              ],
            lawPostlude = [] :: [Sem r ()]
          }
