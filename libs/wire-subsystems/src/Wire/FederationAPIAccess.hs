{-# LANGUAGE TemplateHaskell #-}

module Wire.FederationAPIAccess where

import Data.Kind
import Data.Qualified
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Federation.Component
import Wire.API.Federation.Error

data FederationAPIAccess (fedM :: Component -> Type -> Type) m a where
  RunFederatedEither ::
    (KnownComponent c) =>
    Remote x ->
    fedM c a ->
    FederationAPIAccess fedM m (Either FederationError a)
  RunFederatedConcurrently ::
    forall (c :: Component) f a m x fedM.
    (KnownComponent c, Foldable f) =>
    f (Remote x) ->
    (Remote x -> fedM c a) ->
    FederationAPIAccess fedM m [Either (Remote x, FederationError) (Remote a)]
  -- | An action similar to 'RunFederatedConcurrently', but the input is
  -- bucketed by domain before the RPCs are sent to the remote backends.
  RunFederatedBucketed ::
    forall (c :: Component) f a m x fedM.
    (KnownComponent c, Foldable f, Functor f) =>
    f (Remote x) ->
    (Remote [x] -> fedM c a) ->
    FederationAPIAccess fedM m [Either (Remote [x], FederationError) (Remote a)]
  IsFederationConfigured :: FederationAPIAccess fedM m Bool

makeSem ''FederationAPIAccess

runFederated ::
  forall c fedM x a r.
  ( Member (FederationAPIAccess fedM) r,
    Member (Error FederationError) r,
    KnownComponent c
  ) =>
  Remote x ->
  fedM c a ->
  Sem r a
runFederated rx c = runFederatedEither rx c >>= fromEither
