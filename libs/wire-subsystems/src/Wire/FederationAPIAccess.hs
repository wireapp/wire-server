{-# LANGUAGE TemplateHaskell #-}

module Wire.FederationAPIAccess where

import Data.Qualified
import Imports
import Polysemy
import Polysemy.Error
import Wire.API.Federation.Client
import Wire.API.Federation.Component
import Wire.API.Federation.Error

data FederationAPIAccess m a where
  RunFederatedEither ::
    KnownComponent c =>
    Remote x ->
    FederatorClient c a ->
    FederationAPIAccess m (Either FederationError a)
  RunFederatedConcurrently ::
    forall (c :: Component) f a m x.
    (KnownComponent c, Foldable f) =>
    f (Remote x) ->
    (Remote x -> FederatorClient c a) ->
    FederationAPIAccess m [Either (Remote x, FederationError) (Remote a)]
  -- | An action similar to 'RunFederatedConcurrently', but the input is
  -- bucketed by domain before the RPCs are sent to the remote backends.
  RunFederatedBucketed ::
    forall (c :: Component) f a m x.
    (KnownComponent c, Foldable f, Functor f) =>
    f (Remote x) ->
    (Remote [x] -> FederatorClient c a) ->
    FederationAPIAccess m [Either (Remote [x], FederationError) (Remote a)]
  IsFederationConfigured :: FederationAPIAccess m Bool

makeSem ''FederationAPIAccess

runFederated ::
  ( Member FederationAPIAccess r,
    Member (Error FederationError) r,
    KnownComponent c
  ) =>
  Remote x ->
  FederatorClient c a ->
  Sem r a
runFederated rx c = runFederatedEither rx c >>= fromEither
