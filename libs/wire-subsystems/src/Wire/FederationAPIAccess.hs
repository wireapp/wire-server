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
