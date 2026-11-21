{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneKindSignatures #-}
-- {-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2026 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.FederationConfigSubsystem.InterpreterSpec (spec) where

import Data.Domain
import Data.Map qualified as Map
import GHC.Base (Type)
import Imports
import Polysemy
import Polysemy.State
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Wire.API.Routes.FederationDomainConfig
import Wire.FederationConfigStore qualified as Store
import Wire.FederationConfigSubsystem
import Wire.FederationConfigSubsystem.Interpreter
import Wire.MockInterpreters

type AllEffects :: [(Type -> Type) -> Type -> Type]
type AllEffects =
  [ Store.FederationConfigStore,
    State (Map Domain FederationDomainConfig)
  ]

spec :: Spec
spec = describe "FederationConfigSubsystem.Interpreter" do
  prop "returns no config if it does not exist locally or in db" $
    \(domain :: Domain) ->
      let cfgs = Map.empty
          (_, res) = runAllEffects . interpretFederationConfigSubsystem (Just AllowAll) cfgs $ do
            conf <- getFederationConfig domain
            pure conf
       in res === Nothing
  prop "gets config locally if it exists there" $
    \(domain :: Domain)
     (cfg :: FederationDomainConfig) ->
        let cfgs = Map.singleton domain cfg
            (_, res) = runAllEffects . interpretFederationConfigSubsystem (Just AllowAll) cfgs $ do
              getFederationConfig domain
         in res === Just cfg
  prop "returns changed config after update" $
    \(cfg :: FederationDomainConfig) ->
        let cfgs = Map.empty
            oldConf = cfg {restriction = FederationRestrictionAllowAll}
            domain = oldConf.domain
            newConf = cfg {restriction = FederationRestrictionByTeam []}
            (_, (updateResult, resConf)) = runAllEffects . interpretFederationConfigSubsystem (Just AllowAll) cfgs $ do
              _ <- addFederationConfig oldConf
              updated <- updateFederationConfig newConf
              conf <- getFederationConfig domain
              pure (updated, conf)
         in (updateResult, resConf) =/= (Store.UpdateFederationSuccess, Just oldConf)
              .&. (updateResult, resConf) === (Store.UpdateFederationSuccess, Just newConf)
  prop "gets config from db if it does not exist locally" $
    \(cfg :: FederationDomainConfig) ->
        let cfgs = Map.empty
            (_, (addResult, res)) = runAllEffects . interpretFederationConfigSubsystem (Just AllowAll) cfgs $ do
              actual <- addFederationConfig cfg
              conf <- getFederationConfig cfg.domain
              pure (actual, conf)
         in (addResult, res) === (Store.AddFederationRemoteSuccess, Just cfg)
  where
    runAllEffects ::
      Sem AllEffects a ->
      (Map Domain FederationDomainConfig, a)
    runAllEffects =
      run
        . runState Map.empty
        . inMemoryFederationConfigStoreInterpreter