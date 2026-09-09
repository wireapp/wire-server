{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2023 Wire Swiss GmbH <opensource@wire.com>
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

module Test.SystemSettings where

import API.Brig
import SetupHelpers
import Testlib.Prelude

testGetSettingsPublic ::
  (HasCallStack) =>
  Tagged "nomad-profiles" (Maybe Bool) ->
  Tagged "restrict-user-creation" (Maybe Bool) ->
  App ()
testGetSettingsPublic (MkTagged nomadProfiles) (MkTagged restrictUserCreation) =
  withModifiedBackend def {brigCfg = setFields} \domain -> do
    getSystemSettingsPublic domain `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      lookupField resp.json "nomadProfiles" `shouldMatch` nomadProfiles
      resp.json %. "setRestrictUserCreation" `shouldMatch` fromMaybe False restrictUserCreation
  where
    setFields =
      maybe (removeField "optSettings.setNomadProfiles") (setField "optSettings.setNomadProfiles") nomadProfiles
        >=> maybe (removeField "optSettings.setRestrictUserCreation") (setField "optSettings.setRestrictUserCreation") restrictUserCreation

testGetSettingsInternal :: (HasCallStack) => Tagged "enabled-mls" (Maybe Bool) -> App ()
testGetSettingsInternal (MkTagged enableMls) = do
  let conf = def {brigCfg = maybe (removeField "optSettings.setEnableMLS") (setField "optSettings.setEnableMLS") enableMls}
  withModifiedBackend conf \domain -> do
    user <- randomUser domain def
    getSystemSettingsInternal user `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      resp.json %. "setEnableMls" `shouldMatch` fromMaybe False enableMls

testGetSettingsInternalSsoIdpChangeDetection ::
  (HasCallStack) =>
  Tagged "sso-idp-change-detection" (Maybe Bool) ->
  App ()
testGetSettingsInternalSsoIdpChangeDetection (MkTagged multiIngress) = do
  let conf =
        def
          { brigCfg =
              maybe
                (removeField "optSettings.setSsoIdpChangeDetectionInputs")
                (setField "optSettings.setSsoIdpChangeDetectionInputs" . sparInputs)
                multiIngress
          }
  withModifiedBackend conf \domain -> do
    user <- randomUser domain def
    getSystemSettingsInternal user `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      -- Brig derives the flag in Haskell from the raw inputs: true iff both the
      -- multi-ingress domain configs and the IdP cert fingerprint allowlist are
      -- non-empty (Brig.Options.deriveSsoIdpChangeDetectionEnabled); absent
      -- inputs mean disabled. The tag enumerates Nothing/Just False/Just True
      -- (GEnum), so `fromMaybe` — not `fromJust`.
      resp.json %. "ssoIdpChangeDetectionEnabled" `shouldMatch` fromMaybe False multiIngress
    getSystemSettingsPublic domain `bindResponse` \resp -> do
      resp.status `shouldMatchInt` 200
      lookupField resp.json "ssoIdpChangeDetectionEnabled" `shouldMatch` (Nothing :: Maybe Value)

-- | Minimal mirror of @spar.config@: 'True' is a multi-ingress spar (non-empty
-- @domainConfigs@ and allowlist); 'False' is single-ingress spar (both empty).
sparInputs :: Bool -> Value
sparInputs multiIngress =
  object
    [ "multiIngressDomainConfigs"
        .= if multiIngress then object ["example.com" .= object []] else object [],
      "idpCertFingerprintAllowlist"
        .= if multiIngress then ["00:11:22:33:44:55:66:77:88:99:AA:BB:CC:DD:EE:FF:00:11:22:33" :: String] else ([] :: [String])
    ]
