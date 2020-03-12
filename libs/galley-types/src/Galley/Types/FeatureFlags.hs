{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Galley.Types.FeatureFlags
  ( FeatureFlags (..),
    flagSSO,
    flagLegalHold,
    flagFederation,
    FeatureSSO (..),
    FeatureLegalHold (..),
    FeatureFederation (..),
  )
where

import Control.Lens (makeLenses)
import Data.Aeson
import Data.String.Conversions (cs)
import Imports

data FeatureFlags
  = FeatureFlags
      { _flagSSO :: !FeatureSSO,
        _flagLegalHold :: !FeatureLegalHold,
        _flagFederation :: !FeatureFederation
      }
  deriving (Eq, Show, Generic)

data FeatureSSO
  = FeatureSSOEnabledByDefault
  | FeatureSSODisabledByDefault
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

data FeatureLegalHold
  = FeatureLegalHoldDisabledPermanently
  | FeatureLegalHoldDisabledByDefault
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

data FeatureFederation
  = FeatureFederationDisabled
  | FeatureFederationEnabledExperimental
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance FromJSON FeatureFlags where
  parseJSON = withObject "FeatureFlags" $ \obj ->
    FeatureFlags
      <$> (obj .: "sso")
      <*> (obj .: "legalhold")
      <*> (obj .: "federation")

instance ToJSON FeatureFlags where
  toJSON (FeatureFlags sso legalhold federation) =
    object $
      [ "sso" .= sso,
        "legalhold" .= legalhold,
        "federation" .= federation
      ]

instance FromJSON FeatureSSO where
  parseJSON (String "enabled-by-default") = pure FeatureSSOEnabledByDefault
  parseJSON (String "disabled-by-default") = pure FeatureSSODisabledByDefault
  parseJSON bad = fail $ "FeatureSSO: " <> cs (encode bad)

instance ToJSON FeatureSSO where
  toJSON FeatureSSOEnabledByDefault = String "enabled-by-default"
  toJSON FeatureSSODisabledByDefault = String "disabled-by-default"

instance FromJSON FeatureLegalHold where
  parseJSON (String "disabled-permanently") = pure $ FeatureLegalHoldDisabledPermanently
  parseJSON (String "disabled-by-default") = pure $ FeatureLegalHoldDisabledByDefault
  parseJSON bad = fail $ "FeatureLegalHold: " <> cs (encode bad)

instance ToJSON FeatureLegalHold where
  toJSON FeatureLegalHoldDisabledPermanently = String "disabled-permanently"
  toJSON FeatureLegalHoldDisabledByDefault = String "disabled-by-default"

instance FromJSON FeatureFederation where
  parseJSON (String "disabled") = pure $ FeatureFederationDisabled
  parseJSON (String "enabled-experimental") = pure $ FeatureFederationEnabledExperimental
  parseJSON bad = fail $ "FeatureFederation: " <> cs (encode bad)

instance ToJSON FeatureFederation where
  toJSON FeatureFederationDisabled = String "disabled"
  toJSON FeatureFederationEnabledExperimental = String "enabled-experimental"

makeLenses ''FeatureFlags
