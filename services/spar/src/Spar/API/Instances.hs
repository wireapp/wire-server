{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Spar.API.Instances where

import Data.Aeson
import Data.Aeson.Types
import Data.CaseInsensitive
import Data.Id
import Data.String.Conversions
import Data.Time
import GHC.Generics
import SAML2.WebSSO.Types
import SAML2.WebSSO.XML
import Servant

instance FromHttpApiData UserId where
  parseUrlPiece = fmap Id . parseUrlPiece

instance ToHttpApiData UserId where
  toUrlPiece = toUrlPiece . show

instance FromHttpApiData (ID a) where
  parseUrlPiece = fmap ID . parseUrlPiece

instance ToHttpApiData (ID a) where
  toUrlPiece = toUrlPiece . renderID

instance FromHttpApiData Time where
  parseUrlPiece st =
    fmap Time . parseTimeM True defaultTimeLocale timeFormat =<< parseUrlPiece @String st

instance ToHttpApiData Time where
  toUrlPiece =
    toUrlPiece . formatTime defaultTimeLocale timeFormat . fromTime

instance ToJSON UserRef where
  toJSON (UserRef tenant subject) =
    object ["tenant" .= encodeElem tenant, "subject" .= encodeElem subject]

instance FromJSON UserRef where
  parseJSON = withObject "UserRef" $ \obj -> do
    _uidTenant  <- unpack =<< obj .: "tenant"
    _uidSubject <- unpack =<< obj .: "subject"
    pure UserRef {..}
    where
      unpack :: HasXML a => ST -> Parser a
      unpack = either fail pure . decodeElem . cs

instance FromJSON AccessVerdict
instance ToJSON AccessVerdict

deriving instance Generic ServantErr

instance FromJSON ServantErr
instance ToJSON ServantErr

instance FromJSON (CI SBS) where parseJSON = fmap mk . parseJSON
instance ToJSON (CI SBS)   where toJSON = toJSON . original

instance FromJSON SBS      where parseJSON = fmap (cs @ST @SBS) . parseJSON
instance ToJSON SBS        where toJSON = toJSON . cs @SBS @ST

instance FromJSON LBS      where parseJSON = fmap (cs @ST @LBS) . parseJSON
instance ToJSON LBS        where toJSON = toJSON . cs @LBS @ST
