{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Spar.API.Instances where

import Data.Aeson
import Data.Aeson.Types
import Data.CaseInsensitive
import Data.Id
import Data.String.Conversions
import GHC.Generics
import SAML2.WebSSO.Types
import SAML2.WebSSO.XML as SAML
import Servant hiding (URI)
import Spar.Types


instance MimeRender PlainText Void where
  mimeRender _ = error "instance MimeRender HTML Void: impossible"

instance FromHttpApiData (Id a) where
  parseUrlPiece = fmap Id . parseUrlPiece

instance ToHttpApiData (Id a) where
  toUrlPiece = toUrlPiece . show

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
