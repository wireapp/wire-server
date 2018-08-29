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

import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.CaseInsensitive
import Data.Id
import Data.String.Conversions
import Data.Time
import GHC.Generics
import SAML2.WebSSO.Types
import SAML2.WebSSO.XML
import Servant hiding (URI)
import Text.XML.Util (parseURI')
import URI.ByteString


instance FromHttpApiData URI where
  parseUrlPiece = either (fail . show) pure . parseURI' <=< parseUrlPiece

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

instance ToHttpApiData IdPId where
  toUrlPiece (IdPId uuid) = toUrlPiece uuid

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

instance FromJSON AuthnResponse
instance ToJSON AuthnResponse

instance FromJSON Status
instance ToJSON Status

instance FromJSON Assertion
instance ToJSON Assertion

instance FromJSON SubjectAndStatements
instance ToJSON SubjectAndStatements

instance FromJSON Subject
instance ToJSON Subject

instance FromJSON SubjectConfirmation
instance ToJSON SubjectConfirmation

instance FromJSON SubjectConfirmationMethod
instance ToJSON SubjectConfirmationMethod

instance FromJSON SubjectConfirmationData
instance ToJSON SubjectConfirmationData

instance FromJSON IP
instance ToJSON IP

instance FromJSON Statement
instance ToJSON Statement

instance FromJSON Locality
instance ToJSON Locality

instance FromJSON Attribute
instance ToJSON Attribute

instance FromJSON AttributeValue
instance ToJSON AttributeValue

instance FromJSON (ID a)
instance ToJSON (ID a)

instance FromJSON NameID
instance ToJSON NameID

instance FromJSON UnqualifiedNameID
instance ToJSON UnqualifiedNameID

instance FromJSON Time
instance ToJSON Time

instance FromJSON Conditions
instance ToJSON Conditions

deriving instance Generic ServantErr
instance FromJSON ServantErr
instance ToJSON ServantErr

instance FromJSON (CI SBS) where parseJSON = fmap mk . parseJSON
instance ToJSON (CI SBS)   where toJSON = toJSON . original

instance FromJSON SBS      where parseJSON = fmap (cs @ST @SBS) . parseJSON
instance ToJSON SBS        where toJSON = toJSON . cs @SBS @ST

instance FromJSON LBS      where parseJSON = fmap (cs @ST @LBS) . parseJSON
instance ToJSON LBS        where toJSON = toJSON . cs @LBS @ST
