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

module Spar.Orphans where

import Data.Aeson
import Data.CaseInsensitive
import Data.Id
import Data.String.Conversions
import GHC.Generics
import Servant hiding (URI)
import Spar.Types


instance FromHttpApiData UserId where
  parseUrlPiece = fmap Id . parseUrlPiece

instance ToHttpApiData UserId where
  toUrlPiece = toUrlPiece . show

deriving instance Generic ServantErr
instance FromJSON ServantErr
instance ToJSON ServantErr

instance MimeRender PlainText Void where
  mimeRender _ = error "instance MimeRender HTML Void: impossible"

instance FromJSON (CI SBS) where parseJSON = fmap mk . parseJSON
instance ToJSON (CI SBS)   where toJSON = toJSON . original

instance FromJSON SBS      where parseJSON = fmap (cs @ST @SBS) . parseJSON
instance ToJSON SBS        where toJSON = toJSON . cs @SBS @ST

instance FromJSON LBS      where parseJSON = fmap (cs @ST @LBS) . parseJSON
instance ToJSON LBS        where toJSON = toJSON . cs @LBS @ST
