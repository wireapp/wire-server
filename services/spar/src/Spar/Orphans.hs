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

import Data.Id
import Servant (FromHttpApiData(..), ToHttpApiData(..), MimeRender(..), PlainText)
import Spar.Types


instance FromHttpApiData UserId where
  parseUrlPiece = fmap Id . parseUrlPiece

instance ToHttpApiData UserId where
  toUrlPiece = toUrlPiece . show

instance MimeRender PlainText Void where
  mimeRender _ = error "instance MimeRender HTML Void: impossible"
