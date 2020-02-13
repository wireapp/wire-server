{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Miscellaneous orphan instances needed for Spar.
module Spar.Orphans
  (
  )
where

import Data.Id
import Imports
import Servant (FromHttpApiData (..), MimeRender (..), PlainText, ToHttpApiData (..))

instance FromHttpApiData (Id a) where
  parseUrlPiece = fmap Id . parseUrlPiece

instance ToHttpApiData (Id a) where
  toUrlPiece = toUrlPiece . show

instance MimeRender PlainText Void where
  mimeRender _ = error "instance MimeRender HTML Void: impossible"
