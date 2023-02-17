{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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

module Wire.API.MLS.SubConversation where

import Control.Lens (makePrisms)
import Control.Lens.Tuple (_1)
import Control.Monad.Except
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Id
import Data.Schema
import qualified Data.Swagger as S
import qualified Data.Text as T
import Imports
import Servant (FromHttpApiData (..), ToHttpApiData (toQueryParam))
import Test.QuickCheck
import Wire.Arbitrary

-- | An MLS subconversation ID, which identifies a subconversation within a
-- conversation. The pair of a qualified conversation ID and a subconversation
-- ID identifies globally.
newtype SubConvId = SubConvId {unSubConvId :: Text}
  deriving newtype (Eq, ToSchema, Ord)
  deriving stock (Generic)
  deriving (Arbitrary) via (GenericUniform SubConvId)
  deriving newtype (S.ToParamSchema)
  deriving stock (Show)

instance FromHttpApiData SubConvId where
  parseQueryParam s = do
    unless (T.length s > 0) $ throwError "The subconversation ID cannot be empty"
    unless (T.all isValid s) $ throwError "The subconversation ID contains invalid characters"
    pure (SubConvId s)
    where
      isValid c = isPrint c && isAscii c && not (isSpace c)

instance ToHttpApiData SubConvId where
  toQueryParam = unSubConvId

data ConvOrSubTag = ConvTag | SubConvTag
  deriving (Eq, Enum, Bounded)

data ConvOrSubChoice c s
  = Conv c
  | SubConv c s

deriving instance (Eq c, Eq s) => Eq (ConvOrSubChoice c s)

deriving instance (Show c, Show s) => Show (ConvOrSubChoice c s)

deriving instance (Generic c, Generic s) => Generic (ConvOrSubChoice c s)

deriving via
  (GenericUniform (ConvOrSubChoice c s))
  instance
    (Generic c, Generic s, Arbitrary c, Arbitrary s) => Arbitrary (ConvOrSubChoice c s)

type ConvOrSubConvId = ConvOrSubChoice ConvId SubConvId

makePrisms ''ConvOrSubChoice

instance ToSchema ConvOrSubConvId where
  schema =
    object "ConvOrSubConvId" $
      fromTagged
        <$> toTagged
          .= bind
            (fst .= field "tag" tagSchema)
            (snd .= fieldOver _1 "value" untaggedSchema)
    where
      toTagged :: ConvOrSubConvId -> (ConvOrSubTag, ConvOrSubConvId)
      toTagged c@(Conv _) = (ConvTag, c)
      toTagged c@(SubConv _ _) = (SubConvTag, c)

      fromTagged :: (ConvOrSubTag, ConvOrSubConvId) -> ConvOrSubConvId
      fromTagged = snd

      untaggedSchema = dispatch $ \case
        ConvTag ->
          tag
            _Conv
            (unnamed $ object "" $ field "conv_id" schema)
        SubConvTag ->
          tag
            _SubConv
            ( unnamed $
                object "" $
                  ( (,)
                      <$> fst .= field "conv_id" schema
                      <*> snd .= field "subconv_id" schema
                  )
            )

      tagSchema :: ValueSchema NamedSwaggerDoc ConvOrSubTag
      tagSchema =
        enum @Text "ConvOrSubTag" $
          mconcat
            [ element "conv" ConvTag,
              element "subconv" SubConvTag
            ]

deriving via Schema ConvOrSubConvId instance FromJSON ConvOrSubConvId

deriving via Schema ConvOrSubConvId instance ToJSON ConvOrSubConvId

deriving via Schema ConvOrSubConvId instance S.ToSchema ConvOrSubConvId
