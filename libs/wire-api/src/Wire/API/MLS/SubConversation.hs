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

import Control.Applicative
import Control.Lens (makePrisms, (?~))
import Control.Lens.Tuple (_1)
import Control.Monad.Except
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as A
import Data.ByteString.Conversion
import Data.Id
import Data.OpenApi qualified as S
import Data.Qualified
import Data.Schema hiding (HasField)
import Data.Text qualified as T
import GHC.Records
import Imports
import Servant (FromHttpApiData (..), ToHttpApiData (toQueryParam))
import Test.QuickCheck
import Wire.API.Conversation.Protocol
import Wire.API.MLS.Credential
import Wire.API.MLS.Group
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned
import Wire.Arbitrary

-- | An MLS subconversation ID, which identifies a subconversation within a
-- conversation. The pair of a qualified conversation ID and a subconversation
-- ID identifies globally.
newtype SubConvId = SubConvId {unSubConvId :: Text}
  deriving newtype (Eq, ToSchema, Ord, S.ToParamSchema, ToByteString, ToJSON, FromJSON, S.ToSchema)
  deriving stock (Generic)
  deriving stock (Show)

instance FromHttpApiData SubConvId where
  parseQueryParam s = do
    unless (T.length s > 0) $ throwError "The subconversation ID cannot be empty"
    unless (T.length s < 256) $ throwError "The subconversation ID cannot be longer than 255 characters"
    unless (T.all isValidSubConvChar s) $ throwError "The subconversation ID contains invalid characters"
    pure (SubConvId s)

instance ToHttpApiData SubConvId where
  toQueryParam = unSubConvId

instance Arbitrary SubConvId where
  arbitrary = do
    n <- choose (1, 255)
    cs <- replicateM n (arbitrary `suchThat` isValidSubConvChar)
    pure $ SubConvId (T.pack cs)

isValidSubConvChar :: Char -> Bool
isValidSubConvChar c = isPrint c && isAscii c && not (isSpace c)

data PublicSubConversation = PublicSubConversation
  { pscParentConvId :: Qualified ConvId,
    pscSubConvId :: SubConvId,
    pscGroupId :: GroupId,
    pscActiveData :: Maybe ActiveMLSConversationData,
    pscMembers :: [ClientIdentity]
  }
  deriving (Eq, Show)
  deriving (A.ToJSON, A.FromJSON, S.ToSchema) via (Schema PublicSubConversation)

publicSubConversationSchema :: Maybe Version -> ValueSchema NamedSwaggerDoc PublicSubConversation
publicSubConversationSchema v =
  objectWithDocModifier
    ("PublicSubConversation" <> foldMap (T.toUpper . versionText) v)
    (description ?~ "An MLS subconversation")
    $ PublicSubConversation
      <$> pscParentConvId .= field "parent_qualified_id" schema
      <*> pscSubConvId .= field "subconv_id" schema
      <*> pscGroupId .= field "group_id" schema
      <*> pscActiveData .= optionalActiveMLSConversationDataSchema v
      <*> pscMembers .= field "members" (array schema)

instance ToSchema PublicSubConversation where
  schema = publicSubConversationSchema Nothing

instance ToSchema (Versioned 'V5 PublicSubConversation) where
  schema = Versioned <$> unVersioned .= publicSubConversationSchema (Just V5)

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

instance HasField "conv" (ConvOrSubChoice c s) c where
  getField (Conv c) = c
  getField (SubConv c _) = c

instance HasField "subconv" (ConvOrSubChoice c s) (Maybe s) where
  getField (Conv _) = Nothing
  getField (SubConv _ s) = Just s

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
