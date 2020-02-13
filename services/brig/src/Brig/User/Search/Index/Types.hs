{-# LANGUAGE StrictData #-}

module Brig.User.Search.Index.Types where

import Brig.Types.User
import Control.Lens (makeLenses)
import Data.Aeson
import Data.Id
import Data.Json.Util ((#))
import Database.V5.Bloodhound hiding (key)
import Imports

data IndexUpdate
  = IndexUpdateUser IndexUser
  | IndexUpdateUsers [IndexUser]
  | IndexDeleteUser UserId

data IndexUser
  = IndexUser
      { _iuUserId :: UserId,
        _iuVersion :: IndexVersion,
        _iuName :: Maybe Name,
        _iuHandle :: Maybe Handle,
        _iuColourId :: Maybe ColourId
      }

data IndexQuery r = IndexQuery Query Filter

data IndexError
  = IndexUpdateError EsError
  | IndexLookupError EsError
  | IndexError Text
  deriving (Show)

instance Exception IndexError

newtype IndexVersion = IndexVersion {docVersion :: DocVersion}

-- | Represents the searchable subset of a user record, as indexed by ES
--
-- If a user is not searchable, e.g. because she opted out or the account got
-- suspended, all fields except for the user id are set to 'Nothing' and
-- consequently removed from the index.
data UserDoc
  = UserDoc
      { udId :: UserId,
        udName :: Maybe Text,
        udNormalized :: Maybe Text,
        udHandle :: Maybe Text,
        udColourId :: Maybe ColourId
      }

instance ToJSON UserDoc where
  toJSON ud =
    object $
      "id" .= udId ud
        # "name" .= udName ud
        # "normalized" .= udNormalized ud
        # "handle" .= udHandle ud
        # "accent_id" .= udColourId ud
        # []

instance FromJSON UserDoc where
  parseJSON = withObject "UserDoc" $ \o ->
    UserDoc <$> o .: "id"
      <*> o .:? "name"
      <*> o .:? "normalized"
      <*> o .:? "handle"
      <*> o .:? "accent_id"

makeLenses ''IndexUser

-- FIXME: warn on overflow
mkIndexVersion :: Integral a => a -> IndexVersion
mkIndexVersion = IndexVersion . fromMaybe maxBound . mkDocVersion . fromIntegral

mkIndexUser :: UserId -> IndexVersion -> IndexUser
mkIndexUser u v =
  IndexUser
    { _iuUserId = u,
      _iuVersion = v,
      _iuName = Nothing,
      _iuHandle = Nothing,
      _iuColourId = Nothing
    }
