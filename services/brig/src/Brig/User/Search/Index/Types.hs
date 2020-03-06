{-# LANGUAGE StrictData #-}

module Brig.User.Search.Index.Types where

import Brig.Types.User
import Control.Lens (makeLenses)
import Control.Monad.Catch
import Data.Aeson
import Data.Handle (Handle)
import Data.Id
import Database.V5.Bloodhound hiding (key)
import Imports

data IndexUpdate
  = IndexUpdateUser IndexUser
  | IndexUpdateUsers [IndexUser]
  | IndexDeleteUser UserId

-- | Represents the ES *index*, ie. the attributes of a user searchable in ES.  See also:
-- 'UserDoc'.
data IndexUser
  = IndexUser
      { _iuUserId :: UserId,
        _iuVersion :: IndexVersion,
        _iuTeamId :: Maybe TeamId,
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

-- | Represents an ES *document*, ie. the subset of user attributes stored in ES.
-- See also 'IndexUser'.
--
-- If a user is not searchable, e.g. because the account got
-- suspended, all fields except for the user id are set to 'Nothing' and
-- consequently removed from the index.
data UserDoc
  = UserDoc
      { udId :: UserId,
        udTeamId :: Maybe TeamId,
        udName :: Maybe Name,
        udNormalized :: Maybe Text,
        udHandle :: Maybe Handle,
        udColourId :: Maybe ColourId
      }
  deriving (Eq, Show)

instance ToJSON UserDoc where
  toJSON ud =
    object
      [ "id" .= udId ud,
        "team_id" .= udTeamId ud,
        "name" .= udName ud,
        "normalized" .= udNormalized ud,
        "handle" .= udHandle ud,
        "accent_id" .= udColourId ud
      ]

instance FromJSON UserDoc where
  parseJSON = withObject "UserDoc" $ \o ->
    UserDoc <$> o .: "id"
      <*> o .:? "team_id"
      <*> o .:? "name"
      <*> o .:? "normalized"
      <*> o .:? "handle"
      <*> o .:? "accent_id"

makeLenses ''IndexUser

mkIndexVersion :: (MonadThrow m, Integral a) => a -> m IndexVersion
mkIndexVersion i =
  if i > fromIntegral (maxBound :: Int)
    then throwM $ IndexError "Index overflow"
    else pure . IndexVersion . fromMaybe maxBound . mkDocVersion . fromIntegral $ i

mkIndexUser :: UserId -> IndexVersion -> IndexUser
mkIndexUser u v =
  IndexUser
    { _iuUserId = u,
      _iuVersion = v,
      _iuTeamId = Nothing,
      _iuName = Nothing,
      _iuHandle = Nothing,
      _iuColourId = Nothing
    }
