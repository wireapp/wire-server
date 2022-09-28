module ClientsWithoutPrekeys.Data where

import qualified Data.Aeson as JSON
import Data.Id
import Imports

data UserClient = UserClient
  { uid :: UserId,
    cid :: ClientId
  }
  deriving (Read, Show, Eq, Ord)

instance JSON.FromJSON UserClient where
  parseJSON = JSON.withObject "UserClient" $ \v ->
    UserClient
      <$> v JSON..: "user"
      <*> v JSON..: "client"
