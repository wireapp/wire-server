{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Brig.Blacklist
    ( exists
    , insert
    , delete
    ) where

import Brig.AWS
import Brig.App
import Brig.Data.UserKey (UserKey, keyText)
import Control.Lens
import Control.Monad.Reader
import Data.Text (Text)
import System.Logger.Class (val, msg, field)

import qualified Data.HashMap.Strict  as Map
import qualified Network.AWS.DynamoDB as AWS
import qualified Network.AWS          as AWS
import qualified System.Logger.Class  as Log

hashKey :: Text
hashKey = "key"

-- TODO: If dynamo is not reachable, we return false... probably not a good idea?
exists :: UserKey -> AppIO Bool
exists (keyText -> key) = hasItems <$> go cmd
  where
    cmd t = AWS.getItem t & AWS.giKey .~ item key
                          & AWS.giConsistentRead ?~ True

    hasItems :: AWS.GetItemResponse -> Bool
    hasItems = not . Map.null . view AWS.girsItem

insert :: UserKey -> AppIO ()
insert (keyText -> key) = void $ go cmd
  where
    cmd t = AWS.putItem t & AWS.piItem .~ item key
 
delete :: UserKey -> AppIO ()
delete (keyText -> key) = void $ go cmd
  where
    cmd t = AWS.deleteItem t & AWS.diKey .~ item key

--------------------------------------------------------------------------------
-- Internal

item :: Text -> Map.HashMap Text AWS.AttributeValue
item key = Map.singleton hashKey $ AWS.attributeValue & AWS.avS ?~ key

go :: (AWS.AWSRequest r) => (Text -> r) -> AppIO (AWS.Rs r)
go mkCmd = do
    cmd <- mkCmd <$> view (awsEnv.blacklistTable)
    env <- view (awsEnv.amazonkaEnv)
    exec env cmd
