{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

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
import UnliftIO.Async

import qualified Brig.Data.Blacklist  as Data
import qualified Data.HashMap.Strict  as Map
import qualified Network.AWS.DynamoDB as AWS
import qualified Network.AWS          as AWS

hashKey :: Text
hashKey = "key"

exists :: UserKey -> AppIO Bool
exists key = do
    (fromDyn, fromCas) <- execDyn cmd `concurrently` Data.exists key
    return (hasItems fromDyn || fromCas)
  where
    cmd t = AWS.getItem t & AWS.giKey .~ item (keyText key)
                          & AWS.giConsistentRead ?~ True

    hasItems :: AWS.GetItemResponse -> Bool
    hasItems = not . Map.null . view AWS.girsItem

insert :: UserKey -> AppIO ()
insert key = void $ execDyn cmd `concurrently` Data.insert key
  where
    cmd t = AWS.putItem t & AWS.piItem .~ item (keyText key)

delete :: UserKey -> AppIO ()
delete key = void $ execDyn cmd `concurrently` Data.delete key
  where
    cmd t = AWS.deleteItem t & AWS.diKey .~ item (keyText key)

--------------------------------------------------------------------------------
-- Internal

item :: Text -> Map.HashMap Text AWS.AttributeValue
item key = Map.singleton hashKey $ AWS.attributeValue & AWS.avS ?~ key

execDyn :: (AWS.AWSRequest r) => (Text -> r) -> AppIO (AWS.Rs r)
execDyn mkCmd = do
    cmd <- mkCmd <$> view (awsEnv.blacklistTable)
    env <- view (awsEnv.amazonkaEnv)
    exec env cmd
