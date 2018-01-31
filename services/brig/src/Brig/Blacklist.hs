{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Brig.Blacklist
    ( exists
    , insert
    , delete
    ) where

import Brig.AwsAmazonka
import Brig.App
import Brig.Data.UserKey (UserKey, keyText)
import Control.Lens
import Control.Monad.Reader
import Data.Text (Text)
import System.Logger.Class (val, msg, field)

import qualified Data.HashMap.Strict as Map
import qualified Network.AWS.DynamoDB as AmazonkaDdb
import qualified System.Logger.Class as Log

hashKey :: Text
hashKey = "key"

exists :: UserKey -> AppIO Bool
exists (keyText -> key) = do
    t <- view (amazonkaEnv.dynamoBlacklistTable)
    e <- view (amazonkaEnv.amazonkaAwsEnv)
    let v = AmazonkaDdb.attributeValue & AmazonkaDdb.avS .~ (Just key)
    let gi = AmazonkaDdb.getItem t & AmazonkaDdb.giKey .~ item v
                                   & AmazonkaDdb.giConsistentRead .~ (Just True)
    hasItems <$> execAWS e gi
  where
    hasItems :: AmazonkaDdb.GetItemResponse -> Bool
    hasItems = not . Map.null . view AmazonkaDdb.girsItem

insert :: UserKey -> AppIO ()
insert (keyText -> key) = do
    t <- view (amazonkaEnv.dynamoBlacklistTable)
    e <- view (amazonkaEnv.amazonkaAwsEnv)
    let v = AmazonkaDdb.attributeValue & AmazonkaDdb.avS .~ (Just key)
    void $ execAWS e (AmazonkaDdb.putItem t & AmazonkaDdb.piItem .~ item v)

delete :: UserKey -> AppIO ()
delete (keyText -> key) = do
    t <- view (amazonkaEnv.dynamoBlacklistTable)
    e <- view (amazonkaEnv.amazonkaAwsEnv)
    let v = AmazonkaDdb.attributeValue & AmazonkaDdb.avS .~ (Just key)
    void $ execAWS e (AmazonkaDdb.deleteItem t & AmazonkaDdb.diKey .~ item v)

-------------------------------------------------------------------------------
-- Internal

item :: AmazonkaDdb.AttributeValue -> Map.HashMap Text AmazonkaDdb.AttributeValue
item hv = Map.singleton hashKey hv
