{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Brig.Blacklist
    ( exists
    , insert
    , delete
    ) where

import Aws.Core (Transaction, ServiceConfiguration)
import Aws.DynamoDb (DdbConfiguration)
import Brig.AwsAmazonka
import Brig.App
import Brig.Data.UserKey (UserKey, keyText)
import Control.Error
import Control.Lens
import Control.Monad.Reader
import Data.Text (Text)
import System.Logger.Class (val, msg, field)

import qualified Aws.DynamoDb        as Ddb
import qualified Brig.Aws            as Aws
import qualified Data.HashMap.Strict as Map
import qualified Network.AWS.DynamoDB as AmazonkaDdb
import qualified System.Logger.Class as Log

hashKey :: Text
hashKey = "key"

-- TODO: Check consistency flags used

exists :: UserKey -> AppIO Bool
exists (keyText -> key) = do
    t <- view (amazonkaEnv.dynamoBlacklistTable)
    e <- view (amazonkaEnv.amazonkaAwsEnv)
    let v = AmazonkaDdb.attributeValue & AmazonkaDdb.avS .~ (Just key)
    let gi = AmazonkaDdb.getItem t & AmazonkaDdb.giKey .~ item v
                                   & AmazonkaDdb.giConsistentRead .~ (Just True)

    handleResponse =<< execAWS e gi return
  where
    handleResponse :: AmazonkaDdb.GetItemResponse -> AppIO Bool
    handleResponse r = do
       let items = view AmazonkaDdb.girsItem r
       return . not $ Map.null items

    item :: AmazonkaDdb.AttributeValue -> Map.HashMap Text AmazonkaDdb.AttributeValue
    item hv = Map.singleton hashKey hv

insert :: UserKey -> AppIO ()
insert (keyText -> key) = do
    t <- view (amazonkaEnv.dynamoBlacklistTable)
    e <- view (amazonkaEnv.amazonkaAwsEnv)
    let v = AmazonkaDdb.attributeValue & AmazonkaDdb.avS .~ (Just key)
    _ <- execAWS e (AmazonkaDdb.putItem t & AmazonkaDdb.piItem .~ item v) return
    return ()
  where
    item :: AmazonkaDdb.AttributeValue -> Map.HashMap Text AmazonkaDdb.AttributeValue
    item hv = Map.singleton hashKey hv

delete :: UserKey -> AppIO ()
delete (keyText -> key) = do
    t <- view (amazonkaEnv.dynamoBlacklistTable)
    e <- view (amazonkaEnv.amazonkaAwsEnv)
    let v = AmazonkaDdb.attributeValue & AmazonkaDdb.avS .~ (Just key)
    _ <- execAWS e (AmazonkaDdb.deleteItem t & AmazonkaDdb.diKey .~ item v) return
    return ()
  where
    item :: AmazonkaDdb.AttributeValue -> Map.HashMap Text AmazonkaDdb.AttributeValue
    item hv = Map.singleton hashKey hv

-------------------------------------------------------------------------------
-- Internal
