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
import qualified Network.AWS.DynamoDB as Aws
import qualified System.Logger.Class  as Log

hashKey :: Text
hashKey = "key"

-- TODO: If dynamo is not reachable, we return false... probably not a good idea?
exists :: UserKey -> AppIO Bool
exists (keyText -> key) = do
    t <- view (awsEnv.dynamoBlacklistTable)
    e <- view (awsEnv.amazonkaAwsEnv)
    let v = Aws.attributeValue & Aws.avS .~ (Just key)
    let gi = Aws.getItem t & Aws.giKey .~ item v
                           & Aws.giConsistentRead .~ (Just True)
    hasItems <$> exec e gi
  where
    hasItems :: Aws.GetItemResponse -> Bool
    hasItems = not . Map.null . view Aws.girsItem

insert :: UserKey -> AppIO ()
insert (keyText -> key) = do
    t <- view (awsEnv.dynamoBlacklistTable)
    e <- view (awsEnv.amazonkaAwsEnv)
    let v = Aws.attributeValue & Aws.avS .~ (Just key)
    void $ exec e (Aws.putItem t & Aws.piItem .~ item v)

delete :: UserKey -> AppIO ()
delete (keyText -> key) = do
    t <- view (awsEnv.dynamoBlacklistTable)
    e <- view (awsEnv.amazonkaAwsEnv)
    let v = Aws.attributeValue & Aws.avS .~ (Just key)
    void $ exec e (Aws.deleteItem t & Aws.diKey .~ item v)

-------------------------------------------------------------------------------
-- Internal

item :: Aws.AttributeValue -> Map.HashMap Text Aws.AttributeValue
item hv = Map.singleton hashKey hv

-- exec mkCmd = do
--     cmd <- mkCmd    <$> view (awsConfig.Aws.ddbBlacklistTable)
--     rs  <- fmap snd <$> Aws.tryDynamo cmd
--     case rs of
--         Left ex -> do
--           Log.err $ field "error" (show ex)
--                   . msg (val "Blacklist: Cannot reach DynamoDB")
--           return Nothing
--         Right a -> return (Just a)

-- exec = do
--     t <- view (awsEnv.dynamoBlacklistTable)
--     e <- view (awsEnv.amazonkaAwsEnv)
