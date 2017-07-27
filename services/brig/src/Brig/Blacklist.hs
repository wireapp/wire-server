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
import Brig.App
import Brig.Data.UserKey (UserKey, keyText)
import Control.Error
import Control.Lens (view)
import Control.Monad.Reader
import Data.Text (Text)
import System.Logger.Class (val, msg, field)

import qualified Aws.DynamoDb        as Ddb
import qualified Brig.Aws            as Aws
import qualified System.Logger.Class as Log

hashKey :: Text
hashKey = "key"

exists :: UserKey -> AppIO Bool
exists (Ddb.DString . keyText -> key) =
    maybe False (isJust . Ddb.girItem) <$> exec cmd
  where
    cmd (Aws.BlacklistTable t) = (Ddb.getItem t (Ddb.hk hashKey key))
        { Ddb.giAttrs      = Nothing
        , Ddb.giConsistent = True
        , Ddb.giRetCons    = Ddb.RCNone
        }

insert :: UserKey -> AppIO ()
insert (keyText -> key) = void (exec cmd)
  where
    cmd (Aws.BlacklistTable t) = Ddb.putItem t $ Ddb.item [Ddb.attr hashKey key]

delete :: UserKey -> AppIO ()
delete (Ddb.DString . keyText -> key) = void (exec cmd)
  where
    cmd (Aws.BlacklistTable t) = Ddb.deleteItem t (Ddb.hk hashKey key)

-------------------------------------------------------------------------------
-- Internal

exec :: (Transaction r a, ServiceConfiguration r ~ DdbConfiguration)
     => (Aws.BlacklistTable -> r)
     -> AppIO (Maybe a)
exec mkCmd = do
    cmd <- mkCmd    <$> view (awsConfig.Aws.ddbBlacklistTable)
    rs  <- fmap snd <$> Aws.tryDynamo cmd
    case rs of
        Left ex -> do
          Log.err $ field "error" (show ex)
                  . msg (val "Blacklist: Cannot reach DynamoDB")
          return Nothing
        Right a -> return (Just a)
