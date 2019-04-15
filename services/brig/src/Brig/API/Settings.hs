module Brig.API.Settings
    ( putSettings
    , getSettings
    ) where

import Imports

import           Brig.API.Handler          (Handler, JSON)
import           Brig.App                  (mutableSettings)
import           Brig.Options              (MutableSettings, MutableSettings')
import           Control.Lens
import           Data.Barbie               (bzipWith)
import           Network.HTTP.Types.Status (status200)
import           Network.Wai               (Response)
import           Network.Wai.Utilities     (JsonRequest, empty, json, parseBody', setStatus)

-- | Update the provided settings accordingly
putSettings :: JsonRequest (MutableSettings' Maybe) -> Handler Response
putSettings body = do
    newSettings <- parseBody' body
    mSettingsVar <- view mutableSettings
    atomically $ do
        mSettings <- readTVar mSettingsVar
        let mergedSettings :: MutableSettings
            mergedSettings = bzipWith fromMaybe' mSettings newSettings
        writeTVar mSettingsVar mergedSettings
    return $ (setStatus status200 empty)
  where
    fromMaybe' :: Identity a -> Maybe a -> Identity a
    fromMaybe' a ma = maybe a Identity ma


-- | Update the provided settings accordingly
getSettings :: JSON -> Handler Response
getSettings _ = do
    mSet <- view mutableSettings >>= readTVarIO
    return . setStatus status200
           $ json mSet
