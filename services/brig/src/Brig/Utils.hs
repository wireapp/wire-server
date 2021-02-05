module Brig.Utils where

import Data.Aeson (FromJSON)
import Imports
import Util.Options (FilePathSecrets, loadSecret)

initCredentials :: (FromJSON a) => FilePathSecrets -> IO a
initCredentials secretFile = do
  dat <- loadSecret secretFile
  return $ either (\e -> error $ "Could not load secrets from " ++ show secretFile ++ ": " ++ e) id dat
