module API.Common where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Array as Array
import System.Random (randomRIO)
import Testlib.Prelude

teamRole :: String -> Int
teamRole "partner" = 1025
teamRole "member" = 1587
teamRole "admin" = 5951
teamRole "owner" = 8191
teamRole bad = error $ "unknown team role: " <> bad

defPassword :: String
defPassword = "hunter2!"

randomEmail :: App String
randomEmail = do
  u <- randomName
  pure $ u <> "@example.com"

randomName :: App String
randomName = liftIO $ do
  n <- randomRIO (8, 15)
  replicateM n pick
  where
    chars :: Array.Array Int Char
    chars = mkArray $ ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9']

    mkArray :: [a] -> Array.Array Int a
    mkArray l = Array.listArray (0, length l - 1) l

    pick :: IO Char
    pick = do
      i <- randomRIO (Array.bounds chars)
      pure (chars Array.! i)
