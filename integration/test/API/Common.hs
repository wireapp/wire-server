module API.Common where

import qualified Data.Array as Array
import Imports
import System.Random (randomRIO)
import TestLib.Prelude

teamRolePartner :: Int
teamRolePartner = 1025

teamRoleMember :: Int
teamRoleMember = 1587

teamRoleAdmin :: Int
teamRoleAdmin = 5951

teamRoleOwner :: Int
teamRoleOwner = 8191

defPassword :: String
defPassword = "hunter2!"

randomEmail :: App String
randomEmail = liftIO $ do
  n <- randomRIO (8, 15)
  u <- replicateM n pick
  pure $ u <> "@example.com"
  where
    chars :: Array.Array Int Char
    chars = mkArray $ ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9']

    mkArray :: [a] -> Array.Array Int a
    mkArray l = Array.listArray (0, length l - 1) l

    pick :: IO Char
    pick = do
      i <- randomRIO (Array.bounds chars)
      pure (chars Array.! i)
