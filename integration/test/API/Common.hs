module API.Common where

import Control.Monad
import Control.Monad.IO.Class
import Data.Array ((!))
import qualified Data.Array as Array
import qualified Data.ByteString as BS
import Data.Scientific (scientific)
import qualified Data.Vector as Vector
import System.Random (randomIO, randomRIO)
import Testlib.Prelude

-- | please don't use special shell characters like '!' here.  it makes writing shell lines
-- that use test data a lot less straight-forward.
defPassword :: String
defPassword = "hunter2."

randomEmail :: App String
randomEmail = do
  u <- randomName
  pure $ u <> "@example.com"

randomDomain :: App String
randomDomain = do
  u <- randomName
  pure $ (fmap toLower u) <> ".example"

randomExternalId :: App String
randomExternalId = liftIO $ do
  -- external ID has no constraints, but we only generate human-readable samples
  n <- randomRIO (8, 15)
  replicateM n pick
  where
    chars = mkArray $ ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9']
    pick = (chars !) <$> randomRIO (Array.bounds chars)

randomName :: App String
randomName = liftIO $ do
  n <- randomRIO (8, 15)
  replicateM n pick
  where
    chars = mkArray $ ['A' .. 'Z'] <> ['a' .. 'z'] <> ['0' .. '9']
    pick = (chars !) <$> randomRIO (Array.bounds chars)

randomHandle :: App String
randomHandle = randomHandleWithRange 50 256

randomHandleWithRange :: Int -> Int -> App String
randomHandleWithRange min' max' = liftIO $ do
  n <- randomRIO (min', max')
  replicateM n pick
  where
    chars = mkArray $ ['a' .. 'z'] <> ['0' .. '9'] <> "_-."
    pick = (chars !) <$> randomRIO (Array.bounds chars)

randomBytes :: Int -> App ByteString
randomBytes n = liftIO $ BS.pack <$> replicateM n randomIO

randomString :: Int -> App String
randomString n = liftIO $ replicateM n randomIO

randomJSON :: App Value
randomJSON = do
  let maxThings = 5
  liftIO (randomRIO (0 :: Int, 5)) >>= \case
    0 -> String . fromString <$> (randomString =<< randomRIO (0, maxThings))
    1 -> Number <$> liftIO (scientific <$> randomIO <*> randomIO)
    2 -> Bool <$> liftIO randomIO
    3 -> pure Null
    4 -> do
      n <- liftIO $ randomRIO (0, maxThings)
      Array . Vector.fromList <$> replicateM n randomJSON
    5 -> do
      n <- liftIO $ randomRIO (0, maxThings)
      keys <- do
        keyLength <- randomRIO (0, maxThings)
        replicateM n (randomString keyLength)
      vals <- replicateM n randomJSON
      pure . object $ zipWith (.=) keys vals
    _ -> error $ "impopssible: randomJSON"

randomHex :: Int -> App String
randomHex n = liftIO $ replicateM n pick
  where
    chars = mkArray (['0' .. '9'] <> ['a' .. 'f'])
    pick = (chars !) <$> randomRIO (Array.bounds chars)

-- Should not have leading 0.
randomClientId :: App String
randomClientId = do
  second <- randomHex 15
  first <- pick
  pure $ first : second
  where
    chars = mkArray (['1' .. '9'] <> ['a' .. 'f'])
    pick = (chars !) <$> randomRIO (Array.bounds chars)

mkArray :: [a] -> Array.Array Int a
mkArray l = Array.listArray (0, length l - 1) l

recipient :: (MakesValue u) => u -> App Value
recipient u = do
  uid <- u %. "id"
  pure
    $ object
      [ "user_id" .= uid,
        "route" .= "any",
        "clients" .= ([] :: [String])
      ]
