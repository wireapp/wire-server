{-# LANGUAGE LambdaCase #-}

module Network.Wire.Bot.Crypto.Glue
  ( openBox,
    deleteBox,
    genPrekeys,
    genLastKey,
    randomBytes,
    unwrap,
  )
where

import Control.Monad.Catch (MonadThrow, throwM)
import Data.ByteString.Base64
import Data.Id
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Imports
import qualified Network.Wire.Client.API.Client as C
import System.CryptoBox (Box)
import qualified System.CryptoBox as CBox
import System.FilePath
import System.IO.Error (userError)

openBox :: UserId -> Maybe Text -> IO Box
openBox uid label = do
  dir <- getBoxDir uid label
  createDirectoryIfMissing True dir
  unwrap =<< CBox.open dir

-- | Will delete all boxes if passed 'Nothing'.
deleteBox :: UserId -> Maybe Text -> IO ()
deleteBox uid label = do
  dir <- getBoxDir uid label
  removePathForcibly dir -- using "forcibly" so that it wouldn't fail
    -- if the directory doesn't exist

genPrekeys :: Box -> Word16 -> IO [C.Prekey]
genPrekeys box n = mapM (genPrekey box) [1 .. n - 1]

genLastKey :: Box -> IO C.LastPrekey
genLastKey box = C.lastPrekey . C.prekeyKey <$> genPrekey box 0xFFFF

genPrekey :: Box -> Word16 -> IO C.Prekey
genPrekey box i =
  C.Prekey (C.PrekeyId i) . decodeUtf8 . encode
    <$> (CBox.copyBytes . CBox.prekey =<< unwrap =<< CBox.newPrekey box i)

randomBytes :: MonadIO m => Box -> Word32 -> m ByteString
randomBytes b n = liftIO $ CBox.randomBytes b n >>= unwrap >>= CBox.copyBytes

unwrap :: (Show a, MonadThrow m) => CBox.Result a -> m a
unwrap (CBox.Success a) = return a
unwrap other = throwM $ userError (show other)

getBoxDir :: UserId -> Maybe Text -> IO FilePath
getBoxDir uid label = do
  tmp <- getTemporaryDirectory
  let usrDir = show (toUUID uid)
  let cltDir = maybe "" Text.unpack label
  return $ tmp </> "wire-bot" </> usrDir </> cltDir
