{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.ZAuth.Creation
    ( -- * Types
      Create
    , Env

      -- * Initialisation
    , mkEnv
    , runCreate

    , withIndex
    , accessToken
    , accessToken1
    , userToken
    , sessionToken
    , botToken
    , providerToken
    , renewToken
    ) where

import Control.Lens hiding (withIndex)
import Control.Monad
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.ByteString.Conversion
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (toLazyByteString)
import Data.Time.Clock.POSIX
import Data.UUID
import Data.Vector (Vector, (!))
import Data.Word
import Data.ZAuth.Token hiding (signature)
import Sodium.Crypto.Sign
import System.Random.MWC

import qualified Data.ByteString as Strict
import qualified Data.Vector     as Vec

data Env = Env
    { keyIdx  :: Int
    , zSign   :: Vector (Strict.ByteString -> IO Signature)
    , randGen :: GenIO
    }

newtype Create a = Create
    { zauth :: ReaderT Env IO a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadThrow
               , MonadCatch
               )

tokenVersion :: Int
tokenVersion = 1

mkEnv :: SecretKey -> [SecretKey] -> IO Env
mkEnv k kk = Env 1 (Vec.fromList $ map signature (k:kk)) <$> liftIO createSystemRandom

runCreate :: Env -> Int -> Create a -> IO a
runCreate z k m = do
    when (k < 1 || k > Vec.length (zSign z)) $
        error "runCreate: Key index out of range."
    runReaderT (zauth m) (z { keyIdx = k })

withIndex :: Int -> Create a -> Create a
withIndex k m = Create $ do
    e <- ask
    when (k < 1 || k > Vec.length (zSign e)) $
        error "withIndex: Key index out of range."
    local (const $ (e { keyIdx = k })) (zauth m)

userToken :: Integer -> UUID -> Word32 -> Create (Token User)
userToken dur usr rnd = do
    d <- expiry dur
    k <- Create $ asks keyIdx
    newToken (mkHeader tokenVersion k d U Nothing) (mkUser usr rnd)

sessionToken :: Integer -> UUID -> Word32 -> Create (Token User)
sessionToken dur usr rnd = do
    d <- expiry dur
    k <- Create $ asks keyIdx
    newToken (mkHeader tokenVersion k d U (Just S)) (mkUser usr rnd)

accessToken :: Integer -> UUID -> Word64 -> Create (Token Access)
accessToken dur usr con = do
    d <- expiry dur
    k <- Create $ asks keyIdx
    newToken (mkHeader tokenVersion k d A Nothing) (mkAccess usr con)

accessToken1 :: Integer -> UUID -> Create (Token Access)
accessToken1 dur usr = do
    g <- Create $ asks randGen
    d <- liftIO $ asGenIO (uniform :: GenIO -> IO Word64) g
    accessToken dur usr d

botToken :: UUID -> UUID -> UUID -> Create (Token Bot)
botToken pid bid cnv = do
    k <- Create $ asks keyIdx
    newToken (mkHeader tokenVersion k (-1) B Nothing) (mkBot pid bid cnv)

providerToken :: Integer -> UUID -> Create (Token Provider)
providerToken dur pid = do
    d <- expiry dur
    k <- Create $ asks keyIdx
    newToken (mkHeader tokenVersion k d P Nothing) (mkProvider pid)

renewToken :: ToByteString a => Integer -> Token a -> Create (Token a)
renewToken dur tkn = do
    d <- expiry dur
    newToken (time .~ d $ tkn^.header) (tkn^.body)

-----------------------------------------------------------------------------
-- Internal

newToken :: ToByteString a => Header -> a -> Create (Token a)
newToken h a = Create $ do
    f <- asks zSign
    s <- liftIO . (f ! (h^.key - 1)) . toStrict . toLazyByteString $ writeData h a
    return $ mkToken s h a

expiry :: (Functor m, MonadIO m) => Integer -> m Integer
expiry duration = (duration +) . floor <$> liftIO getPOSIXTime
