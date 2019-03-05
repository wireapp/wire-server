{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.ZAuth.Creation
    ( -- * Types
      Create
    , Env

      -- * Initialisation
    , mkEnv
    , runCreate

      -- * Specific
    , accessToken
    , accessToken1
    , userToken
    , sessionToken
    , botToken
    , providerToken

      -- * Generic
    , withIndex
    , newToken
    , renewToken
    ) where

import Imports
import Control.Lens hiding (withIndex)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Data.ByteString.Conversion
import Data.ByteString.Lazy (toStrict)
import Data.ByteString.Builder (toLazyByteString)
import Data.Time.Clock.POSIX
import Data.UUID
import Data.Vector (Vector, (!))
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
    local (const (e { keyIdx = k })) (zauth m)

userToken :: Integer -> UUID -> Word32 -> Create (Token User)
userToken dur usr rnd = do
    d <- expiry dur
    newToken d U Nothing (mkUser usr rnd)

sessionToken :: Integer -> UUID -> Word32 -> Create (Token User)
sessionToken dur usr rnd = do
    d <- expiry dur
    newToken d U (Just S) (mkUser usr rnd)

accessToken :: Integer -> UUID -> Word64 -> Create (Token Access)
accessToken dur usr con = do
    d <- expiry dur
    newToken d A Nothing (mkAccess usr con)

accessToken1 :: Integer -> UUID -> Create (Token Access)
accessToken1 dur usr = do
    g <- Create $ asks randGen
    d <- liftIO $ asGenIO (uniform :: GenIO -> IO Word64) g
    accessToken dur usr d

botToken :: UUID -> UUID -> UUID -> Create (Token Bot)
botToken pid bid cnv = newToken (-1) B Nothing (mkBot pid bid cnv)

providerToken :: Integer -> UUID -> Create (Token Provider)
providerToken dur pid = do
    d <- expiry dur
    newToken d P Nothing (mkProvider pid)

renewToken :: ToByteString a => Integer -> Token a -> Create (Token a)
renewToken dur tkn = do
    d <- expiry dur
    newToken d (tkn^.header.typ) (tkn^.header.tag) (tkn^.body)

newToken :: ToByteString a => POSIXTime -> Type -> Maybe Tag -> a -> Create (Token a)
newToken ti ty ta a = do
    k <- Create $ asks keyIdx
    let h = mkHeader tokenVersion k (floor ti) ty ta
    s <- signToken h a
    return $ mkToken s h a

-----------------------------------------------------------------------------
-- Internal

signToken :: ToByteString a => Header -> a -> Create Signature
signToken h a = Create $ do
    f <- (! (h^.key - 1)) <$> asks zSign
    liftIO . f . toStrict . toLazyByteString $ writeData h a

expiry :: (Functor m, MonadIO m) => Integer -> m POSIXTime
expiry d = (fromInteger d +) <$> liftIO getPOSIXTime
