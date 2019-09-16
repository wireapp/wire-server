-- | Imports that are supposed to be used in all wire-server code.
module Imports
    (
    -- * Base
      module Prelude
    , module Control.Applicative
    , module Control.Monad
    , module Data.Functor
    , module Data.Bifunctor
    , module Data.Function
    , module Data.Functor.Identity
    , module Data.Int
    , module Data.Word
    , module Data.Void
    , module Data.Bool
    , module Data.Char
    , module Data.Ord
    , module Data.Semigroup
    , module Data.Monoid
    , module Data.Maybe
    , module Data.Either
    , module Data.Foldable
    , module Data.Traversable
    , module Data.Tuple
    , module Data.String
    , module Data.List
    , Generic
    , Typeable
    , HasCallStack
    , readMaybe, readEither

    -- * Transformers, 'MonadIO' and 'UnliftIO'
    , module Control.Monad.Trans
    , module Control.Monad.Reader.Class
    , module Control.Monad.Trans.Reader
    , module Control.Monad.IO.Unlift

    -- * deepseq
    , module Control.DeepSeq

    -- * IO
    , module UnliftIO.IO
    , module UnliftIO.Directory
    -- ** Prelude
    , putChar, putStr, putStrLn, print
    , getChar, getLine, getContents, interact
    , readFile, writeFile, appendFile
    , readIO, readLn
    -- ** Environment
    , getArgs, getEnv, lookupEnv, setEnv, unsetEnv
    -- ** Concurrency primitives
    , ThreadId, forkIO, forkOS, killThread, threadDelay
    -- ** Variables
    , module UnliftIO.IORef
    , module UnliftIO.MVar

    -- * Exceptions
    , Exception(..), SomeException(..), SomeAsyncException(..), IOException

    -- * STM
    , module UnliftIO.STM

    -- * Containers
    , Map, Set, HashMap, HashSet
    , ByteString, LByteString
    , Text, LText

    -- * Extra Helpers
    , whenM
    , unlessM
    ) where

-- Explicitly saying what to import because some things from Prelude clash
-- with e.g. UnliftIO modules
import Prelude (
    Eq(..), Ord(..), Enum(..), Bounded(..),
    Integer, Float, Double, Rational,
    Num(..), Integral(..),
    Real(..), Fractional(..), Floating(..), RealFrac(..), RealFloat(..),
    subtract, even, odd, gcd, lcm, (^), (^^),
    fromIntegral, realToFrac,
    error, undefined, seq, ($!),
    Show(..), ShowS, shows, showChar, showString, showParen,
    Read(..), ReadS, reads, readParen, read, lex,
    IO, FilePath)
import qualified Prelude as P

import Control.Applicative hiding (optional, empty, some, many) -- common in
                                                                -- some libs
import Data.Functor
import Data.Bifunctor hiding (first, second)
import Data.Function
import Data.Functor.Identity
import Data.Int
import Data.Word
import Data.Void
import Data.Bool
import Data.Char
import Data.Ord
import Data.Semigroup hiding (diff, Option, option) -- conflicts with Options.Applicative.Option (should we care?)
import Data.Monoid hiding (First(..), Last(..)) -- First and Last are going to be deprecated. Use Semigroup instead
import Data.Maybe
import Data.Either
import Data.Foldable
import Data.Traversable
import Data.Tuple
import Data.List hiding (insert, delete)  -- 'insert' and 'delete' are
                                          -- common in database modules
import Data.String
import Control.Monad hiding (mapM_, sequence_, forM_, msum, mapM, sequence, forM)
import Control.Monad.Extra (whenM, unlessM)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import Text.Read (readMaybe, readEither)

import UnliftIO.Concurrent
import UnliftIO.Environment
import UnliftIO.Exception
import UnliftIO.IORef
import UnliftIO.MVar
import UnliftIO.IO hiding (Handle, getMonotonicTime)  -- Handle is hidden
                                                      -- because it's common
                                                      -- in Brig
import UnliftIO.Directory hiding (Permissions) -- Permissions is common in Galley
import UnliftIO.STM

import Data.Map (Map)    -- Lazy and strict versions are the same
import Data.Set (Set)
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.Text.Lazy
import qualified Data.ByteString.Lazy

import Control.Monad.Trans
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader (
    Reader, runReader, mapReader, withReader,
    ReaderT(ReaderT), runReaderT, mapReaderT, withReaderT)
import Control.Monad.IO.Unlift

import Control.DeepSeq (NFData(..), deepseq)


----------------------------------------------------------------------------
-- Type aliases

type LText = Data.Text.Lazy.Text
type LByteString = Data.ByteString.Lazy.ByteString

----------------------------------------------------------------------------
-- Lifted functions from Prelude

putChar :: MonadIO m => Char -> m ()
putChar = liftIO . P.putChar

putStr :: MonadIO m => String -> m ()
putStr = liftIO . P.putStr

putStrLn :: MonadIO m => String -> m ()
putStrLn = liftIO . P.putStrLn

print :: (Show a, MonadIO m) => a -> m ()
print = liftIO . P.print

getChar :: MonadIO m => m Char
getChar = liftIO P.getChar

getLine :: MonadIO m => m String
getLine = liftIO P.getLine

getContents :: MonadIO m => m String
getContents = liftIO P.getContents

interact :: MonadIO m => (String -> String) -> m ()
interact = liftIO . P.interact

readFile :: MonadIO m => FilePath -> m String
readFile = liftIO . P.readFile

writeFile :: MonadIO m => FilePath -> String -> m ()
writeFile = fmap liftIO . P.writeFile

appendFile :: MonadIO m => FilePath -> String -> m ()
appendFile = fmap liftIO . P.appendFile

readIO :: (Read a, MonadIO m) => String -> m a
readIO = liftIO . P.readIO

readLn :: (Read a, MonadIO m) => m a
readLn = liftIO P.readLn
