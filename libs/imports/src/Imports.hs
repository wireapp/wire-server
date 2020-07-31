-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

-- | Imports that are supposed to be used in all wire-server code.
module Imports
  ( -- * Base
    module Prelude,
    module Control.Applicative,
    module Control.Monad,
    module Data.Functor,
    module Data.Bifunctor,
    module Data.Function,
    module Data.Functor.Identity,
    module Data.Int,
    module Data.Word,
    module Data.Void,
    module Data.Bool,
    module Data.Char,
    module Data.Ord,
    module Data.Semigroup,
    module Data.Monoid,
    module Data.Maybe,
    module Data.Either,
    module Data.Foldable,
    module Data.Traversable,
    module Data.Tuple,
    module Data.String,
    module Data.List,
    Generic,
    Typeable,
    HasCallStack,
    readMaybe,
    readEither,

    -- * Transformers, 'MonadIO' and 'UnliftIO'
    module Control.Monad.Trans,
    module Control.Monad.Reader.Class,
    module Control.Monad.Trans.Reader,
    module Control.Monad.IO.Unlift,

    -- * deepseq
    module Control.DeepSeq,

    -- * IO
    module UnliftIO.IO,
    module UnliftIO.Directory,

    -- ** Prelude
    putChar,
    putStr,
    putStrLn,
    print,
    getChar,
    getLine,
    getContents,
    interact,
    readFile,
    writeFile,
    appendFile,
    readIO,
    readLn,

    -- ** Environment
    getArgs,
    getEnv,
    lookupEnv,
    setEnv,
    unsetEnv,

    -- ** Concurrency primitives
    ThreadId,
    forkIO,
    forkOS,
    killThread,
    threadDelay,

    -- ** Variables
    module UnliftIO.IORef,
    module UnliftIO.MVar,

    -- * Exceptions
    Exception (..),
    SomeException (..),
    SomeAsyncException (..),
    IOException,

    -- * STM
    module UnliftIO.STM,

    -- * Containers
    Map,
    Set,
    HashMap,
    HashSet,
    ByteString,
    LByteString,
    Text,
    LText,

    -- * Extra Helpers
    whenM,
    unlessM,
  )
where

-- Explicitly saying what to import because some things from Prelude clash
-- with e.g. UnliftIO modules

import Control.Applicative hiding (empty, many, optional, some) -- common in
-- some libs

-- conflicts with Options.Applicative.Option (should we care?)
-- First and Last are going to be deprecated. Use Semigroup instead

-- 'insert' and 'delete' are
-- common in database modules

-- Handle is hidden
-- because it's common
-- in Brig
-- Permissions is common in Galley

-- Lazy and strict versions are the same

import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad hiding (forM, forM_, mapM, mapM_, msum, sequence, sequence_)
import Control.Monad.Extra (unlessM, whenM)
import Control.Monad.IO.Unlift
import Control.Monad.Reader.Class
import Control.Monad.Trans
import Control.Monad.Trans.Reader
  ( Reader,
    ReaderT (ReaderT),
    mapReader,
    mapReaderT,
    runReader,
    runReaderT,
    withReader,
    withReaderT,
  )
import Data.Bifunctor hiding (first, second)
import Data.Bool
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy
import Data.Char
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Int
import Data.List hiding (delete, insert)
import Data.Map (Map)
import Data.Maybe
import Data.Monoid hiding (First (..), Last (..))
import Data.Ord
import Data.Semigroup hiding (Option, diff, option)
import Data.Set (Set)
import Data.String
import Data.Text (Text)
import qualified Data.Text.Lazy
import Data.Traversable
import Data.Tuple
import Data.Typeable (Typeable)
import Data.Void
import Data.Word
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Text.Read (readEither, readMaybe)
import UnliftIO.Concurrent
import UnliftIO.Directory hiding (Permissions)
import UnliftIO.Environment
import UnliftIO.Exception
import UnliftIO.IO hiding (Handle, getMonotonicTime)
import UnliftIO.IORef
import UnliftIO.MVar
import UnliftIO.STM
import Prelude
  ( Bounded (..),
    Double,
    Enum (..),
    Eq (..),
    FilePath,
    Float,
    Floating (..),
    Fractional (..),
    IO,
    Integer,
    Integral (..),
    Num (..),
    Ord (..),
    Rational,
    Read (..),
    ReadS,
    Real (..),
    RealFloat (..),
    RealFrac (..),
    Show (..),
    ShowS,
    error,
    even,
    fromIntegral,
    gcd,
    lcm,
    lex,
    odd,
    read,
    readParen,
    reads,
    realToFrac,
    seq,
    showChar,
    showParen,
    showString,
    shows,
    subtract,
    undefined,
    ($!),
    (^),
    (^^),
  )
import qualified Prelude as P

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
