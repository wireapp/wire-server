module Testlib.Prelude
  ( module Testlib.App,
    module Testlib.Env,
    module Testlib.Cannon,
    module Testlib.Assertions,
    module Testlib.Types,
    module Testlib.ModService,
    module Testlib.HTTP,
    module Testlib.JSON,
    module Testlib.PTest,
    module Data.Aeson,
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
    Default (..),
    Generic,
    Typeable,
    HasCallStack,

    -- * Containers
    Map,
    Set,
    ByteString,

    -- * Exceptions
    Exception (..),
    SomeException (..),
    SomeAsyncException (..),
    IOException,

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
    liftIO,

    -- * Functor
    (<$$>),

    -- * Applicative
    allPreds,
  )
where

import Control.Applicative hiding (empty, many, optional, some)
import Control.Monad hiding (forM, forM_, mapM, mapM_, msum, sequence, sequence_)
-- 'insert' and 'delete' are common in database modules

-- Lazy and strict versions are the same

-- First and Last are going to be deprecated. Use Semigroup instead

-- conflicts with Options.Applicative.Option (should we care?)

-- Permissions is common in Galley

-- Handle is hidden because it's common in Brig
-- Explicitly saying what to import because some things from Prelude clash
-- with e.g. UnliftIO modules

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson hiding ((.=))
import Data.Bifunctor hiding (first, second)
import Data.Bool
import Data.ByteString (ByteString)
import Data.Char
import Data.Default
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.Int
import Data.List hiding (delete, insert, singleton)
import Data.Map (Map)
import Data.Maybe
import Data.Monoid hiding (First (..), Last (..))
import Data.Ord
import Data.Semigroup hiding (diff)
import Data.Set (Set)
import Data.String
import Data.Traversable
import Data.Tuple
import Data.Void
import Data.Word
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Testlib.App
import Testlib.Assertions
import Testlib.Cannon
import Testlib.Env
import Testlib.HTTP
import Testlib.JSON
import Testlib.ModService
import Testlib.PTest
import Testlib.Types
import UnliftIO.Exception
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
-- Lifted functions from Prelude

putChar :: (MonadIO m) => Char -> m ()
putChar = liftIO . P.putChar

putStr :: (MonadIO m) => String -> m ()
putStr = liftIO . P.putStr

putStrLn :: (MonadIO m) => String -> m ()
putStrLn = liftIO . P.putStrLn

print :: (Show a, MonadIO m) => a -> m ()
print = liftIO . P.print

getChar :: (MonadIO m) => m Char
getChar = liftIO P.getChar

getLine :: (MonadIO m) => m String
getLine = liftIO P.getLine

getContents :: (MonadIO m) => m String
getContents = liftIO P.getContents

interact :: (MonadIO m) => (String -> String) -> m ()
interact = liftIO . P.interact

readFile :: (MonadIO m) => FilePath -> m String
readFile = liftIO . P.readFile

writeFile :: (MonadIO m) => FilePath -> String -> m ()
writeFile = fmap liftIO . P.writeFile

appendFile :: (MonadIO m) => FilePath -> String -> m ()
appendFile = fmap liftIO . P.appendFile

readIO :: (Read a, MonadIO m) => String -> m a
readIO = liftIO . P.readIO

readLn :: (Read a, MonadIO m) => m a
readLn = liftIO P.readLn

----------------------------------------------------------------------
-- Functor

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infix 4 <$$>

----------------------------------------------------------------------
-- Applicative

allPreds :: (Applicative f) => [a -> f Bool] -> a -> f Bool
allPreds [] _ = pure True
allPreds [p] x = p x
allPreds (p1 : ps) x = (&&) <$> p1 x <*> allPreds ps x
