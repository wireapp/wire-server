module Testlib.Prelude
  ( module Testlib.App,
    module Testlib.Env,
    module Testlib.Cannon,
    module Text.RawString.QQ,
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
  )
where

import Control.Applicative hiding (empty, many, optional, some)
import Control.Monad hiding (forM, forM_, mapM, mapM_, msum, sequence, sequence_)
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
-- 'insert' and 'delete' are common in database modules
import Data.List hiding (delete, insert, singleton)
-- Lazy and strict versions are the same
import Data.Map (Map)
import Data.Maybe
-- First and Last are going to be deprecated. Use Semigroup instead
import Data.Monoid hiding (First (..), Last (..))
import Data.Ord
-- conflicts with Options.Applicative.Option (should we care?)
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
import Testlib.Cannon
import Testlib.Env
import Text.RawString.QQ
-- Permissions is common in Galley
import UnliftIO.Exception
-- Handle is hidden because it's common in Brig
-- Explicitly saying what to import because some things from Prelude clash
-- with e.g. UnliftIO modules
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
