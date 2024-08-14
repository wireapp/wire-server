{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
    module Data.Either.Combinators,
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
    putStr,
    putStrLn,
    print,
    getLine,
    readFile,
    writeFile,
    appendFile,

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
    catMaybesToList,

    -- * Functor
    (<$$>),
    (<$$$>),

    -- * development
    todo,
    pattern TODO,
    TodoException (..),
  )
where

-- common in some libs
import Control.Applicative hiding (empty, many, optional, some)
import Control.DeepSeq (NFData (..), deepseq)
import Control.Exception
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
import Data.ByteString.Lazy qualified
import Data.Char
import Data.Either
import Data.Either.Combinators hiding (fromLeft, fromRight, isLeft, isRight)
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Int
import Data.Kind (Type)
import Data.List hiding (delete, insert, singleton)
import Data.Map (Map)
import Data.Maybe
import Data.Monoid hiding (First (..), Last (..))
import Data.Ord
import Data.Semigroup hiding (diff)
import Data.Set (Set)
import Data.String
import Data.Text (Text)
import Data.Text.Lazy qualified
import Data.Traversable
import Data.Tuple
import Data.Void
import Data.Word
import GHC.Exts
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
import Prelude qualified as P

----------------------------------------------------------------------------
-- Type aliases

type LText = Data.Text.Lazy.Text

type LByteString = Data.ByteString.Lazy.ByteString

----------------------------------------------------------------------------
-- Lifted functions from Prelude

putStr :: (MonadIO m) => String -> m ()
putStr = liftIO . P.putStr

putStrLn :: (MonadIO m) => String -> m ()
putStrLn = liftIO . P.putStrLn

print :: (Show a, MonadIO m) => a -> m ()
print = liftIO . P.print

getLine :: (MonadIO m) => m String
getLine = liftIO P.getLine

readFile :: (MonadIO m) => FilePath -> m String
readFile = liftIO . P.readFile

writeFile :: (MonadIO m) => FilePath -> String -> m ()
writeFile = fmap liftIO . P.writeFile

appendFile :: (MonadIO m) => FilePath -> String -> m ()
appendFile = fmap liftIO . P.appendFile

----------------------------------------------------------------------
-- placeholders

-- | 'todo' indicates unfinished code.
--
-- It is to be used whenever you want to indicate that you are missing a part of
-- the implementation and want to fill that in later.
--
-- This takes a middle ground between other alternatives - unlike typed holes it doesn't cause
-- a /compile time error/, but in contrast to 'GHC.Err.error' and 'GHC.Err.undefined', it does emit
-- a /warning at compilation time/.
--
-- Similarly to all of 'GHC.Err.undefined', 'GHC.Err.error' and typed holes, this /will throw an error/
-- if it is /evaluated at runtime/. This error can only be caught in 'System.IO.IO'.
--
-- This is intended to /never/ stay in code but exists purely for signifying

-- "work in progress" code.
--
-- To make the emitted warning a compile error instead (e.g. for use in CI), add
-- the @-Werror=x-todo@ flag to your @OPTIONS_GHC@.
--
-- ==== __Examples__
--
-- @
-- superComplexFunction :: 'Data.Maybe.Maybe' a -> 'System.IO.IO' 'Data.Int.Int'
-- -- we already know how to implement this in the 'Data.Maybe.Nothing' case
-- superComplexFunction 'Data.Maybe.Nothing' = 'Control.Applicative.pure' 42
-- -- but the 'Data.Maybe.Just' case is super complicated, so we leave it as 'todo' for now
-- superComplexFunction ('Data.Maybe.Just' a) = 'todo'
-- @
--
-- ==== __Representation Polymorphism__
--
-- 'todo', in contrast to 'TODO', is fully representation polymorphic
--
-- @since base-4.21.0.0
todo :: forall {r :: RuntimeRep} (a :: TYPE r). (HasCallStack) => a
todo = throw TodoException
{-# WARNING todo "'todo' left in code" #-}

-- FUTUREWORK(mangoiv): should be: WARNING in "x-todo" from ghc 9.8 on

-- | 'TODO' indicates unfinished code or an unimplemented pattern match
--
-- You can use this in most positions where you could pass 'todo', but it /also/ can be used in
-- the position of a pattern to indicate that there are cases you have not yet considered.
--
-- This pattern synonym is marked @COMPLETE@, implying that every match after matching on 'TODO'
-- will /emit a redundant pattern match warning/. Adding new options to your datatype, similarly
-- to how wildcard patterns (patterns starting with an underscore) work, will /not cause any warnings or errors/.
--
-- ==== __Examples__
--
-- Since the pattern match is strict, even if the branch itself does not evaluate to bottom, matching on
-- 'TODO' will.
--
-- @
-- >>> x = []
-- >>> case x of
-- ...   (x : _) -> x
-- ...   'TODO' -> 42
-- *** Exception: Develop.Placeholder.todo: not yet implemented
-- @
--
-- As usual, this behaviour can be reversed by using a @~@ in front of 'TODO' in pattern position.
--
-- @
-- >>> x = []
-- >>> case x of
-- ...   (x : _) -> x
-- ...   ~'TODO' -> 42
-- 42
-- @
--
-- In most situations, 'TODO' can be used just like 'todo', where the above is equivalent to the below
--
-- @
-- >>> y :: 'Data.Int.Int' = 'todo'
-- >>> x :: 'Data.Int.Int' = 'TODO'
-- @
--
--
-- ==== __Representation Polymorphism__
--
-- Mind that pattern synonyms may not be representation polymorphic, hence, if you need something
-- that can be used with some kind other than 'Data.Kind.Type', you have to use 'todo'. For example,
-- 'TODO' cannot stand instead of a pattern match on an @'GHC.Exts.Int#' :: 'TYPE' 'GHC.Exts.IntRep'@
-- or as a placeholder for a @'GHC.Exts.ByteArray#' :: 'GHC.Exts.UnliftedType'@
--
-- @since base-4.21.0.0
pattern TODO :: forall (a :: Type). (HasCallStack) => forall. a
pattern TODO <- (throw TodoException -> !_unused)
  where
    TODO = throw TodoException
{-# WARNING TODO "'TODO' left in code" #-}

-- FUTUREWORK(mangoiv): should be WARNING in "x-todo" from ghc 9.8 on

{-# COMPLETE TODO #-}

-- | This is the 'Exception' thrown by 'todo'.
--
-- This exception occurring indicates a bug as 'todo' should /never/ remain in code.
--
-- @since base-4.21.0.0
data TodoException = TodoException
  deriving stock (Eq, Show)

instance Exception TodoException where
  displayException _ = "Develop.Placeholder.todo: not yet implemented"

----------------------------------------------------------------------
-- Functor

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap

infix 4 <$$>

(<$$$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<$$$>) = fmap . fmap . fmap

infix 4 <$$$>

catMaybesToList :: Maybe (Maybe a) -> [a]
catMaybesToList = catMaybes . maybeToList
