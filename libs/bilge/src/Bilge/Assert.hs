{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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

-- | 'Assert' provides ways to maintain claims over 'Response's.
module Bilge.Assert
  ( Assertions,
    Contains (..),
    (!!!),
    (<!!),
    (===),
    (=/=),
    (=~=),
    assertTrue,
    assertTrue_,
    assert,
    assert_,
  )
where

import Control.Monad.Catch
import Control.Monad.Writer.Class
import Control.Monad.Writer.Strict
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as Lazy
import Data.List (intersperse, isInfixOf)
import Imports
import Network.HTTP.Client
import System.Console.ANSI
import Text.Printf

-- | Type-class the denote a containment relation.
class Contains a where
  -- | If 'True', the first argument is contained in the second.
  contains :: a -> a -> Bool

instance Contains ByteString where
  contains a b = not . S.null . snd $ S.breakSubstring a b

instance Contains Lazy.ByteString where
  contains a b = contains (Lazy.toStrict a) (Lazy.toStrict b)

instance Eq a => Contains [a] where
  contains = isInfixOf

instance Contains a => Contains (Maybe a) where
  contains (Just a) (Just b) = contains a b
  contains Nothing _ = True
  contains _ Nothing = False

-- | A 'Writer' monad containing the list of assertions as predicate
-- functions of a 'Response'.
newtype Assertions a = Assertions
  { _assertions :: Writer [Response (Maybe Lazy.ByteString) -> Maybe String] a
  }
  deriving (Functor, Applicative, Monad)

-- | Given an IO action to get a 'Response' and a set of assertions,
-- evaluate all assertions against the response.
-- This function prints an error message for every assertion that fails
-- (N.B. assertions are enumerated, i.e. you will see the index of the
-- assertion that failed). It will also return the response,
-- so it can be used for further inspection.
(<!!) ::
  (HasCallStack, Functor m, MonadIO m, MonadCatch m) =>
  m (Response (Maybe Lazy.ByteString)) ->
  Assertions () ->
  m (Response (Maybe Lazy.ByteString))
io <!! aa = do
  r <- io `catch` printErr
  let results = map ($ r) (execWriter . _assertions $ aa)
  let failures = filter (isJust . snd) $ zip [1 ..] results
  unless (null failures) $
    error . concat $
      title "Assertions failed:\n" :
      intersperse "\n" (map msg failures)
        ++ ["\n\nResponse was:\n\n" ++ show r]
  return r
  where
    msg :: (Int, Maybe String) -> String
    msg (i, Just m) = printf "%2d: " i ++ err m
    msg _ = ""
    printErr :: MonadIO m => SomeException -> m a
    printErr e = error $ title "Error executing request: " ++ err (show e)

-- | Like '<!!' but discards the 'Response'.
(!!!) ::
  (HasCallStack, Functor m, MonadIO m, MonadCatch m) =>
  m (Response (Maybe Lazy.ByteString)) ->
  Assertions () ->
  m ()
(!!!) io = void . (<!!) io

infix 4 ===

infix 4 =/=

infixr 3 !!!

infixr 3 <!!

-- | Tests the assertion that the left-hand side and the right-hand side
-- are equal. If not, actual values will be printed.
(===) ::
  (HasCallStack, Eq a, Show a) =>
  (Response (Maybe Lazy.ByteString) -> a) ->
  (Response (Maybe Lazy.ByteString) -> a) ->
  Assertions ()
f === g = Assertions $ tell [\r -> test " =/= " (==) (f r) (g r)]

-- | Tests the assertion that the left-hand side and the right-hand side
-- are not equal. If not, actual values will be printed.
(=/=) ::
  (HasCallStack, Eq a, Show a) =>
  (Response (Maybe Lazy.ByteString) -> a) ->
  (Response (Maybe Lazy.ByteString) -> a) ->
  Assertions ()
f =/= g = Assertions $ tell [\r -> test " === " (/=) (f r) (g r)]

-- | Tests the assertion that the left-hand side is contained in the right-hand side.
-- If not, actual values will be printed.
(=~=) ::
  (HasCallStack, Show a, Contains a) =>
  (Response (Maybe Lazy.ByteString) -> a) ->
  (Response (Maybe Lazy.ByteString) -> a) ->
  Assertions ()
f =~= g = Assertions $ tell [\r -> test " not in " contains (f r) (g r)]

-- | Generic assertion on a request. The 'String' argument will be printed
-- in case the assertion fails.
assertTrue :: HasCallStack => String -> (Response (Maybe Lazy.ByteString) -> Bool) -> Assertions ()
assertTrue e f = Assertions $ tell [\r -> if f r then Nothing else Just e]

-- | Generic assertion on a request.
assertTrue_ :: HasCallStack => (Response (Maybe Lazy.ByteString) -> Bool) -> Assertions ()
assertTrue_ = assertTrue "false"

-- | Generic assertion inside the 'Assertions' monad. The 'String' argument
-- will be printed in case the assertion fails.
assert :: HasCallStack => String -> Bool -> Assertions ()
assert m = assertTrue m . const

-- | Generic assertion inside the 'Assertions' monad.
assert_ :: HasCallStack => Bool -> Assertions ()
assert_ = assertTrue_ . const

-- Internal

test :: (HasCallStack, Show a) => String -> (a -> a -> Bool) -> a -> a -> Maybe String
test s o a b
  | o a b = Nothing
  | otherwise = Just $ show a ++ s ++ show b

title, err :: String -> String
title = with Yellow
err = with Red

with :: Color -> String -> String
with c a =
  setSGRCode [SetColor Foreground Vivid c] ++ a ++ setSGRCode []
