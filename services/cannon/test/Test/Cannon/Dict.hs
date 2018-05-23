{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cannon.Dict where

import Cannon.Dict (Dict)
import Cannon.WS (Key, mkKey)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import Data.ByteString (ByteString)
import Data.Id
import Data.Maybe (isJust)
import Data.String
import Data.UUID hiding (fromString)
import Data.UUID.V4
import System.Random
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.QuickCheck.Random
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.QuickCheck
import Test.QuickCheck.Gen

import qualified Cannon.Dict          as D
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.List            as List

tests :: TestTree
tests = testGroup "Dict Tests"
    [ testProperty "some dict"       (runProp (void . someDict))
    , testProperty "add/remove"      (runProp insertRemove)
    , testProperty "insert/removeIf" (runProp insertRemoveIf)
    , testCase     "insert/lookup"   insertLookup
    ]

someDict :: ([Key], [ByteString]) -> PropertyM IO (Dict Key ByteString)
someDict (ks, vs) = do
    let entries = zip (List.nub ks) vs
    d <- run $ D.empty 64
    run $ forM_ entries $ \e -> D.insert (fst e) (snd e) d
    s <- run $ D.size d
    assertEq "entries length" s (length entries)
    return d

insertRemove :: ([Key], [ByteString]) -> PropertyM IO ()
insertRemove kv = do
    d <- someDict kv
    a <- head <$> (run $ sample' arbitrary)
    b <- head <$> (run $ sample' arbitrary)
    exists <- run $ isJust <$> D.lookup a d
    pre $ not exists
    x <- run $ D.size d
    run $ D.insert a b d
    y <- run $ D.size d
    assertEq "size+1" (x + 1) y
    run . void $ D.remove a d
    z <- run $ D.size d
    assertEq "original size" z x

insertRemoveIf :: ([Key], [ByteString]) -> PropertyM IO ()
insertRemoveIf kv = do
    d  <- someDict kv
    a  <- head <$> (run $ sample' arbitrary)
    b  <- head <$> (run $ sample' arbitrary)
    b' <- run $ do
            D.insert a b d
            D.lookup a d

    pre $ Just b == b'
    x  <- run $ D.removeIf (maybe False (b ==)) a d
    assert x

    c  <- head <$> (run $ sample' arbitrary)
    y  <- run $ D.removeIf (maybe False (c ==)) a d
    assert (not y)

insertLookup :: Assertion
insertLookup = do
    let nThreads = 741
    dict <- D.empty 256
    keys <- take nThreads <$> samples 64 arbitrary
    mapM_ wait =<< zipWithM ($) (repeat $ async . action dict) keys
  where
    action :: Dict Key Lazy.ByteString -> Key -> IO ()
    action d k = do
        v     <- toByteString <$> nextRandom
        added <- D.add k v d
        when added $ replicateM_ 361 $ do
            threadDelay 3571
            x <- D.lookup k d
            Just v @=? x

assertEq :: (Show a, Eq a, Monad m) => String -> a -> a -> PropertyM m ()
assertEq m a b
    | a == b    = return ()
    | otherwise = fail $
        "assertEq: " ++ m ++ ": " ++ show a ++ " =/= " ++ show b

samples :: Int -> Gen a -> IO [a]
samples n (MkGen f) = do
    gen <- newQCGen
    let rands g = g1 : rands g2 where (g1, g2) = split g
    return $ [ f r i | i <- repeat n, r <- rands gen]

runProp :: (Show a, Arbitrary a, Testable b) => (a -> PropertyM IO b) -> Property
runProp = monadicIO . forAllM arbitrary

instance Arbitrary Key where
    arbitrary = mkKey <$> arbitrary <*> arbitrary

instance Arbitrary ConnId where
    arbitrary = ConnId <$> arbitrary

instance Arbitrary ByteString where
    arbitrary = fromString <$> arbitrary
