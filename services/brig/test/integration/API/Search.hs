{-# LANGUAGE OverloadedStrings #-}

module API.Search (tests) where

import API.Search.Util
import Brig.Types
import Control.Concurrent              (threadDelay)
import Control.Concurrent.Async.Lifted.Safe
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Monoid
import Network.Wire.Client.Monad as Client
import Network.Wire.Client.API.Search
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Named

tests :: Client.Env -> IO TestTree
tests env =
    return $ testGroup "search"
        [ testNew env "opt-in-out" $ testOptInOut
        , testNew env "by-name"    $ testSearchByName
        , testNew env "by-handle"  $ testSearchByHandle
        , testNew env "reindex"    $ testReindex
        ]

testOptInOut :: Test ()
testOptInOut = do
    brig <- getBrig

    u1 <- randomUserWithHandle
    u2 <- liftHttp $ randomUser brig
    refreshIndex

    let uid1 = userId u1
        uid2 = userId u2
        Just h1 = fromHandle <$> userHandle u1

    asUser uid1 $ assertSearchable "default" True
    asUser uid2 $ assertCanFind uid1 h1

    asUser uid1 $ do
        setSearchable optOut
        refreshIndex
        assertSearchable "opted out" False
    asUser uid2 $
        assertCan'tFind uid1 h1

    asUser uid1 $ do
        setSearchable optIn
        refreshIndex
        assertSearchable "opted in" True
    asUser uid2 $
        assertCanFind uid1 h1

testSearchByName :: Test ()
testSearchByName = do
    brig <- getBrig

    u1 <- liftHttp $ randomUser brig
    u2 <- liftHttp $ randomUser brig
    refreshIndex

    let uid1 = userId u1
        uid2 = userId u2

    asUser uid1 $ assertCanFind uid2 (fromName (userName u2))
    asUser uid2 $ assertCanFind uid1 (fromName (userName u1))
    -- Users cannot find themselves
    asUser uid1 $ assertCan'tFind uid1 (fromName (userName u1))
    asUser uid2 $ assertCan'tFind uid2 (fromName (userName u2))

testSearchByHandle :: Test ()
testSearchByHandle = do
    brig <- getBrig

    u1 <- randomUserWithHandle
    u2 <- liftHttp $ randomUser brig
    refreshIndex

    let uid1 = userId u1
        uid2 = userId u2
        Just h = fromHandle <$> userHandle u1

    asUser uid2 $ assertCanFind uid1 h

testReindex :: Test ()
testReindex = do
    brig <- getBrig
    u <- liftHttp $ randomUser brig
    asUser (userId u) $ do

        ((), regular, tinfoil, unfoil) <- runConcurrently $ (,,,)
            <$> Concurrently reindexAll
            <*> Concurrently (replicateM 5 $ delayed *> mkRegularUser)
            <*> Concurrently (replicateM 5 $ delayed *> mkInvisibleUser)
            <*> Concurrently (replicateM 5 $ delayed *> mkTmpInvisibleUser)

        refreshIndex

        for_ tinfoil $ \u' ->
            let Just h = fromHandle <$> userHandle u'
             in assertCan'tFind (userId u') h

        for_ (regular <> unfoil) $ \u' -> do
            let Just h = fromHandle <$> userHandle u'
            assertCanFind (userId u') h
            (found:_) <- searchResults <$>
                search @Test ! #query h
                             ! defaults
            liftIO $ do
                assertEqual "Unexpected UserId" (contactUserId  found) (userId u')
                assertEqual "Unexpected Name"   (contactName    found) (fromName $ userName u')
                assertEqual "Unexpected Colour" (contactColorId found) (Just . fromIntegral . fromColourId  $ userAccentId u')
                assertEqual "Unexpected Handle" (contactHandle  found) (fromHandle <$> userHandle u')
  where
    -- note: delaying user creation a bit to increase the chance of actually
    -- happen concurrently to the reindex on a small test database
    delayed = liftIO $ threadDelay 10000

    mkRegularUser = randomUserWithHandle

    mkInvisibleUser = do
        u <- randomUserWithHandle
        asUser (userId u) $
            setSearchable optOut
        pure u

    mkTmpInvisibleUser = do
        u <- randomUserWithHandle
        asUser (userId u) $ do
            setSearchable optOut
            setSearchable optIn
        pure u
