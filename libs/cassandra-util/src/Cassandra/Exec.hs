{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cassandra.Exec
    ( Client
    , MonadClient    (..)
    , ClientState
    , CassandraError (..)
    , syncCassandra
    , runClient
    , Database.CQL.IO.init
    , shutdown

    , Page (hasMore, result, nextPage)
    , emptyPage

    , params
    , paramsP
    , x5
    , x1

    , query
    , query1
    , write
    , schema
    , paginate
    , paginateC

      -- * Prepared Queries
    , PrepQuery
    , prepared
    , queryString

      -- * Batch
    , BatchM
    , addQuery
    , addPrepQuery
    , setType
    , setConsistency
    , setSerialConsistency
    , batch

      -- * Retry Settings
    , RetrySettings
    , adjustConsistency
    , adjustSendTimeout
    , adjustResponseTimeout
    , retry
    ) where

import Imports
import Cassandra.CQL
import Control.Monad.Catch
import Data.Conduit
import Database.CQL.IO

params :: Tuple a => Consistency -> a -> QueryParams a
params c p = QueryParams c False p Nothing Nothing Nothing Nothing
{-# INLINE params #-}

paramsP :: Consistency -> a -> Int32 -> QueryParams a
paramsP c p n = QueryParams c False p (Just n) Nothing Nothing Nothing
{-# INLINE paramsP #-}

-- | 'x5' must only be used for idempotent queries, or for cases
-- when a duplicate write has no severe consequences in
-- the context of the application's data model.
-- For more info see e.g.
-- https://docs.datastax.com/en/developer/java-driver//3.6/manual/idempotence/
--
-- The eager retry policy permits 5 retries with exponential
-- backoff (base-2) with an initial delay of 100ms, i.e. the
-- retries will be performed with 100ms, 200ms, 400ms, 800ms
-- and 1.6s delay, respectively, for a maximum delay of ~3s.
x5 :: RetrySettings
x5 = eagerRetrySettings
{-# INLINE x5 #-}

-- | Single, immediate retry, always safe.
-- The 'defRetryHandlers' used are safe also with non-idempotent queries.
x1 :: RetrySettings
x1 = defRetrySettings
{-# INLINE x1 #-}

data CassandraError
    = Cassandra   !Error
    | Comm        !IOException
    | InvalidData !Text
    | Other       !SomeException
    deriving Show

syncCassandra :: (Functor m, MonadIO m, MonadCatch m) => m a -> m (Either CassandraError a)
syncCassandra m = catches (Right <$> m)
    [ Handler $ \(e :: Error)         -> return . Left . Cassandra $ e
    , Handler $ \(e :: IOException)   -> return . Left . Comm $ e
    , Handler $ \(e :: SomeException) -> return . Left . Other $ e
    ]

-- | Stream results of a query.
--
-- You can execute this conduit by doing @transPipe (runClient ...)@.
paginateC :: (Tuple a, Tuple b, RunQ q, MonadClient m)
          => q R a b -> QueryParams a -> RetrySettings -> ConduitM () [b] m ()
paginateC q p r = go =<< lift (retry r (paginate q p))
  where
    go page = do
        unless (null (result page)) $
            yield (result page)
        when (hasMore page) $
            go =<< lift (retry r (liftClient (nextPage page)))
