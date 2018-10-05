{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-} -- for MonadUnliftIO Client

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
    , noRetry
    , retryForever
    , maxRetries
    , adjustConsistency
    , constDelay
    , expBackoff
    , fibBackoff
    , adjustSendTimeout
    , adjustResponseTimeout
    , retry
    ) where

import Cassandra.CQL
import Control.Exception (IOException)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Int
import Data.Text (Text)
import Database.CQL.IO
import Data.Conduit

params :: Tuple a => Consistency -> a -> QueryParams a
params c p = QueryParams c False p Nothing Nothing Nothing
{-# INLINE params #-}

paramsP :: Consistency -> a -> Int32 -> QueryParams a
paramsP c p n = QueryParams c False p (Just n) Nothing Nothing
{-# INLINE paramsP #-}

x5 :: RetrySettings
x5 = maxRetries 5 . expBackoff 0.1 5 $ retryForever
{-# INLINE x5 #-}

x1 :: RetrySettings
x1 = maxRetries 1 retryForever
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

instance MonadUnliftIO Client where
    askUnliftIO = do
        env <- ask
        pure $ UnliftIO (runClient env)
