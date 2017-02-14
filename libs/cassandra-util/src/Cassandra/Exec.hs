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
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Int
import Data.Text (Text)
import Database.CQL.IO

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
