-- | Best-effort optimistic locking for prekeys via DynamoDB
module Wire.ClientStore.DynamoDB where

import Amazonka qualified as AWS
import Amazonka.Data.Text qualified as AWS
import Amazonka.DynamoDB qualified as AWS
import Amazonka.DynamoDB.Lens qualified as AWS
import Bilge.Retry (httpHandlers)
import Control.Error
import Control.Exception.Lens qualified as EL
import Control.Lens
import Control.Monad.Catch
import Control.Retry
import Data.ByteString.Conversion (toByteString')
import Data.HashMap.Strict qualified as HashMap
import Data.Id
import Data.Text qualified as Text
import Data.UUID qualified as UUID
import Imports
import Polysemy hiding (run)
import Polysemy.Input
import Polysemy.TinyLog (TinyLog)
import Polysemy.TinyLog qualified as Log
import Prometheus qualified as Prom
import System.Logger.Class (field, msg, val)
import UnliftIO.Resource (runResourceT)
import Wire.Sem.Metrics (Metrics)
import Wire.Sem.Metrics qualified as Metrics

data OptimisticLockEnv = OptimisticLockEnv
  { awsEnv :: AWS.Env,
    prekeyTable :: Text
  }

ddbClient :: Text
ddbClient = "client"

ddbVersion :: Text
ddbVersion = "version"

ddbKey :: UserId -> ClientId -> AWS.AttributeValue
ddbKey u c = AWS.S (UUID.toText (toUUID u) <> "." <> clientToText c)

key :: UserId -> ClientId -> HashMap Text AWS.AttributeValue
key u c = HashMap.singleton ddbClient (ddbKey u c)

deleteOptLock ::
  (Member (Embed IO) r, Member (Input OptimisticLockEnv) r) =>
  UserId ->
  ClientId ->
  Sem r ()
deleteOptLock u c = do
  t <- inputs (.prekeyTable)
  e <- inputs (.awsEnv)
  embed . runResourceT . void $ AWS.send e (AWS.newDeleteItem t & AWS.deleteItem_key .~ key u c)

withOptLock ::
  forall a r.
  ( Member (Final IO) r,
    Member (Input OptimisticLockEnv) r,
    Member Metrics r,
    Member TinyLog r
  ) =>
  UserId ->
  ClientId ->
  Sem r a ->
  Sem r a
withOptLock u c ma = go (10 :: Int)
  where
    go !n = do
      v <- (version =<<) <$> execDyn pure get
      a <- ma
      r <- execDyn pure (put v)
      case r of
        Nothing | n > 0 -> reportAttemptFailure >> go (n - 1)
        Nothing -> reportFailureAndLogError >> pure a
        Just _ -> pure a
    version :: AWS.GetItemResponse -> Maybe Word32
    version v = conv . HashMap.lookup ddbVersion =<< (view AWS.getItemResponse_item v)
      where
        conv :: Maybe AWS.AttributeValue -> Maybe Word32
        conv = \case
          Just (AWS.N t) -> readMaybe $ Text.unpack t
          _ -> Nothing
    get :: Text -> AWS.GetItem
    get t =
      AWS.newGetItem t
        & AWS.getItem_key .~ key u c
        & AWS.getItem_consistentRead ?~ True
    put :: Maybe Word32 -> Text -> AWS.PutItem
    put v t =
      AWS.newPutItem t
        & AWS.putItem_item .~ item v
        & AWS.putItem_expected ?~ check v

    check :: Maybe Word32 -> HashMap Text AWS.ExpectedAttributeValue
    check Nothing = HashMap.singleton ddbVersion $ AWS.newExpectedAttributeValue & AWS.expectedAttributeValue_comparisonOperator ?~ AWS.ComparisonOperator_NULL
    check (Just v) =
      HashMap.singleton ddbVersion $
        AWS.newExpectedAttributeValue
          & AWS.expectedAttributeValue_comparisonOperator ?~ AWS.ComparisonOperator_EQ
          & AWS.expectedAttributeValue_attributeValueList ?~ [toAttributeValue v]
    item :: Maybe Word32 -> HashMap Text AWS.AttributeValue
    item v =
      HashMap.insert ddbVersion (toAttributeValue (maybe (1 :: Word32) (+ 1) v)) $
        key u c
    toAttributeValue :: Word32 -> AWS.AttributeValue
    toAttributeValue w = AWS.N $ AWS.toText (fromIntegral w :: Int)

    reportAttemptFailure :: Sem r ()
    reportAttemptFailure = Metrics.incCounter optimisticLockGrabAttemptFailedCounter

    reportFailureAndLogError :: Sem r ()
    reportFailureAndLogError = do
      Log.err $
        field "user" (toByteString' u)
          . field "client" (toByteString' c)
          . msg (val "PreKeys: Optimistic lock failed")
      Metrics.incCounter optimisticLockFailedCounter
    execDyn ::
      forall s x.
      (AWS.AWSRequest s) =>
      (AWS.AWSResponse s -> Maybe x) ->
      (Text -> s) ->
      Sem r (Maybe x)
    execDyn cnv mkCmd = do
      cmd <- mkCmd <$> inputs (.prekeyTable)
      e <- inputs (.awsEnv)
      embedFinal $ execDyn' e cnv cmd
      where
        execDyn' ::
          forall y p.
          (AWS.AWSRequest p) =>
          AWS.Env ->
          (AWS.AWSResponse p -> Maybe y) ->
          p ->
          IO (Maybe y)
        execDyn' e conv cmd = recovering policy handlers (const run)
          where
            run = runResourceT (AWS.sendEither e cmd) >>= either handleErr (pure . conv)
            handlers = httpHandlers ++ [const $ EL.handler_ AWS._ConditionalCheckFailedException (pure True)]
            policy = limitRetries 3 <> exponentialBackoff 100000
            handleErr (AWS.ServiceError se) | se ^. AWS.serviceError_code == AWS.ErrorCode "ProvisionedThroughputExceeded" = do
              Prom.incCounter dynProvisionedThroughputExceededCounter
              pure Nothing
            handleErr _ = pure Nothing

execCatch ::
  ( AWS.AWSRequest a,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  AWS.Env ->
  a ->
  m (Either AWS.Error (AWS.AWSResponse a))
execCatch e cmd =
  AWS.runResourceT $
    EL.trying AWS._Error $
      AWS.send e cmd

exec ::
  ( AWS.AWSRequest a,
    MonadCatch m,
    MonadIO m
  ) =>
  AWS.Env ->
  a ->
  m (AWS.AWSResponse a)
exec e cmd = liftIO (execCatch e cmd) >>= either throwM pure

withLocalLock :: MVar () -> IO a -> IO a
withLocalLock l ma = do
  (takeMVar l *> ma) `finally` putMVar l ()

{-# NOINLINE optimisticLockGrabAttemptFailedCounter #-}
optimisticLockGrabAttemptFailedCounter :: Prom.Counter
optimisticLockGrabAttemptFailedCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "client_opt_lock_optimistic_lock_grab_attempt_failed",
          Prom.metricHelp = "Number of times grab attempts for optimisitic lock on prekeys failed"
        }

{-# NOINLINE optimisticLockFailedCounter #-}
optimisticLockFailedCounter :: Prom.Counter
optimisticLockFailedCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "client_opt_lock_optimistic_lock_failed",
          Prom.metricHelp = "Number of time optimisitic lock on prekeys failed"
        }

{-# NOINLINE dynProvisionedThroughputExceededCounter #-}
dynProvisionedThroughputExceededCounter :: Prom.Counter
dynProvisionedThroughputExceededCounter =
  Prom.unsafeRegister $
    Prom.counter
      Prom.Info
        { Prom.metricName = "client_opt_lock_provisioned_throughput_exceeded",
          Prom.metricHelp = "Number of times provisioned throughput on DynamoDB was exceeded"
        }
