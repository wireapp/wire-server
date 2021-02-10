module Federator.Brig where

-- Is there is a point in creating an effect for each service?
--
-- FUTUREWORK: Once we authenticate the call, we should send authentication data
-- to brig so brig can do some authorization as required.

import qualified Bilge as RPC
import Bilge.RPC (rpc')
import Bilge.Retry (rpcHandlers)
import Control.Lens (view)
import Control.Retry (RetryPolicy, exponentialBackoff, limitRetries, recovering)
import Federator.App (Federator)
import Federator.Types (brig)
import Federator.UnliftExcept ()
import Imports
import qualified Network.HTTP.Types as HTTP
import Polysemy
import Wire.API.Federation.GRPC.Types

data Brig m a where
  -- | Returns status and body, 'HTTP.Response' is not nice to work with in tests
  BrigCall :: HTTP.StdMethod -> ByteString -> [QueryParam] -> ByteString -> Brig m (HTTP.Status, Maybe LByteString)

makeSem ''Brig

-- This can realistically only be tested in an integration test
-- FUTUREWORK: Do we want to use servant client here? May make everything typed and safe
interpretBrig ::
  Member (Embed Federator) r =>
  Sem (Brig ': r) a ->
  Sem r a
interpretBrig = interpret $ \case
  BrigCall m p q b -> embed @Federator $ do
    brigReq <- view brig <$> ask
    let theCall =
          rpc' "brig" brigReq $
            RPC.method m
              . RPC.path ("federation/" <> p) -- FUTUREWORK: Protect against path traversal
              . RPC.query (map (\(QueryParam k v) -> (k, Just v)) q)
              . RPC.body (RPC.RequestBodyBS b)
    res <-
      case m of
        -- FUTUREWORK: Maybe other HTTP methods can also be retried, this is the
        -- only usecase as of now and seems safe.
        HTTP.GET -> recovering x3 rpcHandlers $ const theCall
        _ -> theCall
    pure (RPC.responseStatus res, RPC.responseBody res)

x3 :: RetryPolicy
x3 = limitRetries 3 <> exponentialBackoff 100000
