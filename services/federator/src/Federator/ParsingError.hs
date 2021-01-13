{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Federator.ParsingError where

import qualified Control.Concurrent.Async as Async
import qualified Data.Text as T
import Federator.GRPC.ProtoExample
import GHC.IO (finally)
import Imports
import Mu.GRpc.Client.Record
import Mu.GRpc.Client.TyApps
import Mu.GRpc.Server
import Mu.Server hiding (resolver)
import Network.HTTP2.Client

------------------------------------------------------------------------------
-- app

runServer :: IO ()
runServer = do
  runGRpcApp msgProtoBuf 8070 grpcServer

------------------------------------------------------------------------------
-- grpc server api

grpcServer :: MonadServer m => SingleServerT i Service m _
grpcServer =
  singleService
    ( method @"SayHello" sayHello
    )

sayHello :: MonadServer m => HelloRequestMessage -> m HelloReplyMessage
sayHello (HelloRequestMessage nm) = do
  case nm of
    -- in some cases we want to throw an error, here when the name sent is 'Bob'
    "Bob" -> throwError $ ServerError NotFound "Bob not there"
    _ -> pure $ HelloReplyMessage ("hi, " <> nm)

------------------------------------------------------------------------------
-- grpc client calls

outboundSayHello' :: HostName -> PortNumber -> T.Text -> IO (GRpcReply Text)
outboundSayHello' host port req = do
  attempt <- liftIO $ setupGrpcClient' (grpcClientConfigSimple host port False)
  case attempt of
    Right c -> do
      x <- fmap (\(HelloReplyMessage r) -> r) <$> outBoundSayHello c (HelloRequestMessage req)
      pure x
    _ -> undefined

outBoundSayHello :: GrpcClient -> HelloRequestMessage -> IO (GRpcReply HelloReplyMessage)
outBoundSayHello = gRpcCall @'MsgProtoBuf @Service @"Service" @"SayHello"

------------------------------------------------------------------------------
-- test

runTest :: IO ()
runTest = do
  putStrLn "testing things..."
  aliceResult <- outboundSayHello' "127.0.0.1" 8070 "Alice"
  putStr "The result for saying hello to Alice: "
  print (show aliceResult)
  bobResult <- outboundSayHello' "127.0.0.1" 8070 "Bob"
  -- bobResult should give a valid error from the server (NotFound), instead of a generic "not enough bytes"
  putStr "The result for saying hello to Bob: "
  print bobResult
  putStrLn "done with the haskell test, waiting 10 seconds before shutting down..."
  threadDelay $ 10 * 1000000 -- wait 10 seconds to allow running grpcurl against the server, too.

main :: IO ()
main = do
  server <- Async.async runServer
  finally runTest (Async.cancel server)
