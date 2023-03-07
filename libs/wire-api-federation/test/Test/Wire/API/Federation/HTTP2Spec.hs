module Test.Wire.API.Federation.HTTP2Spec where

import Control.Concurrent.Async
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Imports
import Network.HTTP.Types
import qualified Network.HTTP2.Client as HTTP2
import Test.Hspec
import Wire.API.Federation.HTTP2

spec :: Spec
spec = do
  describe "HTTP2 Client Manager" $ do
    it "should allow accessing data over HTTP2" $ do
      mgr <- defaultHTTP2Manager
      let req = HTTP2.requestNoBody "GET" "/httpbin/base64/Zm9v" mempty
      -- TODO: This service still responds to HTTP 1.1, find a better server or
      -- spin one up here, preferably don't write it using the http2 library.
      withHTTP2Request mgr "nghttp2.org" 80 req $ \res -> do
        HTTP2.responseStatus res `shouldBe` Just status200
        bdy <- fmap LBS.fromChunks . getBS $ HTTP2.getResponseBodyChunk res
        -- Zm9v decodes to foo, just trust me.
        bdy `shouldBe` "foo"

    it "should allow accessing multiple things over HTTP2 sequentially" $ do
      mgr <- defaultHTTP2Manager
      let reqFoo = HTTP2.requestNoBody "GET" "/httpbin/base64/Zm9v" mempty
      -- TODO: This service still responds to HTTP 1.1, find a better server or
      -- spin one up here, preferably don't write it using the http2 library.
      withHTTP2Request mgr "nghttp2.org" 80 reqFoo $ \res -> do
        HTTP2.responseStatus res `shouldBe` Just status200
        bdy <- fmap LBS.fromChunks . getBS $ HTTP2.getResponseBodyChunk res
        -- Zm9v decodes to foo, just trust me.
        bdy `shouldBe` "foo"

      -- TODO: How do we assert that this reuses the connection? Perhaps a
      -- better HTTP2 server _is_ needed?
      let reqBar = HTTP2.requestNoBody "GET" "/httpbin/base64/YmFy" mempty
      withHTTP2Request mgr "nghttp2.org" 80 reqBar $ \res -> do
        HTTP2.responseStatus res `shouldBe` Just status200
        bdy <- fmap LBS.fromChunks . getBS $ HTTP2.getResponseBodyChunk res
        -- Zm9v decodes to foo, just trust me.
        bdy `shouldBe` "bar"

    it "should allow accessing multiple things over HTTP2 concurrently" $ do
      mgr <- defaultHTTP2Manager
      let req = HTTP2.requestNoBody "GET" "/httpbin/base64/Zm9v" mempty
      -- TODO: This service still responds to HTTP 1.1, find a better server or
      -- spin one up here, preferably don't write it using the http2 library.
      let assertFoo = withHTTP2Request mgr "nghttp2.org" 80 req $ \res -> do
            HTTP2.responseStatus res `shouldBe` Just status200
            bdy <- fmap LBS.fromChunks . getBS $ HTTP2.getResponseBodyChunk res
            -- Zm9v decodes to foo, just trust me.
            bdy `shouldBe` "foo"

      -- TODO: How do we assert that this reuses the connection? Perhaps a
      -- better HTTP2 server _is_ needed?
      let reqBar = HTTP2.requestNoBody "GET" "/httpbin/base64/YmFy" mempty
      let assertBar = withHTTP2Request mgr "nghttp2.org" 80 reqBar $ \res -> do
            HTTP2.responseStatus res `shouldBe` Just status200
            bdy <- fmap LBS.fromChunks . getBS $ HTTP2.getResponseBodyChunk res
            -- Zm9v decodes to foo, just trust me.
            bdy `shouldBe` "bar"
      concurrently_ assertFoo assertBar

getBS :: IO ByteString -> IO [ByteString]
getBS action = do
  bs <- action
  if BS.null bs
    then pure [bs]
    else (bs :) <$> getBS action
