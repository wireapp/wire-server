{-# LANGUAGE OverloadedStrings #-}

module Test.DNSMock where

import Control.Lens
import Control.Monad.Reader.Class
import qualified Data.ByteString.Lazy as LBS
import Network.DNS
import Network.DNS.Decode as Dec
import qualified Network.HTTP.Client as HTTP
import Testlib.Prelude

type LByteString = LBS.ByteString

-- | Test that we can provide and lookup a TXT record in
-- Technitium (dns-server for tests)
testNewTXTRecord :: (HasCallStack) => App ()
testNewTXTRecord = do
  tok <- getTechnitiumApiKey
  setTechnitiumReverseProxyACL tok "0.0.0.0/0"
  registerTechnitiumZone tok "example.com"
  registerTechnitiumRecord tok "example.com" "example.com" "TXT" "we own this domain and we're the good guys, trust us!"

  dohUrl <- technitiumDohUrl
  let question = HTTP.RequestBodyBS $ encodeQuestion 0 (Question "example.com" TXT) mempty
  req <- externalRequest dohUrl <&> addBody question "application/dns-message" . addHeader "Accept" "application/dns-message"
  bindResponse (submit "POST" req) $ \resp -> do
    let received :: Either DNSError DNSMessage = Dec.decode (resp.body :: ByteString)
        expected :: Either DNSError DNSMessage = Right (DNSMessage {header = DNSHeader {identifier = 0, flags = DNSFlags {qOrR = QR_Response, opcode = OP_STD, authAnswer = True, trunCation = False, recDesired = True, recAvailable = True, rcode = NoErr, authenData = False, chkDisable = False}}, ednsHeader = EDNSheader (EDNS {ednsVersion = 0, ednsUdpSize = 1232, ednsDnssecOk = False, ednsOptions = []}), question = [Question {qname = "example.com.", qtype = TXT}], answer = [ResourceRecord {rrname = "example.com.", rrtype = TXT, rrclass = 1, rrttl = 3600, rdata = RD_TXT "we own this domain and we're the good guys, trust us!"}], authority = [], additional = []})
    -- if we had aeson instances for DNSError and DNSMessage, we could get nicer error messages here, but meh.
    show received `shouldMatch` show expected

technitiumDohUrl :: App String
technitiumDohUrl = do
  env <- ask
  pure $ "http://" <> env.dnsMockServerConfig.host <> ":" <> show env.dnsMockServerConfig.dohPort <> "/dns-query"

technitiumApiUrl :: App String
technitiumApiUrl = do
  env <- ask
  pure $ "http://" <> env.dnsMockServerConfig.host <> ":" <> show env.dnsMockServerConfig.apiPort <> "/api"

getTechnitiumApiKey :: (HasCallStack) => App String
getTechnitiumApiKey = do
  tok <- requestTechnitiumApiKey
  setTechnitiumReverseProxyACL tok "0.0.0.0/0"
  pure tok

requestTechnitiumApiKey :: (HasCallStack) => App String
requestTechnitiumApiKey = do
  url <- technitiumApiUrl <&> (<> "/user/createToken")
  req <- externalRequest url <&> addQueryParams [("user", "admin"), ("pass", "admin"), ("tokenName", "someToken")]
  bindResponse (submit "POST" req) $ \resp -> do
    resp.status `shouldMatchInt` 200
    resp.jsonBody %. "status" `shouldMatch` ("ok" :: String)
    asString $ resp.jsonBody %. "token"

setTechnitiumReverseProxyACL :: (HasCallStack) => String -> String -> App ()
setTechnitiumReverseProxyACL tok acl = do
  url <- technitiumApiUrl <&> (<> "/settings/set")
  req <- externalRequest url <&> addQueryParams [("token", tok), ("reverseProxyNetworkACL", acl)]
  submit "POST" req >>= assertStatus 200

registerTechnitiumZone :: (HasCallStack) => String -> String -> App ()
registerTechnitiumZone tok zone = do
  url <- technitiumApiUrl <&> (<> "/zones/create")
  req <- externalRequest url <&> addQueryParams [("token", tok), ("zone", zone), ("type", "primary")]
  submit "POST" req >>= assertStatus 200

registerTechnitiumRecord :: (HasCallStack) => String -> String -> String -> String -> String -> App ()
registerTechnitiumRecord tok zone domain typ text = do
  url <- technitiumApiUrl <&> (<> "/zones/records/add")
  let params =
        [ ("token", tok),
          ("zone", zone),
          ("domain", domain),
          ("type", typ),
          ("text", text)
        ]
  req <- externalRequest url <&> addQueryParams params
  submit "POST" req >>= assertStatus 200

deleteTechnitiumRecord :: (HasCallStack) => String -> String -> String -> String -> String -> App ()
deleteTechnitiumRecord tok zone domain typ text = do
  url <- technitiumApiUrl <&> (<> "/zones/records/delete")
  let params =
        [ ("token", tok),
          ("zone", zone),
          ("domain", domain),
          ("type", typ),
          ("text", text)
        ]
  req <- externalRequest url <&> addQueryParams params
  submit "POST" req >>= assertStatus 200
