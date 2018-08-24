{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Capabilities.MetaSchemaSpec (spec) where

import           Data.Aeson hiding (json)
import qualified Data.HashMap.Strict as SMap
import           Data.Maybe (catMaybes)
import qualified Data.Vector as Vector
import           Web.SCIM.Capabilities.MetaSchema
import           Network.Wai.Test (SResponse (..))
import           Servant
import           Servant.Generic
import           Test.Hspec hiding (shouldSatisfy)
import qualified Test.Hspec.Expectations as Expect
import           Test.Hspec.Wai      hiding (post, put, patch)
import           Test.Hspec.Wai.JSON

server :: Proxy (ToServant (ConfigAPI AsApi))
server = Proxy

app :: Application
app = serve server $ toServant $ configServer empty

shouldSatisfy :: (Show a, FromJSON a) =>
                 WaiSession SResponse -> (a -> Bool) -> WaiExpectation
shouldSatisfy resp predicate = do
  maybeDecoded <- decode . simpleBody <$> resp
  case maybeDecoded of
    Nothing -> liftIO $ Expect.expectationFailure "decode error"
    (Just decoded) -> liftIO $ Expect.shouldSatisfy decoded predicate


coreSchemas :: [Value]
coreSchemas = String <$>
  [ "urn:ietf:params:scim:schemas:core:2.0:User"
  , "urn:ietf:params:scim:schemas:core:2.0:ServiceProviderConfig"
  , "urn:ietf:params:scim:schemas:core:2.0:Group"
  , "urn:ietf:params:scim:schemas:core:2.0:Schema"
  , "urn:ietf:params:scim:schemas:core:2.0:ResourceType"
  ]

hasSchemas :: Value -> Bool
-- TODO: these needs to be sorted, but there's no Ord on Value
-- (and I'm lazy)
hasSchemas (Array arr) = coreSchemas == getSchemaIds arr
hasSchemas _ = False

getSchemaIds :: Array -> [Value]
getSchemaIds arr = catMaybes . Vector.toList $ getSchemaId <$> arr

getSchemaId :: Value -> Maybe Value
getSchemaId (Object o) = SMap.lookup "id" o
getSchemaId _ = Nothing

spec :: Spec
spec = with (pure app) $ do
  describe "GET /Schemas" $ do
    it "lists schemas" $ do
      get "/Schemas" `shouldRespondWith` 200
      get "/Schemas" `shouldSatisfy` hasSchemas

  describe "GET /Schemas/:id" $ do
    it "returns valid schema" $ do
      -- TODO: (partially) verify content
      get "/Schemas/urn:ietf:params:scim:schemas:core:2.0:User"
        `shouldRespondWith` 200
    it "returns 404 on unknown schemaId" $ do
      get "/Schemas/unknown" `shouldRespondWith` 404

  describe "GET /ServiceProviderConfig" $ do
    it "returns configuration" $ do
      get "/ServiceProviderConfig" `shouldRespondWith` spConfig


-- FIXME: missing some "supported" fields and URI should not be null
spConfig :: ResponseMatcher
spConfig = [json|
{"documentationUri":null,
 "etag":{"supported":false},
 "bulk":{"maxOperations":0,
         "maxPayloadSize":0,
         "supported":false
        },
 "patch":{"supported":false},
 "authenticationSchemes":[],
 "changePassword":{"supported":false},
 "sort":{"supported":false},
 "filter":{"maxResults":0,
           "supported":false
          }
}
|]
