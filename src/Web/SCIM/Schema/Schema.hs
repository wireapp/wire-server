{-# LANGUAGE OverloadedStrings #-}

module Web.SCIM.Schema.Schema where

import Data.Aeson

data Schema = User20
            | ListResponse2_0
  deriving (Show, Eq)

instance FromJSON Schema where
  parseJSON = withText "schema" $ \s -> case s of
    "urn:ietf:params:scim:schemas:core:2.0:User" ->
      pure User20
    "urn:ietf:params:scim:api:messages:2.0:ListResponse" ->
      pure ListResponse2_0
    _ ->
      fail "unsupported schema" 

instance ToJSON Schema where
  toJSON User20 =
    "urn:ietf:params:scim:schemas:core:2.0:User"
  toJSON ListResponse2_0 =
    "urn:ietf:params:scim:api:messages:2.0:ListResponse"
