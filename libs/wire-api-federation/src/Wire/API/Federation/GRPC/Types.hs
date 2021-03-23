{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

module Wire.API.Federation.GRPC.Types where

import Data.Domain (Domain (..), domainText, mkDomain)
import Data.Either.Validation
import Data.List.NonEmpty (NonEmpty ((:|)))
import Imports
import Mu.Quasi.GRpc (grpc)
import Mu.Schema
import qualified Network.HTTP.Types as HTTP
import Test.QuickCheck (Arbitrary (..), elements)
import Wire.API.Arbitrary (GenericUniform (..))
import Wire.API.Federation.GRPC.Helper

-- Note: To understand the purpose of this file, please see the documentation of
-- mu-haskell: https://higherkindness.io/mu-haskell/

-- | Whenever proto/router.proto is changed, we want this file to recompile
-- In the case of a change to one of the existing types, this will then lead to
-- a compilation error (which we want to see at compilation time to keep the
-- haskell code, the mu schema, and the protobuf file in sync)
recompileRouterUponProtoChanges

grpc "Router" id routerProtoFile

data Component
  = Brig
  deriving (Typeable, Show, Eq, Generic, ToSchema Router "Component", FromSchema Router "Component")
  deriving (Arbitrary) via (GenericUniform Component)

data HTTPResponse = HTTPResponse
  { responseStatus :: Word32,
    responseBody :: ByteString
  }
  deriving (Typeable, Show, Eq, Generic, ToSchema Router "HTTPResponse", FromSchema Router "HTTPResponse")
  deriving (Arbitrary) via (GenericUniform HTTPResponse)

-- | FUTUREWORK(federation): Make this a better ADT for the errors
data InwardResponse
  = InwardResponseHTTPResponse HTTPResponse
  | InwardResponseErr Text
  deriving (Typeable, Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform InwardResponse)

instance ToSchema Router "InwardResponse" InwardResponse where
  toSchema r =
    let protoChoice = case r of
          (InwardResponseHTTPResponse res) -> Z (FSchematic (toSchema res))
          (InwardResponseErr e) -> S (Z (FPrimitive e))
     in TRecord (Field (FUnion protoChoice) :* Nil)

instance FromSchema Router "InwardResponse" InwardResponse where
  fromSchema (TRecord (Field (FUnion protoChoice) :* Nil)) =
    case protoChoice of
      Z (FSchematic res) -> InwardResponseHTTPResponse $ fromSchema res
      S (Z (FPrimitive e)) -> InwardResponseErr e
      S (S x) ->
        -- I don't understand why this empty case is needed, but there is some
        -- explanation here:
        -- https://github.com/well-typed/generics-sop/issues/116
        case x of

data OutwardResponse
  = OutwardResponseHTTPResponse HTTPResponse
  | OutwardResponseError OutwardError
  deriving (Typeable, Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform OutwardResponse)

instance ToSchema Router "OutwardResponse" OutwardResponse where
  toSchema r =
    let protoChoice = case r of
          OutwardResponseHTTPResponse res -> Z (FSchematic (toSchema res))
          OutwardResponseError err -> S (Z (FSchematic (toSchema err)))
     in TRecord (Field (FUnion protoChoice) :* Nil)

instance FromSchema Router "OutwardResponse" OutwardResponse where
  fromSchema (TRecord (Field (FUnion protoChoice) :* Nil)) =
    case protoChoice of
      Z (FSchematic res) -> OutwardResponseHTTPResponse $ fromSchema res
      S (Z (FSchematic err)) -> OutwardResponseError $ fromSchema err
      S (S x) -> case x of

type OutwardErrorFieldMapping =
  '[ "outwardErrorType" ':-> "type",
     "outwardErrorPayload" ':-> "payload"
   ]

data OutwardError = OutwardError
  { outwardErrorType :: OutwardErrorType,
    outwardErrorPayload :: Maybe ErrorPayload
  }
  deriving (Typeable, Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform OutwardError)
  deriving
    (ToSchema Router "OutwardError", FromSchema Router "OutwardError")
    via (CustomFieldMapping "OutwardError" OutwardErrorFieldMapping OutwardError)

data OutwardErrorType
  = RemoteNotFound
  | DiscoveryFailed
  | ConnectionRefused
  | TLSFailure
  | InvalidCertificate
  | VersionMismatch
  | FederationDeniedByRemote
  | FederationDeniedLocally
  | RemoteFederatorError
  | InvalidRequest
  deriving (Typeable, Show, Eq, Generic, ToSchema Router "OutwardError.ErrorType", FromSchema Router "OutwardError.ErrorType")
  deriving (Arbitrary) via (GenericUniform OutwardErrorType)

data ErrorPayload = ErrorPayload
  { label :: Text,
    msg :: Text
  }
  deriving (Typeable, Show, Eq, Generic, ToSchema Router "ErrorPayload", FromSchema Router "ErrorPayload")
  deriving (Arbitrary) via (GenericUniform ErrorPayload)

-- | This type exists to avoid orphan instances of ToSchema and FromSchema
newtype HTTPMethod = HTTPMethod {unwrapMethod :: HTTP.StdMethod}
  deriving (Typeable, Eq, Show, Generic)

instance Arbitrary HTTPMethod where
  arbitrary =
    HTTPMethod
      <$> elements
        [ HTTP.GET,
          HTTP.POST,
          HTTP.HEAD,
          HTTP.PUT,
          HTTP.DELETE,
          HTTP.TRACE,
          HTTP.CONNECT,
          HTTP.OPTIONS,
          HTTP.PATCH
        ]

instance ToSchema Router "Method" HTTPMethod where
  toSchema (HTTPMethod m) =
    let enumChoice = case m of
          HTTP.GET -> Z Proxy
          HTTP.POST -> S (Z Proxy)
          HTTP.HEAD -> S (S (Z Proxy))
          HTTP.PUT -> S (S (S (Z Proxy)))
          HTTP.DELETE -> S (S (S (S (Z Proxy))))
          HTTP.TRACE -> S (S (S (S (S (Z Proxy)))))
          HTTP.CONNECT -> S (S (S (S (S (S (Z Proxy))))))
          HTTP.OPTIONS -> S (S (S (S (S (S (S (Z Proxy)))))))
          HTTP.PATCH -> S (S (S (S (S (S (S (S (Z Proxy))))))))
     in TEnum enumChoice

instance FromSchema Router "Method" HTTPMethod where
  fromSchema (TEnum enumChoice) =
    let m = case enumChoice of
          Z _ -> HTTP.GET
          S (Z _) -> HTTP.POST
          S (S (Z _)) -> HTTP.HEAD
          S (S (S (Z _))) -> HTTP.PUT
          S (S (S (S (Z _)))) -> HTTP.DELETE
          S (S (S (S (S (Z _))))) -> HTTP.TRACE
          S (S (S (S (S (S (Z _)))))) -> HTTP.CONNECT
          S (S (S (S (S (S (S (Z _))))))) -> HTTP.OPTIONS
          S (S (S (S (S (S (S (S (Z _)))))))) -> HTTP.PATCH
          S (S (S (S (S (S (S (S (S x)))))))) -> case x of
     in HTTPMethod m

data QueryParam = QueryParam
  { key :: ByteString,
    value :: ByteString
  }
  deriving (Typeable, Eq, Show, Generic, ToSchema Router "QueryParam", FromSchema Router "QueryParam")
  deriving (Arbitrary) via (GenericUniform QueryParam)

-- Does this make it hard to use in a type checked way?
data Request = Request
  { component :: Component,
    method :: HTTPMethod,
    path :: ByteString,
    query :: [QueryParam],
    body :: ByteString
  }
  deriving (Typeable, Eq, Show, Generic, ToSchema Router "Request", FromSchema Router "Request")
  deriving (Arbitrary) via (GenericUniform Request)

data RequestValidationError
  = ComponentMissing
  | MethodMissing
  deriving (Typeable, Show, Eq)

data FederatedRequest = FederatedRequest
  { domain :: Text,
    request :: Maybe Request
  }
  deriving (Typeable, Eq, Show, Generic, ToSchema Router "FederatedRequest", FromSchema Router "FederatedRequest")
  deriving (Arbitrary) via (GenericUniform FederatedRequest)

data FederatedRequestValidationError
  = InvalidDomain String
  | RequestMissing
  deriving (Typeable, Show, Eq)

data ValidatedFederatedRequest = ValidatedFederatedRequest
  { vDomain :: Domain,
    vRequest :: Request
  }
  deriving (Typeable, Eq, Show)

validateFederatedRequest :: FederatedRequest -> Validation (NonEmpty FederatedRequestValidationError) ValidatedFederatedRequest
validateFederatedRequest FederatedRequest {..} = do
  vDomain <- validateDomain
  vRequest <- validateLocalPart
  pure $ ValidatedFederatedRequest {..}
  where
    validateDomain = case mkDomain domain of
      Left str -> Failure $ InvalidDomain str :| []
      Right d -> Success d
    validateLocalPart = case request of
      Nothing -> Failure $ RequestMissing :| []
      Just lc -> Success lc

validatedFederatedRequestToFederatedRequest :: ValidatedFederatedRequest -> FederatedRequest
validatedFederatedRequestToFederatedRequest ValidatedFederatedRequest {..} = FederatedRequest (domainText vDomain) (Just vRequest)
