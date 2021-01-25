{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

module Wire.API.Federation.GRPC.Types where

-- TODO: Replace Wire.API.Federation.GRPC.Proto with this

import Data.Bifunctor (Bifunctor (first))
import Data.Domain (Domain (..), mkDomain)
import Data.Either.Validation
import Data.List.NonEmpty (NonEmpty ((:|)))
import Imports
import Mu.Quasi.GRpc (grpc)
import Mu.Schema
import qualified Network.HTTP.Types as HTTP
import Test.QuickCheck (Arbitrary (..), elements)
import Wire.API.Arbitrary (GenericUniform (..))
import Wire.API.Federation.GRPC.Helper

recompileRouterUponProtoChanges

grpc "Router" id routerProtoFile

data Component
  = UnspecifiedComponent
  | Brig
  deriving (Show, Eq, Generic, ToSchema Router "Component", FromSchema Router "Component")
  deriving (Arbitrary) via (GenericUniform Component)

-- | FUTUREWORK: Make this a better ADT for the errors
data Response
  = ResponseOk ByteString
  | ResponseErr Text
  deriving (Show, Eq)

instance ToSchema Router "Response" Response where
  toSchema r =
    let protoChoice = case r of
          (ResponseOk res) -> Z (FPrimitive res)
          (ResponseErr e) -> S (Z (FPrimitive e))
     in TRecord (Field (FUnion protoChoice) :* Nil)

instance FromSchema Router "Response" Response where
  fromSchema (TRecord (Field (FUnion protoChoice) :* Nil)) =
    case protoChoice of
      Z (FPrimitive res) -> ResponseOk res
      S (Z (FPrimitive e)) -> ResponseErr e
      S (S x) ->
        -- I don't understand why this empty case is needed, but there is some
        -- explanation here:
        -- https://github.com/well-typed/generics-sop/issues/116
        case x of

-- | This type exists to avoid orphan instances of ToSchema and FromSchema
newtype HTTPMethod = HTTPMethod {unwrapMethod :: HTTP.StdMethod}
  deriving (Eq, Show, Generic)

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

-- TODO: Write roundtrip tests
-- TODO: The instances seem to be wrong, at least they don't work with grpcui
instance ToSchema Router "Method" HTTPMethod where
  toSchema (HTTPMethod m) =
    let enumChoice = case m of
          HTTP.GET -> S (Z Proxy)
          HTTP.POST -> S (S (Z Proxy))
          HTTP.HEAD -> S (S (S (Z Proxy)))
          HTTP.PUT -> S (S (S (S (Z Proxy))))
          HTTP.DELETE -> S (S (S (S (S (Z Proxy)))))
          HTTP.TRACE -> S (S (S (S (S (S (Z Proxy))))))
          HTTP.CONNECT -> S (S (S (S (S (S (S (Z Proxy)))))))
          HTTP.OPTIONS -> S (S (S (S (S (S (S (S (Z Proxy))))))))
          HTTP.PATCH -> S (S (S (S (S (S (S (S (S (Z Proxy)))))))))
     in TEnum enumChoice

instance FromSchema Router "Method" HTTPMethod where
  fromSchema (TEnum enumChoice) =
    let m = case enumChoice of
          Z _ ->
            -- This is due to this bug
            -- https://github.com/higherkindness/mu-haskell/issues/282
            error "router:method:unspecified is impossible"
          S (Z _) -> HTTP.GET
          S (S (Z _)) -> HTTP.POST
          S (S (S (Z _))) -> HTTP.HEAD
          S (S (S (S (Z _)))) -> HTTP.PUT
          S (S (S (S (S (Z _))))) -> HTTP.DELETE
          S (S (S (S (S (S (Z _)))))) -> HTTP.TRACE
          S (S (S (S (S (S (S (Z _))))))) -> HTTP.CONNECT
          S (S (S (S (S (S (S (S (Z _)))))))) -> HTTP.OPTIONS
          S (S (S (S (S (S (S (S (S (Z _))))))))) -> HTTP.PATCH
          S (S (S (S (S (S (S (S (S (S x))))))))) -> case x of
     in HTTPMethod m

data QueryParam = QueryParam
  { key :: ByteString,
    value :: ByteString
  }
  deriving (Eq, Show, Generic, ToSchema Router "QueryParam", FromSchema Router "QueryParam")
  deriving (Arbitrary) via (GenericUniform QueryParam)

-- Does this make it hard to use in a type checked way?
data LocalCall = LocalCall
  { component :: Maybe Component,
    method :: Maybe HTTPMethod,
    path :: ByteString,
    query :: [QueryParam],
    body :: ByteString
  }
  deriving (Eq, Show, Generic, ToSchema Router "LocalCall", FromSchema Router "LocalCall")
  deriving (Arbitrary) via (GenericUniform LocalCall)

-- | This type exists because Enums are wrongly marked optional by mu-protobuf
-- Bug: https://github.com/higherkindness/mu-haskell/issues/282
data ValidatedLocalCall = ValidatedLocalCall
  { vComponent :: Component,
    vMethod :: HTTP.StdMethod,
    vPath :: ByteString,
    vQuery :: [QueryParam],
    vBody :: ByteString
  }
  deriving (Show, Eq)

validatedLocalCallToLocalCall :: ValidatedLocalCall -> LocalCall
validatedLocalCallToLocalCall ValidatedLocalCall {..} = LocalCall (Just vComponent) (Just (HTTPMethod vMethod)) vPath vQuery vBody

data LocalCallValidationError
  = ComponentMissing
  | MethodMissing
  deriving (Show, Eq)

validateLocalCall :: LocalCall -> Validation (NonEmpty LocalCallValidationError) ValidatedLocalCall
validateLocalCall LocalCall {..} = do
  let vPath = path
      vQuery = query
      vBody = body
  vComponent <- validateComponent component
  vMethod <- validateMethod method
  pure $ ValidatedLocalCall {..}
  where
    validateComponent :: Maybe Component -> Validation (NonEmpty LocalCallValidationError) Component
    validateComponent Nothing = Failure $ ComponentMissing :| []
    validateComponent (Just UnspecifiedComponent) = Failure $ ComponentMissing :| []
    validateComponent (Just c) = Success c

    validateMethod :: Maybe HTTPMethod -> Validation (NonEmpty LocalCallValidationError) HTTP.StdMethod
    validateMethod Nothing = Failure $ MethodMissing :| []
    validateMethod (Just (HTTPMethod m)) = Success m

data RemoteCall = RemoteCall
  { domain :: Text,
    localCall :: Maybe LocalCall
  }
  deriving (Eq, Show, Generic, ToSchema Router "RemoteCall", FromSchema Router "RemoteCall")

data RemoteCallValidationError
  = InvalidDomain String
  | LocalCallMissing
  | InvalidLocalCall (NonEmpty LocalCallValidationError)
  deriving (Show, Eq)

data ValidatedRemoteCall = ValidatedRemoteCall
  { vDomain :: Domain,
    vLocalCall :: ValidatedLocalCall
  }
  deriving (Eq, Show)

validateRemoteCall :: RemoteCall -> Validation (NonEmpty RemoteCallValidationError) ValidatedRemoteCall
validateRemoteCall RemoteCall {..} = do
  vDomain <- validateDomain
  vLocalCall <- validateLocalPart
  pure $ ValidatedRemoteCall {..}
  where
    validateDomain = case mkDomain domain of
      Left str -> Failure $ InvalidDomain str :| []
      Right d -> Success d
    validateLocalPart = case localCall of
      Nothing -> Failure $ LocalCallMissing :| []
      Just lc -> first (\x -> InvalidLocalCall x :| []) $ validateLocalCall lc
