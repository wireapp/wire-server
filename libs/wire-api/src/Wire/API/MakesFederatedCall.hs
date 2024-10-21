-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2022 Wire Swiss GmbH <opensource@wire.com>
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
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Wire.API.MakesFederatedCall
  ( MakesFederatedCall,
    Component (..),
    callsFed,
    ShowComponent,
    HasFeds (..),
    FedCallFrom' (..),
    Calls (..),
  )
where

import Control.Lens ((%~))
import Control.Monad.State (State, evalState, get, gets, modify)
import Data.Aeson
import Data.ByteString.Char8 (unpack)
import Data.Constraint
import Data.Kind
import Data.Map qualified as M
import Data.Metrics.Servant
import Data.OpenApi qualified as S
import Data.Proxy
import Data.Schema
import Data.Text qualified as T
import GHC.TypeLits
import Imports
import Servant.API
import Servant.API.Extended (ReqBodyCustomError')
import Servant.API.Extended.RawM qualified as RawM
import Servant.Client
import Servant.Multipart
import Servant.OpenApi
import Test.QuickCheck (Arbitrary)
import Wire.API.Deprecated (Deprecated)
import Wire.API.Error (CanThrow, CanThrowMany)
import Wire.API.Routes.Bearer (Bearer)
import Wire.API.Routes.Cookies (Cookies)
import Wire.API.Routes.LowLevelStream (LowLevelStream)
import Wire.API.Routes.MultiVerb (MultiVerb)
import Wire.API.Routes.Named
import Wire.API.Routes.Public
import Wire.API.Routes.QualifiedCapture (QualifiedCapture', WithDomain)
import Wire.API.Routes.Version
import Wire.API.Routes.Versioned (VersionedReqBody)
import Wire.API.Routes.WebSocket (WebSocketPending)
import Wire.API.SwaggerServant (OmitDocs)
import Wire.Arbitrary (GenericUniform (..))

data Component
  = Brig
  | Galley
  | Cargohold
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform Component)
  deriving (ToJSON, FromJSON) via (Schema Component)

instance ToSchema Component where
  schema =
    enum @Text "Component" $
      mconcat
        [ element "brig" Brig,
          element "galley" Galley,
          element "cargohold" Cargohold
        ]

instance FromHttpApiData Component where
  parseUrlPiece :: Text -> Either Text Component
  parseUrlPiece = \case
    "brig" -> Right Brig
    "galley" -> Right Galley
    "cargohold" -> Right Cargohold
    c -> Left $ "Invalid component: " <> c

instance ToHttpApiData Component where
  toUrlPiece = \case
    Brig -> "brig"
    Galley -> "galley"
    Cargohold -> "cargohold"

-- | A typeclass with the same layout as 'CallsFed', which exists only so we
-- can discharge 'CallsFeds' constraints by unsafely coercing this one.
class Nullary

instance Nullary

-- | Servant combinator for tracking calls to federated calls. Annotating API
-- endpoints with 'MakesFederatedCall' is the only way to eliminate 'CallsFed'
-- constraints on handlers.
data MakesFederatedCall (comp :: Component) (name :: Symbol)

instance (HasLink api) => HasLink (MakesFederatedCall comp name :> api :: Type) where
  type MkLink (MakesFederatedCall comp name :> api) x = MkLink api x
  toLink f _ l = toLink f (Proxy @api) l

instance (RoutesToPaths api) => RoutesToPaths (MakesFederatedCall comp name :> api :: Type) where
  getRoutes = getRoutes @api

-- | Get a symbol representation of our component.
type family ShowComponent (x :: Component) = (res :: Symbol) | res -> x where
  ShowComponent 'Brig = "brig"
  ShowComponent 'Galley = "galley"
  ShowComponent 'Cargohold = "cargohold"

type instance
  SpecialiseToVersion v (MakesFederatedCall comp name :> api) =
    MakesFederatedCall comp name :> SpecialiseToVersion v api

-- | 'MakesFederatedCall' annotates the swagger documentation with an extension
-- tag @x-wire-makes-federated-calls-to@.
instance (HasOpenApi api, KnownSymbol name, KnownSymbol (ShowComponent comp)) => HasOpenApi (MakesFederatedCall comp name :> api :: Type) where
  toOpenApi _ =
    toOpenApi (Proxy @api)
      -- Append federated call line to the description of routes
      -- that perform calls to federation members.
      & S.allOperations
        . S.description
        %~ pure . maybe call (\d -> d <> "<br>" <> call)
    where
      call :: Text
      call =
        T.pack "Calls federation service "
          <> T.pack (symbolVal $ Proxy @(ShowComponent comp))
          <> T.pack " on "
          <> T.pack (symbolVal $ Proxy @name)

instance (HasClient m api) => HasClient m (MakesFederatedCall comp name :> api :: Type) where
  type Client m (MakesFederatedCall comp name :> api) = Client m api
  clientWithRoute p _ = clientWithRoute p $ Proxy @api
  hoistClientMonad p _ f c = hoistClientMonad p (Proxy @api) f c

-- | Type class to automatically lift a function of the form @(c1, c2, ...) =>
-- r@ into @Dict c1 -> Dict c2 -> ... -> r@.
class SolveCallsFed c r a where
  -- | Safely discharge a 'CallsFed' constraint. Intended to be used when
  -- connecting your handler to the server router.
  --
  -- This function should always be called with an argument of
  -- 'exposeAnnotations'. See the documentation there for more information on
  -- why.
  callsFed :: ((c) => r) -> a

instance (c ~ ((k, d) :: Constraint), SolveCallsFed d r a) => SolveCallsFed c r (Dict k -> a) where
  callsFed f Dict = callsFed @d @r @a f

instance {-# OVERLAPPABLE #-} (c ~ (() :: Constraint), r ~ a) => SolveCallsFed c r a where
  callsFed f = f

data FedCallFrom' f = FedCallFrom
  { name :: f String,
    method :: f String,
    fedCalls :: Calls
  }

deriving instance Show (FedCallFrom' Maybe)

deriving instance Show (FedCallFrom' Identity)

type FedCallFrom = FedCallFrom' Maybe

-- Merge the maps, perserving as much unique info as possible.
instance Semigroup (FedCallFrom' Maybe) where
  a <> b =
    FedCallFrom
      (name a <|> name b)
      (method a <|> method b)
      (fedCalls a <> fedCalls b)

instance Semigroup (FedCallFrom' Identity) where
  a <> b =
    FedCallFrom
      (name a)
      (method a)
      (fedCalls a <> fedCalls b)

instance Monoid FedCallFrom where
  mempty = FedCallFrom mempty mempty mempty

newtype Calls = Calls
  { unCalls :: Map String [String]
  }
  deriving (Eq, Ord, Show)

instance Semigroup Calls where
  Calls a <> Calls b = Calls $ M.unionWith (\na nb -> nub . sort $ na <> nb) a b

instance Monoid Calls where
  mempty = Calls mempty

class HasFeds a where
  getFedCalls :: Proxy a -> State FedCallFrom [FedCallFrom]

-- Here onwards are all of the interesting instances that have something we care about
instance (KnownSymbol seg, HasFeds rest) => HasFeds (seg :> rest) where
  getFedCalls _ = do
    let segString = "/" <> T.unpack (T.dropAround (== '"') $ renderSymbol @seg)
    modify $ appendName segString
    getFedCalls $ Proxy @rest

instance (KnownSymbol capture, HasFeds rest) => HasFeds (Capture' mods capture a :> rest) where
  getFedCalls _ = do
    let segString = "/{" <> T.unpack (T.dropAround (== '"') $ renderSymbol @capture) <> "}"
    modify $ appendName segString
    getFedCalls $ Proxy @rest

instance (KnownSymbol capture, KnownSymbol (AppendSymbol capture "_domain"), HasFeds rest) => HasFeds (QualifiedCapture' mods capture a :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @(WithDomain mods capture a rest)

instance (ReflectMethod method) => HasFeds (LowLevelStream method status headers desc ctype) where
  getFedCalls _ = do
    modify $ \s -> s {method = getMethod @method}
    gets pure

instance (HasFeds rest, KnownSymbol (ShowComponent comp), KnownSymbol name) => HasFeds (MakesFederatedCall comp name :> rest) where
  getFedCalls _ = do
    let call =
          M.singleton
            (symbolVal $ Proxy @(ShowComponent comp))
            (pure (symbolVal $ Proxy @name))
    modify $ \s -> s {fedCalls = fedCalls s <> Calls call}
    getFedCalls $ Proxy @rest

instance (ReflectMethod method) => HasFeds (MultiVerb method cs as r) where
  getFedCalls _ = do
    modify $ \s -> s {method = getMethod @method}
    gets pure

instance (ReflectMethod method) => HasFeds (Verb method status cts a) where
  getFedCalls _ = do
    modify $ \s -> s {method = getMethod @method}
    gets pure

instance (ReflectMethod method) => HasFeds (NoContentVerb method) where
  getFedCalls _ = do
    modify $ \s -> s {method = getMethod @method}
    gets pure

instance (ReflectMethod method) => HasFeds (Stream method status framing ct a) where
  getFedCalls _ = do
    modify $ \s -> s {method = getMethod @method}
    gets pure

instance HasFeds WebSocketPending where
  getFedCalls _ = do
    modify $ \s -> s {method = pure $ show GET}
    gets pure

instance (HasFeds route, HasFeds routes) => HasFeds (route :<|> routes) where
  getFedCalls _ = do
    s <- get
    -- Use what state we have up until now, as it might be a funky style of endpoint.
    -- Routes will usually specify their own name, as we don't have a style of sharing
    -- a route name between several HTTP methods.
    let a = evalState (getFedCalls $ Proxy @route) s
        b = evalState (getFedCalls $ Proxy @routes) s
    pure $ a <> b

instance HasFeds EmptyAPI where
  getFedCalls _ = gets pure

instance HasFeds Raw where
  getFedCalls _ = gets pure

instance HasFeds RawM.RawM where
  getFedCalls _ = gets pure

getMethod :: forall method. (ReflectMethod method) => Maybe String
getMethod = pure . fmap toLower . unpack . reflectMethod $ Proxy @method

appendName :: String -> FedCallFrom -> FedCallFrom
appendName toAppend s = s {name = pure $ maybe toAppend (<> toAppend) $ name s}

-- All of the boring instances live here.
instance (RenderableSymbol name, HasFeds rest) => HasFeds (Named name rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (Header' mods name a :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (ReqBody' mods cts a :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (StreamBody' opts framing ct a :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (Summary summary :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (QueryParam' mods name a :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (MultipartForm tag a :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (QueryFlag a :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (Description desc :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (Deprecated :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (CanThrow e :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (CanThrowMany es :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (Bearer a :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (Cookies cs :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (ZHostOpt :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (ZAuthServant ztype opts :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (ReqBodyCustomError' mods cts tag a :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (DescriptionOAuthScope scope :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (OmitDocs :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest

instance (HasFeds rest) => HasFeds (VersionedReqBody v cts a :> rest) where
  getFedCalls _ = getFedCalls $ Proxy @rest
