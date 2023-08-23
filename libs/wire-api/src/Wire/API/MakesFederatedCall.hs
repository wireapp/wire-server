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
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Wire.API.MakesFederatedCall
  ( CallsFed,
    MakesFederatedCall,
    Component (..),
    callsFed,
    unsafeCallsFed,
    AddAnnotation,
    Location (..),
    ShowComponent,
    Annotation,
    exposeAnnotations,
  )
where

import Data.Aeson
import Data.Constraint
import Data.Kind
import Data.Metrics.Servant
import Data.Proxy
import Data.Schema
import Data.Swagger.Operation (addExtensions)
import Data.Text qualified as T
import GHC.TypeLits
import Imports
import Servant.API
import Servant.Client
import Servant.Server
import Servant.Swagger
import Test.QuickCheck (Arbitrary)
import TransitiveAnns.Types
import Unsafe.Coerce (unsafeCoerce)
import Wire.API.Routes.Version
import Wire.Arbitrary (GenericUniform (..))

-- | This function exists only to provide a convenient place for the
-- @transitive-anns@ plugin to solve the 'ToHasAnnotations' constraint. This is
-- highly magical and warrants a note.
--
-- The call @'exposeAnnotations' (some expr here)@ will expand to @some expr
-- here@, additionally generating wanted 'HasAnnotation' constraints for every
-- 'AddAnnotation' constraint in the _transitive call closure_ of @some expr
-- here@.
--
-- The use case is always going to be @'callsFed' ('exposeAnnotations' expr)@,
-- where 'exposeAnnotations' re-introduces all of the constraints we've been
-- squirreling away, and 'callsFed' is responsible for discharging them. It
-- would be very desirable to combine these into one call, but the semantics of
-- solving 'ToHasAnnotations' attaches the wanted calls to the same place as
-- the call itself, which means the wanteds appear just after our opportunity
-- to solve them via 'callsFed'. This is likely not a hard limitation.
--
-- The @x@ parameter here is intentionally ambiguous, existing as a unique
-- skolem to prevent GHC from caching the results of solving
-- 'ToHasAnnotations'. Callers needn't worry about it.
exposeAnnotations :: ToHasAnnotations x => a -> a
exposeAnnotations = id

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

-- | A typeclass corresponding to calls to federated services. This class has
-- no methods, and exists only to automatically propagate information up to
-- servant.
--
-- The only way to discharge this constraint is via 'callsFed', which should be
-- invoked for each federated call when connecting handlers to the server
-- definition.
type CallsFed (comp :: Component) = HasAnnotation 'Remote (ShowComponent comp)

-- | A typeclass with the same layout as 'CallsFed', which exists only so we
-- can discharge 'CallsFeds' constraints by unsafely coercing this one.
class Nullary

instance Nullary

-- | Construct a dictionary for 'CallsFed'.
synthesizeCallsFed :: forall (comp :: Component) (name :: Symbol). Dict (CallsFed comp name)
synthesizeCallsFed = unsafeCoerce $ Dict @Nullary

-- | Servant combinator for tracking calls to federated calls. Annotating API
-- endpoints with 'MakesFederatedCall' is the only way to eliminate 'CallsFed'
-- constraints on handlers.
data MakesFederatedCall (comp :: Component) (name :: Symbol)

instance (HasServer api ctx) => HasServer (MakesFederatedCall comp name :> api :: Type) ctx where
  -- \| This should have type @CallsFed comp name => ServerT api m@, but GHC
  -- complains loudly thinking this is a polytype. We need to introduce the
  -- 'CallsFed' constraint so that we can eliminate it via
  -- 'synthesizeCallsFed', which otherwise is too-high rank for GHC to notice
  -- we've solved our constraint.
  type ServerT (MakesFederatedCall comp name :> api) m = Dict (CallsFed comp name) -> ServerT api m
  route _ ctx f = route (Proxy @api) ctx $ fmap ($ synthesizeCallsFed @comp @name) f
  hoistServerWithContext _ ctx f s = hoistServerWithContext (Proxy @api) ctx f . s

instance HasLink api => HasLink (MakesFederatedCall comp name :> api :: Type) where
  type MkLink (MakesFederatedCall comp name :> api) x = MkLink api x
  toLink f _ l = toLink f (Proxy @api) l

instance RoutesToPaths api => RoutesToPaths (MakesFederatedCall comp name :> api :: Type) where
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
instance (HasSwagger api, KnownSymbol name, KnownSymbol (ShowComponent comp)) => HasSwagger (MakesFederatedCall comp name :> api :: Type) where
  toSwagger _ =
    toSwagger (Proxy @api)
      & addExtensions
        mergeJSONArray
        [ ( "wire-makes-federated-call-to",
            Array
              [ Array
                  [ String $ T.pack $ symbolVal $ Proxy @(ShowComponent comp),
                    String $ T.pack $ symbolVal $ Proxy @name
                  ]
              ]
          )
        ]

mergeJSONArray :: Value -> Value -> Value
mergeJSONArray (Array x) (Array y) = Array $ x <> y
mergeJSONArray _ _ = error "impossible! bug in construction of federated calls JSON"

instance HasClient m api => HasClient m (MakesFederatedCall comp name :> api :: Type) where
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
  callsFed :: (c => r) -> a

instance (c ~ ((k, d) :: Constraint), SolveCallsFed d r a) => SolveCallsFed c r (Dict k -> a) where
  callsFed f Dict = callsFed @d @r @a f

instance {-# OVERLAPPABLE #-} (c ~ (() :: Constraint), r ~ a) => SolveCallsFed c r a where
  callsFed f = f

-- | Unsafely discharge a 'CallsFed' constraint. Necessary for interacting with
-- wai-routes.
--
-- This is unsafe in the sense that it will drop the 'CallsFed' constraint, and
-- thus might mean a federated call gets forgotten in the documentation.
unsafeCallsFed :: forall (comp :: Component) (name :: Symbol) r. (CallsFed comp name => r) -> r
unsafeCallsFed f = withDict (synthesizeCallsFed @comp @name) f
