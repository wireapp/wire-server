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

module Wire.API.MakesFederatedCall
  ( CallsFed,
    MakesFederatedCall,
    Component (..),
    callsFed,
  )
where

import Data.Constraint
import Data.Proxy
import Data.Swagger (Tag (..), applyTags)
import qualified Data.Text as T
import GHC.TypeLits
import Imports
import Servant.API
import Servant.Client
import Servant.Server
import Servant.Swagger
import Test.QuickCheck (Arbitrary)
import Unsafe.Coerce (unsafeCoerce)
import Wire.Arbitrary (GenericUniform (..))

data Component
  = Brig
  | Galley
  | Cargohold
  deriving (Show, Eq, Generic)
  deriving (Arbitrary) via (GenericUniform Component)

class CallsFed (comp :: Component) (name :: Symbol)

class Nullary

instance Nullary

synthesizeCallsFed :: forall (comp :: Component) (name :: Symbol). Dict (CallsFed comp name)
synthesizeCallsFed = unsafeCoerce $ Dict @Nullary

data MakesFederatedCall (comp :: Component) (name :: Symbol)

instance (HasServer api ctx) => HasServer (MakesFederatedCall comp name :> api :: *) ctx where
  type ServerT (MakesFederatedCall comp name :> api) m = Dict (CallsFed comp name) -> ServerT api m
  route _ ctx f = route (Proxy @api) ctx $ fmap ($ synthesizeCallsFed @comp @name) f
  hoistServerWithContext _ ctx f s = hoistServerWithContext (Proxy @api) ctx f . s

instance HasLink api => HasLink (MakesFederatedCall comp name :> api :: *) where
  type MkLink (MakesFederatedCall comp name :> api) x = MkLink api x
  toLink f _ l = toLink f (Proxy @api) l

type family ShowComponent (x :: Component) :: Symbol where
  ShowComponent 'Brig = "brig"
  ShowComponent 'Galley = "galley"
  ShowComponent 'Cargohold = "cargohold"

instance (HasSwagger api, KnownSymbol name, KnownSymbol (ShowComponent comp)) => HasSwagger (MakesFederatedCall comp name :> api :: *) where
  toSwagger _ =
    toSwagger (Proxy @api)
      & applyTags
        [ Tag
            "x-wire-makes-federated-call-to"
            ( Just $
                mconcat
                  [ T.pack $ symbolVal $ Proxy @(ShowComponent comp),
                    "/",
                    T.pack $ symbolVal $ Proxy @name
                  ]
            )
            Nothing
        ]

instance HasClient m api => HasClient m (MakesFederatedCall comp name :> api :: *) where
  type Client m (MakesFederatedCall comp name :> api) = Client m api
  clientWithRoute p _ = clientWithRoute p $ Proxy @api
  hoistClientMonad p _ f c = hoistClientMonad p (Proxy @api) f c

-- type Test = MakesFederatedCall 'Brig "hello" :> MakesFederatedCall 'Brig "soup" :> Get '[JSON] ()
--       :<|> Get '[JSON] ()

callsFed :: (c => r) -> Dict c -> r
callsFed f Dict = f

-- test :: Server Test
-- test = callsFed (callsFed yo) :<|> pure ()

-- yo :: CallsFed 'Brig "hello" => Handler ()
-- yo = pure ()

-- runx :: Application
-- runx = serve (Proxy @Test) (test)
