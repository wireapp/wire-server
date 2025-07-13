{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}

module Wire.APICompatSpec (spec) where

import Data.Domain (Domain (Domain))
import Data.Kind (Type)
import Data.Proxy
import Data.Qualified
import Imports
import Network.Wai qualified as Wai
import Polysemy
import Polysemy.Internal.Kind (Append)
import Servant
import Test.Hspec
import Wire.API.Routes.Named
import Wire.API.Routes.Public.Brig qualified as BrigRoutes
import Wire.MockInterpreters as Mock
import Wire.UserGroupSubsystem

-- FUTUREWORK: extend this to cover all of the APIs?
spec :: Spec
spec = describe "api compatibility (compile-type unit tests, yeay!)" do
  it "user groups" $ do
    let _doesThisTypecheck :: Wai.Application
        _doesThisTypecheck =
          Servant.serveWithContext
            (Proxy @BrigRoutes.UserGroupAPI)
            (Servant.defaultErrorFormatters :. Domain "localdomain" :. Servant.EmptyContext)
            ( Named (polyRun fakeRun (createGroup . tUnqualified))
                :<|> Named (polyRun fakeRun (getGroup . tUnqualified))
                :<|> Named (polyRun fakeRun (getGroups . tUnqualified))
                :<|> Named (polyRun fakeRun (updateGroup . tUnqualified))
                :<|> Named (polyRun fakeRun (deleteGroup . tUnqualified))
                :<|> Named (polyRun fakeRun (addUser . tUnqualified))
                :<|> Named (polyRun fakeRun (removeUser . tUnqualified))
            )

    -- no need to run anything here, it's supposed to pass or fail at compile time
    True `shouldBe` True

fakeRun :: forall m a. Sem ('[UserGroupSubsystem] `Append` UserGroupStoreInMemEffectStackTest) a -> m a
fakeRun = error "undefined, but that's fine, we don't need to run this."

-- | `PolyRun` may be absolute overkill, but it was fun to reinvent it!  :)
class PolyRun (m :: Type -> Type) f where
  type NT m f
  type InFun m f

  polyRun :: NT m f -> f -> InFun m f

instance PolyRun m (a -> Sem e z) where
  type NT m (a -> Sem e z) = Sem e z -> m z
  type InFun m (a -> Sem e z) = a -> m z

  polyRun nt infun a = nt (infun a)

instance PolyRun m (a -> b -> Sem eff z) where
  type NT m (a -> b -> Sem eff z) = Sem eff z -> m z
  type InFun m (a -> b -> Sem eff z) = a -> b -> m z

  polyRun nt infun a b = nt (infun a b)

instance PolyRun m (a -> b -> c -> Sem eff z) where
  type NT m (a -> b -> c -> Sem eff z) = Sem eff z -> m z
  type InFun m (a -> b -> c -> Sem eff z) = a -> b -> c -> m z

  polyRun nt infun a b c = nt (infun a b c)

instance PolyRun m (a -> b -> c -> d -> Sem eff z) where
  type NT m (a -> b -> c -> d -> Sem eff z) = Sem eff z -> m z
  type InFun m (a -> b -> c -> d -> Sem eff z) = a -> b -> c -> d -> m z

  polyRun nt infun a b c d = nt (infun a b c d)

instance PolyRun m (a -> b -> c -> d -> e -> Sem eff z) where
  type NT m (a -> b -> c -> d -> e -> Sem eff z) = Sem eff z -> m z
  type InFun m (a -> b -> c -> d -> e -> Sem eff z) = a -> b -> c -> d -> e -> m z

  polyRun nt infun a b c d e = nt (infun a b c d e)

instance PolyRun m (a -> b -> c -> d -> e -> f -> Sem eff z) where
  type NT m (a -> b -> c -> d -> e -> f -> Sem eff z) = Sem eff z -> m z
  type InFun m (a -> b -> c -> d -> e -> f -> Sem eff z) = a -> b -> c -> d -> e -> f -> m z

  polyRun nt infun a b c d e f = nt (infun a b c d e f)
