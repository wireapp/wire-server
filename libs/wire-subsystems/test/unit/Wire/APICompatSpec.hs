{-# OPTIONS_GHC -Wno-ambiguous-fields -Wno-incomplete-uni-patterns -Wno-incomplete-patterns #-}

module Wire.APICompatSpec (spec) where

import Data.Domain (Domain (Domain))
import Data.Proxy
import Data.Qualified
import Imports
import Network.Wai qualified as Wai
import Polysemy
import Polysemy.Error
import Polysemy.Input (Input)
import Polysemy.Internal.Kind (Append)
import Polysemy.State
import Servant as Servant
import Test.Hspec
import Wire.API.Routes.Named
import Wire.API.Routes.Public.Brig qualified as BrigRoutes
import Wire.GalleyAPIAccess
import Wire.MockInterpreters as Mock
import Wire.NotificationSubsystem
import Wire.UserGroupSubsystem
import Wire.UserGroupSubsystem.Interpreter
import Wire.UserSubsystem (UserSubsystem)

type TestEffectStack =
  '[ UserSubsystem,
     GalleyAPIAccess
   ]
    `Append` EffectStack
    `Append` '[ Input (Local ()),
                NotificationSubsystem,
                State [Push],
                Error UserGroupSubsystemError
              ]

-- FUTUREWORK: extend this to cover all of the APIs?
spec :: Spec
spec = describe "api compatibility (compile-type unit tests, yeay!)" do
  it "user groups" $ do
    let _doesThisTypecheck :: Wai.Application
        _doesThisTypecheck =
          Servant.serveWithContext
            (Proxy @BrigRoutes.UserGroupAPI)
            (Servant.defaultErrorFormatters :. Domain "localdomain" :. Servant.EmptyContext)
            ( Named (\lusr newUserGroup -> fakeRun $ createGroup (tUnqualified lusr) newUserGroup)
                :<|> Named (\lusr ugid -> fakeRun $ getGroup (tUnqualified lusr) ugid)
                :<|> Named (\lusr q sortByKeys sortOrder pSize pState -> fakeRun $ getGroups (tUnqualified lusr) q sortByKeys sortOrder pSize pState)
                :<|> Named (\lusr gid gupd -> fakeRun $ updateGroup (tUnqualified lusr) gid gupd)
                :<|> Named (\lusr gid -> fakeRun $ deleteGroup (tUnqualified lusr) gid)
                :<|> Named (\lusr gid mid -> fakeRun $ addUser (tUnqualified lusr) gid mid)
                :<|> Named (\lusr gid mid -> fakeRun $ removeUser (tUnqualified lusr) gid mid)
            )
    -- no need to run anything here, it's supposed to pass or fail at compile time
    True `shouldBe` True

-- | (We could make this a type class with instances for all function arities, which would make
-- the call side look nicer.)
fakeRun :: forall m a. Sem ('[UserGroupSubsystem] `Append` TestEffectStack) a -> m a
fakeRun = error "undefined, but that's fine, we don't need to run this."
