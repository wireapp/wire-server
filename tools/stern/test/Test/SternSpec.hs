{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Test.SternSpec (spec) where

import Imports

import Control.Exception (ErrorCall(ErrorCall))
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Unlift
import Data.String.Conversions (cs)
import Network.HTTP.Types.Status
import Network.HTTP.Types.Status (statusCode)
import Network.Wai
import Network.Wai.Utilities.Error
import Servant.API
import Servant.API.Generic
import Servant.Server.Generic
import Stern.App
import Stern.Intra
import Stern.Types
import System.IO.Silently (capture)
import Test.Hspec

import qualified Stern.Servant.Handler as ServantStern
import qualified Network.Wai.Test as HW
import qualified System.Logger as Log
import qualified Test.Hspec.Wai as HW
import qualified Test.Hspec.Wai.Internal as HW


testEnv :: IO Env
testEnv = do
  let _brig        = undefined
      _galley      = undefined
      _gundeck     = undefined
      _ibis        = undefined
      _galeb       = undefined
      _metrics     = undefined
      _requestId   = undefined
      _httpManager = undefined

  _applog <- Log.new (Log.defSettings & Log.setLogLevel Log.Warn)
  pure Env {..}


runLegacyThrowingApp :: Env -> Application
runLegacyThrowingApp env req cont = runHandler env req handler cont
  where
    handler = case pathInfo req of
      ["fail400"] -> throwRpcError $ Error status400 "bad request" "well, what can i say?  it'd just bad."
      ["fail502"] -> throwM $ ErrorCall "this is an internal error in stern, but it should still be handled."
      bad         -> error $ show bad


data ServantThrowingApi routes = ServantThrowingApi
  { fail400 :: routes :- "fail400" :> Get '[JSON] ()
  , fail502 :: routes :- "fail502" :> Get '[JSON] ()
  }
  deriving (Generic)

servantThrowingApi :: ServantThrowingApi (AsServerT App)
servantThrowingApi = ServantThrowingApi
  { fail400 = throwRpcError (Error status400 "bad request" "well, what can i say?  it'd just bad.")
  , fail502 = throwM (ErrorCall "this is an internal error in stern, but it should still be handled.")
  }

runServantThrowingApp :: Env -> Application
runServantThrowingApp env = ServantStern.mkAppGeneric env servantThrowingApi


spec :: Spec
spec = beforeAll testEnv $ do
  describe "MonadIntra" $ do
    describe "legacy instance in Stern.Intra" $ do
      it "handles throwRpcError" $ \env -> HW.withApplication (runLegacyThrowingApp env) $ do
        HW.get "/fail400" `HW.shouldRespondWith` 400

      it "crashes on throwM" $ \env -> HW.withApplication (runLegacyThrowingApp env) $ do
        liftIO $ pendingWith "I think this just needs some fiddling with the unliftio package to get to work."
        -- HW.get "/fail502" `shouldThrow` \(ErrorCall _) -> True
        -- (5xx errors in legacy code are handled by a middleware, not inside the
        -- application, so this should be fine.)

    describe "servant instance in Stern.Servant.Handler" $ do
      it "handles throwRpcError" $ \env -> HW.withApplication (runServantThrowingApp env) $ do
        HW.get "/fail400" `HW.shouldRespondWith` 400

      it "handles throwM" $ \env -> HW.withApplication (runServantThrowingApp env) $ do
        HW.get "/fail502" `HW.shouldRespondWith` 502
