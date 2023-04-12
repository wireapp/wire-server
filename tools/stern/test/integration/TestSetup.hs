{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fprint-potential-instances #-}

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

module TestSetup
  ( test,
    tsManager,
    tsLogger,
    tsStern,
    tsBrig,
    tsGalley,
    TestM (..),
    TestSetup (..),
    Stern,
    Galley,
    Brig,
  )
where

import Bilge (HttpT (..), Manager, MonadHttp, Request, runHttpT)
import Control.Lens (makeLenses, (^.))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Imports
import qualified System.Logger as Log
import Test.Tasty (TestName, TestTree)
import Test.Tasty.HUnit (Assertion, testCase)

newtype TestM a = TestM
  { runTestM :: ReaderT TestSetup (HttpT IO) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader TestSetup,
      MonadIO,
      MonadCatch,
      MonadThrow,
      MonadMask,
      MonadHttp,
      MonadUnliftIO,
      MonadFail
    )

type Stern = Request -> Request

type Brig = Request -> Request

type Galley = Request -> Request

data TestSetup = TestSetup
  { _tsManager :: Manager,
    _tsStern :: Stern,
    _tsBrig :: Brig,
    _tsGalley :: Galley,
    _tsLogger :: Log.Logger
  }

makeLenses ''TestSetup

test :: IO TestSetup -> TestName -> TestM a -> TestTree
test mkSetup testName testAction = testCase testName runTest
  where
    runTest :: Assertion
    runTest = do
      setup <- mkSetup
      void . runHttpT (setup ^. tsManager) . flip runReaderT setup . runTestM $ testAction
