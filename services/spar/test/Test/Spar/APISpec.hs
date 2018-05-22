{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Test.Spar.APISpec where

import Control.Lens
import Control.Monad.State
import Data.Id
import Spar.API
import Spar.App
import Test.Hspec

import qualified Data.Map as Map
import qualified Data.UUID as UUID
import qualified SAML2.WebSSO as SAML
import qualified Text.XML.Util as SAML


data TestState = TestState
  { _callmap  :: Map.Map String Int
  , _getuser  :: Maybe UserId
  , _cruser   :: UserId
  }
  deriving (Eq, Show)

makeLenses ''TestState


newtype TestSpar a = TestSpar { fromTestSpar :: State TestState a }
  deriving (Functor, Applicative, Monad, MonadState TestState)

instance MonadSpar TestSpar where
  getUser _ = called "getUser" >> gets (view getuser)
  createUser _ = called "createUser" >> gets (view cruser)
  forwardBrigLogin _ = called "forwardBrigLogin"

called :: String -> TestSpar SAML.Void
called funname = modify (callmap %~ Map.alter (maybe (Just 1) (Just . (+1))) funname) >> pure undefined

run :: TestSpar a -> TestState -> TestState
run = fmap snd . runState . fromTestSpar


spec :: Spec
spec = do
  describe "onSuccess" $ do
    let samluid  = SAML.UserId (SAML.Issuer $ SAML.unsafeParseURI "http://example.com/") (SAML.opaqueNameID "phoo")
        briguid  = Id UUID.nil
        tstempty = TestState mempty Nothing briguid
        tstfull  = TestState mempty (Just briguid) (error "should not have called createUser!")

    context "new user" $ do
      it "calls createUser" $ do
        (onSuccess samluid `run` tstempty) `shouldSatisfy`
          \(TestState cm _ _) -> Map.lookup "createUser" cm == Just 1

    context "known user" $ do
      it "does not call createUser" $ do
        (onSuccess samluid `run` tstfull) `shouldSatisfy`
          \(TestState cm _ _) -> Map.lookup "createUser" cm == Nothing

    it "calls forwardBrigLogin" . forM_ [tstempty, tstfull] $ \tst -> do
      (onSuccess samluid `run` tst) `shouldSatisfy`
        \(TestState cm _ _) -> Map.lookup "forwardBrigLogin" cm == Just 1
