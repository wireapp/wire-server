{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Test.Spar.Sem.NowSpec where

import Arbitrary ()
import Data.Time
import Data.Time.Calendar.Julian
import Imports
import Polysemy
import Polysemy.Input
import SAML2.WebSSO.Types
import Spar.Sem.Now.IO
import Spar.Sem.Now.Input
import Spar.Sem.Now.Spec
import Test.Hspec
import Test.Hspec.QuickCheck

someTime :: Time
someTime = Time (UTCTime (fromJulianYearAndDay 1990 209) (secondsToDiffTime 0))

spec :: Spec
spec = do
  modifyMaxSuccess (const 1000) $ do
    propsForInterpreter "nowToIO" $ fmap Identity . runM . nowToIO . runInputConst ()
    propsForInterpreter "nowToInput" $ pure . Identity . run . runInputConst someTime . nowToInput . runInputConst ()
