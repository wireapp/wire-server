{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE QuantifiedConstraints #-}

{-# OPTIONS_GHC -Wno-orphans             #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Test.Spar.Sem.NowSpec where

import Arbitrary ()
import Imports
import Polysemy
import Polysemy.Check
import SAML2.WebSSO.Types
import qualified Spar.Sem.Now as E
import Spar.Sem.Now.IO
import Spar.Sem.Now.Input
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Polysemy.Input
import Data.Time
import Data.Time.Calendar.Julian

deriveGenericK ''E.Now

propsForInterpreter ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Spec
propsForInterpreter lower = do
  describe "Now Actions" $ do
    prop "now/now" $ prop_nowNow lower

someTime :: Time
someTime = Time (UTCTime (fromJulianYearAndDay 1990 209) (secondsToDiffTime 0))

spec :: Spec
spec = do
  modifyMaxSuccess (const 1000) $ do
    propsForInterpreter $ fmap Identity . runM . nowToIO . runInputConst ()
    propsForInterpreter $ pure . Identity . run . runInputConst someTime . nowToInput . runInputConst ()

-- | All the constraints we need to generalize properties in this module.
-- A regular type synonym doesn't work due to dreaded impredicative
-- polymorphism.
class (Member E.Now r, Member (Input ()) r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z))
   => PropConstraints r f
instance (Member E.Now r, Member (Input ()) r, forall z. Show z => Show (f z), forall z. Eq z => Eq (f z))
   => PropConstraints r f


prop_nowNow ::
  PropConstraints r f =>
  (forall a. Sem r a -> IO (f a)) ->
  Property
prop_nowNow =
  -- NOTE: This @Input ()@ effect is a workaround to an oversight in
  -- @polysemy-check@. 'prepropLaw' wants to synthesize some actions to run
  -- before and after its generators, and check their results for equality. We
  -- can't use 'Now' as this effect, because 'E.get' won't return equivalent
  -- results! And we can't keep it empty, because that triggers a crash in
  -- @polysemy-check@. Thus @Input ()@, which isn't beautiful, but works fine.
  prepropLaw @'[Input ()] $ do
        pure
          ( liftA2 (<=) E.get E.get,
            pure True
          )

