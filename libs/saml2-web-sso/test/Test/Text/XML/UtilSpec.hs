{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-binds -Wno-incomplete-patterns -Wno-incomplete-uni-patterns -Wno-orphans #-}

module Test.Text.XML.UtilSpec
  ( spec,
  )
where

import Control.Monad ((>=>))
import SAML2.WebSSO.Test.Arbitrary ()
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSize)
import Test.QuickCheck
import Text.Show.Pretty
import Text.XML
import Text.XML.Util

spec :: Spec
spec = do
  describe "hxtToConduit, conduitToHxt" $ do
    modifyMaxSize (* 2 {- it took @*10@ here to find a bug once -}) $
      it "roundtrip" . property $ \(normalizeDoc -> doc) -> do
        let msg =
              ppShow
                ( '1',
                  doc,
                  '2',
                  renderLBS def doc,
                  '3',
                  conduitToHxt @(Either String) doc,
                  '4',
                  docToXML' <$> (conduitToHxt @(Either String) doc),
                  '5',
                  (conduitToHxt >=> hxtToConduit @(Either String)) doc
                )
        counterexample msg $
          (conduitToHxt >=> fmap normalizeDoc . hxtToConduit) doc === Right doc
