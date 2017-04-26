{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Json where

import Test.Tasty
import Test.Tasty.QuickCheck

import Control.Lens (set, view, (&))
import Data.Aeson
import Data.Id ()
import Gundeck.Types.Push

tests :: TestTree
tests = testGroup "JSON"
    [ testProperty "push/recipient" $
        forAll genRecipient serialiseOkProp
    ]

serialiseOkProp :: Recipient -> Property
serialiseOkProp r = property $
        let r' = decode (encode r)
        in   (view recipientId       <$> r') == Just (view recipientId r)
          && (view recipientRoute    <$> r') == Just (view recipientRoute r)
          && (view recipientClients  <$> r') == Just (view recipientClients r)
          && (view recipientFallback <$> r') == Just (view recipientFallback r)

-----------------------------------------------------------------------------
-- Randomness

genRecipient :: Gen Recipient
genRecipient = do
    r <- recipient <$> arbitrary <*> elements [ RouteAny, RouteDirect, RouteNative ]
    c <- arbitrary
    f <- arbitrary
    return $ r & set recipientFallback f & set recipientClients c

