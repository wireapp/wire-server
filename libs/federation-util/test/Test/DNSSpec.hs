module Test.DNSSpec where

import Imports
import Test.Hspec
import Network.Federation.Util.Internal

spec :: Spec
spec = do
    describe "order" $ do
        it "orders records according to ascending priority" $ do
            actual <- orderSrvResult [
                    -- priority, weight, port, domain
                         (0, 0, 443, "offline.com")
                        ,(15, 10, 443, "main.com")
                        ,(0, 5, 443, "backup.com")
                        ,(2, 10, 443, "main.com")
                        ,(2, 20, 443, "main.com")
                        ,(3, 5, 443, "main.com")
                        ,(0, 0, 443, "backup.com")
                        ]
            (fst4 <$> actual) `shouldBe` [0,0,0,2,2,3,15]

        it "orders records with the same priority according to weight with certain probability" $ do
            let raw = [  (2, 10, 443, "server1.com")
                        ,(2, 20, 443, "server2.com")
                        ,(2, 0, 443, "dontuseoften.com")]
            -- order the list 50 times
            actuals <- replicateM 50 (orderSrvResult raw)
            let weightLists = fmap (fmap snd4) actuals
            let x = filter ((==20) . head) weightLists
            let y = filter ((==10) . head) weightLists
            -- we may have e.g.
            -- the server with weight 20 first in the list 30 times,
            -- the server with weight 10 15 times
            -- and the server with weight 0 5 times.
            -- We only check that there is *some* distribution that is not 50 times one order
            length x `shouldSatisfy` (> 0)
            length x `shouldSatisfy` (< 49)
            length y `shouldSatisfy` (> 0)
            length y `shouldSatisfy` (< 49)

fst4 :: (a,b,c,d) -> a
fst4 (a,_,_,_) = a

snd4 :: (a,b,c,d) -> b
snd4 (_,b,_,_) = b
