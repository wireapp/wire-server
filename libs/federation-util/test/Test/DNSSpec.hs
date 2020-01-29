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

        -- "orders records with the same priority according to weight with certain probability"
            -- actual <- orderSrvResult [
            --              (2, 10, 443, "server1.com")
            --             ,(2, 20, 443, "server2.com")
            --             ,(2, 0, 443, "dontuseoften.com")
            --             ]
            -- (snd4 <$> actual) has a higher chance of being [20,10,0] than
            -- other combinations, but can be in any combination
            -- does it make sense to test the randomness here?

fst4 :: (a,b,c,d) -> a
fst4 (a,_,_,_) = a

snd4 :: (a,b,c,d) -> b
snd4 (_,b,_,_) = b
