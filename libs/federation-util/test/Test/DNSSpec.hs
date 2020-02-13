module Test.DNSSpec where

import Imports
import Test.Hspec
import Network.DNS
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
            -- We only check that there is *some* distribution
            length x `shouldSatisfy` (> 0)
            length x `shouldSatisfy` (< 49)
            length y `shouldSatisfy` (> 0)
            length y `shouldSatisfy` (< 49)
    describe "srvLookup" $ do
        it "returns the expected result for wire.com" $ do
            rs <- makeResolvSeed defaultResolvConf
            wire <- srvLookup'' mockLookupSRV "_wire-server" "wire.com" rs
            wire `shouldBe` Just [("wire.com", 443)]
        it "filters out single '.' results" $ do
            rs <- makeResolvSeed defaultResolvConf
            exampleDotCom <- srvLookup'' mockLookupSRV "_wire-server" "example.com" rs
            exampleDotCom `shouldBe` Nothing
        it "can return multiple results" $ do
            rs <- makeResolvSeed defaultResolvConf
            zinfra <- srvLookup'' mockLookupSRV "_wire-server" "zinfra.io" rs
            (length <$> zinfra) `shouldBe` Just 2
        it "returns Nothing if there is no DNS record" $ do
            rs <- makeResolvSeed defaultResolvConf
            noRecord <- srvLookup'' mockLookupSRV "_wire-server" "no-record-here" rs
            noRecord `shouldBe` Nothing

fst4 :: (a,b,c,d) -> a
fst4 (a,_,_,_) = a

snd4 :: (a,b,c,d) -> b
snd4 (_,b,_,_) = b

-- mock function matching Network.DNS's 'lookupSRV' types
mockLookupSRV :: Resolver -> Domain -> IO (Either DNSError [(Word16, Word16, Word16, Domain)])
mockLookupSRV _ domain = do
    case domain of
         "_wire-server._tcp.wire.com." -> return $ Right [(0, 0, 443, "wire.com")]
         "_wire-server._tcp.zinfra.io." -> return $ Right [(0, 0, 443, "server1.zinfra.io"), (0, 0, 443, "server2.zinfra.io")]
         "_wire-server._tcp.example.com." -> return $ Right [(0,0,443, ".")]
         _ -> return $ Right []
