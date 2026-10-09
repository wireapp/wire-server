module FanInPerf.OptionsSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import FanInPerf.Options
import FanInPerf.Targets (TargetEntry (..), TargetKind (..))
import Imports
import Options.Applicative
import Test.Hspec

parse :: [String] -> Maybe Options
parse = getParseResult . execParserPure defaultPrefs optionsInfo

parseOk :: [String] -> IO Options
parseOk args = maybe (expectationFailure "parse failed" >> undefined) pure (parse args)

spec :: Spec
spec = do
  it "parses produce with defaults" $ do
    o <- parseOk ["--db", "postgresql://u:p@localhost/db", "produce", "--targets", "team:10"]
    o.global.db `shouldBe` "postgresql://u:p@localhost/db"
    o.global.poolSize `shouldBe` Nothing
    o.global.metricsPort `shouldBe` 9400
    o.global.isolation `shouldBe` ReadCommitted
    o.command
      `shouldBe` Produce
        ProduceOptions
          { writers = 16,
            targets = TargetEntry KindTeam 10 1 :| [],
            clientsPerUser = 1,
            payloadBytes = 512,
            duration = Nothing,
            warmup = 5
          }

  it "parses all produce flags" $ do
    o <-
      parseOk
        [ "--db",
          "x",
          "--pool-size",
          "8",
          "--metrics-port",
          "9500",
          "--isolation",
          "serializable",
          "produce",
          "--writers",
          "4",
          "--targets",
          "user:100x5",
          "--clients-per-user",
          "2",
          "--payload-bytes",
          "64",
          "--duration",
          "30",
          "--warmup",
          "0"
        ]
    (o.global.poolSize, o.global.metricsPort, o.global.isolation)
      `shouldBe` (Just 8, 9500, Serializable)
    o.command
      `shouldBe` Produce (ProduceOptions 4 (TargetEntry KindUser 100 5 :| []) 2 64 (Just 30) 0)

  it "parses reset" $
    fmap (.command) (parse ["--db", "x", "reset"]) `shouldBe` Just Reset

  forM_
    [ ["produce", "--targets", "team:10"],
      ["--db", "x", "produce"],
      ["--db", "x", "produce", "--targets", "team:0"],
      ["--db", "x", "produce", "--targets", "team:10", "--writers", "0"],
      ["--db", "x", "--isolation", "dirty", "reset"],
      ["--db", "x", "--metrics-port", "70000", "reset"],
      ["--db", "x"],
      ["--db", "x", "produce", "--targets", "team:10", "--clients-per-user", "0"],
      ["--db", "x", "produce", "--targets", "team:10", "--clients-per-user", "-1"],
      ["--db", "x", "produce", "--targets", "team:10", "--writers", "-1"],
      ["--db", "x", "produce", "--targets", "team:10", "--payload-bytes", "0"],
      ["--db", "x", "produce", "--targets", "team:10", "--duration", "0"],
      ["--db", "x", "produce", "--targets", "team:10", "--warmup", "-1"],
      ["--db", "x", "--pool-size", "0", "reset"],
      ["--db", "x", "--pool-size", "10001", "reset"],
      ["--db", "x", "--metrics-port", "18446744073709551617", "reset"],
      ["--db", "x", "produce", "--targets", "team:10", "--writers", "18446744073709551617"],
      ["--db", "x", "produce", "--targets", "team:10", "--writers", "10001"],
      ["--db", "x", "produce", "--targets", "team:10", "--payload-bytes", "10000001"],
      ["--db", "x", "produce", "--targets", "team:10", "--clients-per-user", "100001"],
      ["--db", "x", "produce", "--targets", "team:10", "--duration", "9223372036854775807"],
      ["--db", "x", "produce", "--targets", "team:10", "--warmup", "9223372036854775807"]
    ]
    $ \args ->
      it ("rejects " <> unwords args) $ isNothing (parse args) `shouldBe` True
