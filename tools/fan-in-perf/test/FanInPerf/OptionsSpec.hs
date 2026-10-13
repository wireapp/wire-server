module FanInPerf.OptionsSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import FanInPerf.Options
import FanInPerf.Targets (TargetConfig (..), TargetKind (..))
import Hasql.Transaction.Sessions (IsolationLevel (..))
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
            targets = TargetConfig KindTeam 10 1 :| [],
            clientsPerUser = 1,
            payloadBytes = 512,
            duration = Nothing,
            warmup = 5,
            seed = Nothing
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
          "0",
          "--seed",
          "-42"
        ]
    (o.global.poolSize, o.global.metricsPort, o.global.isolation)
      `shouldBe` (Just 8, 9500, Serializable)
    o.command
      `shouldBe` Produce (ProduceOptions 4 (TargetConfig KindUser 100 5 :| []) 2 64 (Just 30) 0 (Just (-42)))

  it "parses migrate" $
    fmap (.command) (parse ["--db", "x", "migrate"]) `shouldBe` Just Migrate

  it "parses reset" $
    fmap (.command) (parse ["--db", "x", "reset"]) `shouldBe` Just Reset

  it "parses consume with defaults" $ do
    o <- parseOk ["--db", "postgresql://u:p@localhost/db", "consume", "--targets", "user:100,team:1"]
    case o.command of
      Consume c -> do
        (c.clients, c.epochsPerClient, c.pollIntervalMs, c.fetchLimit, c.readerPoolSize, c.drainTimeout, c.safetyWindowMs)
          `shouldBe` (2000, 3, 100, 100, 32, 10, 0)
        c.writerOptions.writers `shouldBe` 16
        c.writerOptions.targets `shouldBe` (TargetConfig KindUser 100 1 :| [TargetConfig KindTeam 1 1])
      _ -> expectationFailure "not a consume command"

  it "parses all consume flags" $ do
    o <-
      parseOk
        [ "--db",
          "postgresql://u:p@localhost/db",
          "consume",
          "--targets",
          "epoch:50",
          "--clients",
          "10",
          "--epochs-per-client",
          "2",
          "--poll-interval",
          "0",
          "--fetch-limit",
          "5",
          "--reader-pool-size",
          "4",
          "--drain-timeout",
          "0",
          "--safety-window",
          "60000",
          "--writers",
          "3",
          "--duration",
          "9"
        ]
    case o.command of
      Consume c -> do
        (c.clients, c.epochsPerClient, c.pollIntervalMs, c.fetchLimit, c.readerPoolSize, c.drainTimeout, c.safetyWindowMs)
          `shouldBe` (10, 2, 0, 5, 4, 0, 60000)
        (c.writerOptions.writers, c.writerOptions.duration) `shouldBe` (3, Just 9)
      _ -> expectationFailure "not a consume command"

  it "rejects consume flags out of bounds and the connections kind" $ do
    let consume extra = ["--db", "x", "consume", "--targets", "user:10"] <> extra
    isNothing (parse (consume ["--clients", "0"])) `shouldBe` True
    isNothing (parse (consume ["--fetch-limit", "0"])) `shouldBe` True
    isNothing (parse (consume ["--poll-interval", "-1"])) `shouldBe` True
    isNothing (parse (consume ["--reader-pool-size", "0"])) `shouldBe` True
    isNothing (parse (consume ["--safety-window", "-1"])) `shouldBe` True
    isNothing (parse (consume ["--safety-window", "60001"])) `shouldBe` True
    isNothing (parse ["--db", "x", "consume", "--targets", "connections:5"]) `shouldBe` True
    isNothing (parse ["--db", "x", "consume", "--targets", "user:10,connections:5"]) `shouldBe` True
    -- `produce` still accepts connections
    isNothing (parse ["--db", "x", "produce", "--targets", "connections:5"]) `shouldBe` False

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
      ["--db", "x", "produce", "--targets", "team:10", "--warmup", "9223372036854775807"],
      ["--db", "x", "produce", "--targets", "team:10", "--seed", "abc"],
      ["--db", "x", "produce", "--targets", "team:10", "--seed", "9223372036854775808"]
    ]
    $ \args ->
      it ("rejects " <> unwords args) $ isNothing (parse args) `shouldBe` True
