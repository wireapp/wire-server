{-# LANGUAGE NumDecimals                 #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Test.IntersperseSpec where

import qualified Data.Set as S
import Imports hiding (intersperse)
import Polysemy
import Polysemy.Testing
import Polysemy.Trace
import UnliftIO (async)
import Test.Hspec

-- | This test spins up an async thread that communicates with the main
-- polysemy monad via an 'MVar'. We then use 'intersperse' to inject polling
-- logic between each bind in order to read from the 'MVar'.
spec :: Spec
spec = do
  it "should poll from async-written channel" $ do
    result <- liftIO test
    let desired = S.fromList $ mconcat
          [ fmap ("loaded: " <>) ["hello", "world", "last"]
          , fmap (show @Int) [1..4]
          , ["finished"]
          ]
    result `shouldBe` desired


pull :: (Member (Embed IO) r, Member Trace r) => MVar String -> Sem r ()
pull chan = do
  embed (tryTakeMVar chan) >>= \case
    Nothing -> pure ()
    Just s -> do
      trace $ "loaded: " <> s
      pull chan


push :: MVar String -> IO ()
push chan = do
  putMVar chan "hello"
  putMVar chan "world"
  putMVar chan "last"



test :: IO (Set String)
test = fmap S.fromList $ do
  chan <- newEmptyMVar @_ @String
  _ <- async $ push chan
  fmap fst $ runM $
    runTraceList $
      intersperse (pull chan) $ do
        for_ [1 .. 4] $ \i -> do
          trace $ show @Int i
          liftIO $ threadDelay 1e5
        trace "finished"

