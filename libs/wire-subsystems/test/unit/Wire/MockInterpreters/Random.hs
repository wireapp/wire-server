module Wire.MockInterpreters.Random where

import Crypto.Random
import Data.ByteString.Short (fromShort)
import Data.Id
import Imports
import Polysemy
import Polysemy.State
import System.Random hiding (Random)
import Wire.Sem.Random

randomToStatefulStdGen :: (Member (State StdGen) r) => InterpreterFor Random r
randomToStatefulStdGen = interpret $ \case
  Bytes n -> do
    fromShort <$> withStatefulGen (genShortByteString n)
  Uuid -> withStatefulGen random
  ScimTokenId -> Id <$> withStatefulGen random
  LiftRandom m -> do
    seedInt <- withStatefulGen (random @Int)
    let seed = seedFromInteger $ toInteger seedInt
        drg = drgNewSeed seed
        (x, _) = withDRG drg m
    pure x
  NDigitNumber n -> withStatefulGen $ randomR (0, 10 ^ n - 1)

runRandomPure :: InterpreterFor Random r
runRandomPure = evalState defaulGen . randomToStatefulStdGen . raiseUnder

defaulGen :: StdGen
defaulGen = mkStdGen 0xBAD

withStatefulGen :: (Member (State StdGen) r) => (StdGen -> (a, StdGen)) -> Sem r a
withStatefulGen f = do
  g <- get
  let (x, g') = f g
  put g'
  pure x
