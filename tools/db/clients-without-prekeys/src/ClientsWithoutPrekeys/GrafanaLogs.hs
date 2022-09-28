module ClientsWithoutPrekeys.GrafanaLogs where

import ClientsWithoutPrekeys.Data
import qualified Data.Aeson as JSON
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Csv as CSV
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as V
import Imports hiding (ByteString)

type CSVInput = (ByteString, Word64, ByteString, ByteString, ByteString)

type Mixed = (FilePath, Either String (Vector (Either CSVInput UserClient)))

type Good = (FilePath, Vector UserClient)

type Bad = (FilePath, Either String (Vector CSVInput))

report :: [Bad] -> IO ()
report bads = forM_ bads $ \(fn, e) ->
  either
    (print . (<>) ("skipped file (" <> fn <> "): "))
    (V.mapM_ (print . (<>) ("skipped line (" <> fn <> "): ") . show))
    e

mergeUnique :: [Good] -> [UserClient]
mergeUnique = Set.toList . Set.unions . map (Set.fromList . V.toList . snd)

countNew :: (String, Map UserClient String, Int) -> Good -> (String, Map UserClient String, Int)
countNew (_, m, _) (fn, ucs) = (fn, n, count)
  where
    n = m <> V.foldr (`Map.insert` fn) Map.empty ucs
    count = Map.size $ Map.filter (fn ==) n

readCSV :: FilePath -> IO Mixed
readCSV fn = do
  raw <- LBS.readFile fn
  pure (fn, V.map parseUserClient <$> parseTrimmed (LBS.filter (/= '\r') raw) (Left "start"))

splitBadGood :: [Mixed] -> ([Bad], [Good])
splitBadGood = foldr sp ([], [])
  where
    sp (fn, e) (bads, goods) =
      either
        (\x -> ((fn, Left x) : bads, goods))
        ( bimap
            ((: bads) . (fn,) . Right . V.fromList)
            ((: goods) . (fn,) . V.fromList)
            . partitionEithers
            . V.toList
        )
        e

parseTrimmed ::
  ByteString ->
  Either String (Vector CSVInput) ->
  Either String (Vector CSVInput)
parseTrimmed raw = either cont Right
  where
    cont e | LBS.length raw < 2 = Left e
    cont _ = parseTrimmed (LBS.tail raw) (CSV.decode CSV.HasHeader raw)

parseUserClient :: CSVInput -> Either CSVInput UserClient
parseUserClient input@(_, _, lineBS, _, _) = maybe (Left input) Right $ JSON.decode lineBS
