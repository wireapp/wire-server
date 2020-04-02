module Main where

import qualified Cassandra as C
import qualified Cassandra.Settings as C
import qualified Cassandra.Util as C
import Data.Id
import Imports
import qualified System.Logger.Extended as Log

main :: IO ()
main = do
  [host, portString] <- getArgs
  let port :: Int32 = read portString
  l <- Log.mkLogger'
  casClient <- initCassandra l host port
  C.runClient casClient $ do
    page1 <- scanForIndex 500
    go 0 page1
  where
    go n page = do
      let result = C.result page
          newCount = n + length result
      mapM_ printIfErrorSome result
      putStrLn $ "Scanned " ++ show newCount
      when (C.hasMore page) $ do
        nextPage <- C.liftClient (C.nextPage page)
        go newCount nextPage

printIfErrorSome :: MonadIO m => ReindexRow -> m ()
printIfErrorSome row@(userId, mName, mNameWT, mColour, mColourWT, mActivated, mActivatedWT) =
  if or
    [ any isNothing [mNameWT, mColourWT, mActivatedWT],
      isNothing mName,
      isNothing mColour,
      isNothing mActivated
    ]
    then do
      putStrLn $ "Found unexpected null for: " <> show userId
      print row
    else pure ()

initCassandra ::
  (MonadIO m, Integral a) =>
  Log.Logger ->
  String ->
  a ->
  m C.ClientState
initCassandra l host port =
  C.init
    $ C.setLogger (C.mkLogger l)
      . C.setContacts host []
      . C.setPortNumber (fromIntegral port)
      . C.setKeyspace (C.Keyspace "brig")
      . C.setProtocolVersion C.V4
    $ C.defSettings

type Name = Text

type Activated = Bool

type ColourId = Int32

type ReindexRow =
  ( UserId,
    Maybe Name,
    Maybe (C.Writetime Name),
    Maybe ColourId,
    Maybe (C.Writetime ColourId),
    Maybe Activated,
    Maybe (C.Writetime Activated)
  )

scanForIndex :: Int32 -> C.Client (C.Page ReindexRow)
scanForIndex num =
  C.paginate cql (C.paramsP C.One () (num + 1))
  where
    cql :: C.PrepQuery C.R () ReindexRow
    cql =
      "SELECT \
      \id, \
      \name, \
      \writetime(name), \
      \accent_id, \
      \writetime(accent_id), \
      \activated, \
      \writetime(activated) \
      \FROM user"
