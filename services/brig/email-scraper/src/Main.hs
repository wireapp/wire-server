module Main where

import qualified Cassandra as C
import qualified Cassandra.Settings as C
import Brig.Types
import Control.Lens hiding ((.=))
import Data.Id
import Imports
import Options
import Options.Applicative
import qualified System.Logger.Extended as Log

main :: IO ()
main = do
  s <- execParser (info (helper <*> settingsParser) desc)
  l <- Log.mkLogger'
  casClient <- initCassandra l (s^.setCasBrig)
  C.runClient casClient $ do
    page1 <- scanForIndex 2500
    go 0 page1
  where
    desc = header   "list-emails-with-domain"
        <> progDesc "User script"
        <> fullDesc

    go n page = do
      let result = C.result page
          newCount = n + length result
      mapM_ printIfMatchesDomain result
      putStrLn $ "Scanned " ++ show newCount
      when (C.hasMore page) $ do
        nextPage <- C.liftClient (C.nextPage page)
        go newCount nextPage

interestingDomains :: [Maybe Text]
interestingDomains = map Just ["example.com"]

printIfMatchesDomain :: MonadIO m => UserRow -> m ()
printIfMatchesDomain (_     , Nothing    , _    , _         ) = pure ()
printIfMatchesDomain (userId, Just mEmail, mTeam, mActivated) = do
  let email = parseEmail mEmail
  if (emailDomain <$> email) `elem` interestingDomains
    then print (userId, pretty email, pretty mTeam, pretty mActivated)
    else pure ()
 where
  pretty :: Show a => Maybe a -> String
  pretty (Just x) = show x
  pretty Nothing  = ""

initCassandra ::
  MonadIO m =>
  Log.Logger ->
  CassandraSettings ->
  m C.ClientState
initCassandra l settings =
  C.init
    $ C.setLogger (C.mkLogger l)
      . C.setContacts (settings^.cHosts) []
      . C.setPortNumber (fromIntegral $ settings^.cPort)
      . C.setKeyspace (settings^.cKeyspace)
      . C.setProtocolVersion C.V4
    $ C.defSettings

type Activated = Bool

type UserRow =
  ( UserId,
    Maybe Text,
    Maybe TeamId,
    Maybe Activated
  )

scanForIndex :: Int32 -> C.Client (C.Page UserRow)
scanForIndex num =
  C.paginate cql (C.paramsP C.One () (num))
  where
    cql :: C.PrepQuery C.R () UserRow
    cql =
      "SELECT \
      \id, \
      \email, \
      \team, \
      \activated \
      \FROM user"
