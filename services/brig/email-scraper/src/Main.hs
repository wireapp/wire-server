module Main where

import qualified Cassandra as C
import qualified Cassandra.Settings as C
import Brig.Types
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Id
import Data.Text.Ascii
import qualified Data.Text as Text
import Imports
import Options
import Options.Applicative
import qualified Data.Aeson as JSON
import qualified System.Logger.Extended as Log

main :: IO ()
main = do
  s <- execParser (info (helper <*> settingsParser) desc)
  l <- Log.mkLogger'
  casClient <- initCassandra l (s^.setCasBrig)
  C.runClient casClient $ do
    page1 <- scanForIndex 2500
    goCheckProperties l 0 0 page1
  where
    desc = header   "list-emails-with-domain"
        <> progDesc "User script"
        <> fullDesc

    goCheckProperties l total n page = do
      let result = C.result page
          newCount = n + length result
      mapM_ (countTeamMemberIfHasProperties l) result
      Log.info l $ (Log.msg $ Log.val $ toByteString' ("Scanned " ++ show newCount))
      when (C.hasMore page) $ do
        nextPage <- C.liftClient (C.nextPage page)
        goCheckProperties l total newCount nextPage

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

-- Copied due to lack of Cql instance
type PropertyKeyCql = AsciiPrintable
type PropertyValueCql = Value

instance C.Cql PropertyValueCql where
  ctype = C.Tagged C.BlobColumn
  toCql = C.toCql . C.Blob . JSON.encode
  fromCql (C.CqlBlob v) = case JSON.eitherDecode v of
    Left e -> fail ("Failed to read property value: " <> e)
    Right x -> pure x
  fromCql _ = fail "PropertyValue: Blob expected"

propertySelect :: C.PrepQuery C.R (UserId, PropertyKeyCql) (Identity PropertyValueCql)
propertySelect = "SELECT value FROM properties where user = ? and key = ?"

lookupProperty :: C.MonadClient m => UserId -> PropertyKeyCql -> m (Maybe PropertyValueCql)
lookupProperty u k =
  fmap runIdentity
    <$> (C.query1 propertySelect (C.params C.Quorum (u, k)))

countTeamMemberIfHasProperties :: C.MonadClient m => Log.Logger -> UserRow -> m ()
countTeamMemberIfHasProperties _ (_     , _    , Nothing   , _         ) = pure ()
countTeamMemberIfHasProperties l (userId, _    , Just mTeam, mActivated) = do
  mProperty <- lookupProperty userId "webapp"
  let hasConsent = case mProperty of
                      Just property -> hasUserGivenConsent property
                      _ -> False
  when hasConsent $
    Log.info l (Log.msg $ Log.val $ toByteString' $ show (userId, show mTeam))
 where
  -- {\"settings\":{\"privacy\":{\"improve_wire\":true}}}
  hasUserGivenConsent :: Value -> Bool
  hasUserGivenConsent x = x ^? key "settings" . key "privacy" . key "improve_wire" . _Bool == Just True
