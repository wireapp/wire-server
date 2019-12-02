module Galley.Data.Services
    ( -- * BotMember
      BotMember
    , fromBotMember
    , newBotMember
    , botMemId
    , botMemService
    , addBotMember

      -- * Service
    , insertService
    , lookupService
    , deleteService
    ) where

import Imports
import Cassandra
import Control.Lens
import Data.Id
import Data.Time.Clock
import Galley.App
import Galley.Data (newMember)
import Galley.Data.Queries
import Galley.Data.Instances ()
import Galley.Types.Bot
import Galley.Types.Conversations.Roles
import Galley.Types hiding (Conversation)

-- BotMember ------------------------------------------------------------------

newtype BotMember = BotMember { fromBotMember :: Member }

newBotMember :: Member -> Maybe BotMember
newBotMember m = const (BotMember m) <$> memService m

botMemId :: BotMember -> BotId
botMemId = BotId . memId . fromBotMember

botMemService :: BotMember -> ServiceRef
botMemService = fromJust . memService . fromBotMember

addBotMember :: UserId -> ServiceRef -> BotId -> ConvId -> UTCTime -> Galley (Event, BotMember)
addBotMember orig s bot cnv now = do
    let pid = s^.serviceRefProvider
    let sid = s^.serviceRefId
    retry x5 $ batch $ do
        setType BatchLogged
        setConsistency Quorum
        addPrepQuery insertUserConv (botUserId bot, cnv)
        addPrepQuery insertBot (cnv, bot, sid, pid, roleNameWireMember)
    let e = Event MemberJoin cnv orig now (Just . EdMembersJoin . SimpleMembers $ (fmap toSimpleMember [botUserId bot]))
    let mem = (newMember (botUserId bot)) { memService = Just s }
    return (e, BotMember mem)
  where
    toSimpleMember :: UserId -> SimpleMember
    toSimpleMember u = SimpleMember u (Just s) roleNameWireMember

-- Service --------------------------------------------------------------------

insertService :: MonadClient m => Service -> m ()
insertService s = do
    let sid = s^.serviceRef.serviceRefId
    let pid = s^.serviceRef.serviceRefProvider
    let tok = s^.serviceToken
    let url = s^.serviceUrl
    let fps = Set (s^.serviceFingerprints)
    let ena = s^.serviceEnabled
    retry x5 $ write insertSrv (params Quorum (pid, sid, url, tok, fps, ena))

lookupService :: MonadClient m => ServiceRef -> m (Maybe Service)
lookupService s = fmap toService <$>
    retry x1 (query1 selectSrv (params Quorum (s^.serviceRefProvider, s^.serviceRefId)))
  where
    toService (url, tok, Set fps, ena) =
        newService s url tok fps & set serviceEnabled ena

deleteService :: MonadClient m => ServiceRef -> m ()
deleteService s = retry x5 (write rmSrv (params Quorum (s^.serviceRefProvider, s^.serviceRefId)))
