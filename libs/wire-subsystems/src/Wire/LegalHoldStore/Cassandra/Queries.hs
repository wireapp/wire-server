module Wire.LegalHoldStore.Cassandra.Queries where

import Cassandra as C
import Data.Functor.Identity (Identity)
import Data.Id
import Data.LegalHold
import Data.Misc
import Data.Text (Text)
import Text.RawString.QQ
import Wire.API.Provider.Service
import Wire.API.User.Client.Prekey

insertLegalHoldSettings :: PrepQuery W (HttpsUrl, Fingerprint Rsa, ServiceToken, ServiceKey, TeamId) ()
insertLegalHoldSettings =
  [r|
    update legalhold_service
    set base_url    = ?,
        fingerprint = ?,
        auth_token  = ?,
        pubkey      = ?
    where team_id = ?
  |]

selectLegalHoldSettings :: PrepQuery R (Identity TeamId) (HttpsUrl, Fingerprint Rsa, ServiceToken, ServiceKey)
selectLegalHoldSettings =
  [r|
   select base_url, fingerprint, auth_token, pubkey
     from legalhold_service
     where team_id = ?
   |]

removeLegalHoldSettings :: PrepQuery W (Identity TeamId) ()
removeLegalHoldSettings = "delete from legalhold_service where team_id = ?"

insertPendingPrekeys :: PrepQuery W (UserId, PrekeyId, Text) ()
insertPendingPrekeys =
  [r|
        insert into legalhold_pending_prekeys (user, key, data) values (?, ?, ?)
    |]

dropPendingPrekeys :: PrepQuery W (Identity UserId) ()
dropPendingPrekeys =
  [r|
        delete from legalhold_pending_prekeys
          where user = ?
    |]

selectPendingPrekeys :: PrepQuery R (Identity UserId) (PrekeyId, Text)
selectPendingPrekeys =
  [r|
        select key, data
          from legalhold_pending_prekeys
          where user = ?
          order by key asc
    |]

updateUserLegalHoldStatus :: PrepQuery W (UserLegalHoldStatus, TeamId, UserId) ()
updateUserLegalHoldStatus =
  [r|
        update team_member
          set legalhold_status = ?
          where team = ? and user = ?
    |]

insertLegalHoldWhitelistedTeam :: PrepQuery W (Identity TeamId) ()
insertLegalHoldWhitelistedTeam =
  [r|
        insert into legalhold_whitelisted (team) values (?)
    |]

removeLegalHoldWhitelistedTeam :: PrepQuery W (Identity TeamId) ()
removeLegalHoldWhitelistedTeam =
  [r|
        delete from legalhold_whitelisted where team = ?
    |]
