{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module API.Team.Util where

import Bilge hiding (accept, timeout, head)
import Bilge.Assert
import Control.Monad
import Data.Aeson
import Data.ByteString.Conversion
import Data.Id hiding (client)
import Data.Range
import Data.Text (Text)
import Galley.Types (ConvTeamInfo (..), NewConv (..))
import Util

import qualified Data.Set                    as Set
import qualified Galley.Types.Teams          as Team

createTeam :: UserId -> Galley -> Http TeamId
createTeam u galley = do
    tid <- randomId
    r <- put ( galley
              . paths ["i", "teams", toByteString' tid]
              . contentJson
              . zAuthAccess u "conn"
              . expect2xx
              . lbytes (encode newTeam)
              )
    maybe (error "invalid team id") return $
        fromByteString $ getHeader' "Location" r

addTeamMember :: Galley -> TeamId -> Team.NewTeamMember -> Http ()
addTeamMember galley tid mem =
    void $ post ( galley
                . paths ["i", "teams", toByteString' tid, "members"]
                . contentJson
                . expect2xx
                . lbytes (encode mem)
                )

createTeamConv :: Galley -> TeamId -> UserId -> [UserId] -> Maybe Text -> Http ConvId
createTeamConv g tid u us name = do
    let tinfo = Just $ ConvTeamInfo tid False
    let conv = NewConv us name (Set.fromList []) tinfo
    r <- post ( g
              . path "/conversations"
              . zUser u
              . zConn "conn"
              . contentJson
              . lbytes (encode conv)
              ) <!! const 201 === statusCode
    maybe (error "invalid conv id") return $
        fromByteString $ getHeader' "Location" r

deleteTeamConv :: Galley -> TeamId -> ConvId -> UserId -> Http ()
deleteTeamConv g tid cid u = do
    delete ( g
           . paths ["teams", toByteString' tid, "conversations", toByteString' cid]
           . zUser u
           . zConn "conn"
           ) !!! const 200 === statusCode

deleteTeam :: Galley -> TeamId -> UserId -> Http ()
deleteTeam g tid u = do
    delete ( g
           . paths ["teams", toByteString' tid]
           . zUser u
           . zConn "conn"
           . lbytes (encode $ Team.newTeamDeleteData Util.defPassword)
           ) !!! const 202 === statusCode

newTeam :: Team.BindingNewTeam
newTeam = Team.BindingNewTeam $ Team.newNewTeam (unsafeRange "teamName") (unsafeRange "defaultIcon")
