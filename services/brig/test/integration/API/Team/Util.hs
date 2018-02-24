{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module API.Team.Util where

import Bilge hiding (accept, timeout, head)
import Bilge.Assert
import Brig.Types.User
import Control.Lens (view)
import Control.Monad
import Data.Aeson
import Data.ByteString.Conversion
import Data.Id hiding (client)
import Data.Maybe (fromMaybe)
import Data.Range
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

createUserWithTeam :: Brig -> Galley -> Http (UserId, TeamId)
createUserWithTeam brig galley = do
    e <- randomEmail
    n <- randomName
    let p = RequestBodyLBS . encode $ object
            [ "name"            .= n
            , "email"           .= fromEmail e
            , "password"        .= defPassword
            , "team"            .= newTeam
            ]
    rsp <- post (brig . path "/i/users" . contentJson . body p)
    uid <- maybe (error "invalid user id") return $ fromByteString $ getHeader' "Location" rsp
    (team:_) <- view Team.teamListTeams <$> getTeams uid galley
    return (uid, view Team.teamId team)

addTeamMember :: Galley -> TeamId -> Team.NewTeamMember -> Http ()
addTeamMember galley tid mem =
    void $ post ( galley
                . paths ["i", "teams", toByteString' tid, "members"]
                . contentJson
                . expect2xx
                . lbytes (encode mem)
                )

createTeamConv :: Galley -> TeamId -> UserId -> [UserId] -> Bool -> Http ConvId
createTeamConv g tid u us managed = do
    let tinfo = Just $ ConvTeamInfo tid managed
    let conv = NewConv us Nothing (Set.fromList []) Nothing tinfo
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

getTeams :: UserId -> Galley -> Http Team.TeamList
getTeams u galley = do
    r <- get ( galley
             . paths ["teams"]
             . zAuthAccess u "conn"
             . expect2xx
             )
    return $ fromMaybe (error "getTeams: failed to parse response") (decodeBody r)

newTeam :: Team.BindingNewTeam
newTeam = Team.BindingNewTeam $ Team.newNewTeam (unsafeRange "teamName") (unsafeRange "defaultIcon")
