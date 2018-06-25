{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module API.Team.Util where

import Bilge hiding (accept, timeout, head)
import Bilge.Assert
import Brig.Types.User
import Control.Lens (view)
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.ByteString.Conversion
import Data.Id hiding (client)
import Data.Maybe (fromMaybe)
import Data.Misc (Milliseconds)
import Data.Range
import Galley.Types (ConvTeamInfo (..), NewConv (..))
import GHC.Stack (HasCallStack)
import Test.Tasty.HUnit
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
    bdy <- decodeBody <$> post (brig . path "/i/users" . contentJson . body p)
    let (Just uid, Just (Just tid)) = (userId <$> bdy, userTeam <$> bdy)
    (team:_) <- view Team.teamListTeams <$> getTeams uid galley
    liftIO $ assertBool "Team ID in registration and team table do not match" (tid == view Team.teamId team)
    selfTeam <- userTeam . selfUser <$> getSelfProfile brig uid
    liftIO $ assertBool "Team ID in self profile and team table do not match" (selfTeam == Just tid)
    return (uid, tid)

addTeamMember :: Galley -> TeamId -> Team.NewTeamMember -> Http ()
addTeamMember galley tid mem =
    void $ post ( galley
                . paths ["i", "teams", toByteString' tid, "members"]
                . contentJson
                . expect2xx
                . lbytes (encode mem)
                )

createTeamConv :: HasCallStack => Galley -> TeamId -> UserId -> [UserId] -> Maybe Milliseconds -> Http ConvId
createTeamConv g tid u us mtimer = do
    let tinfo = Just $ ConvTeamInfo tid False
    let conv = NewConv us Nothing (Set.fromList []) Nothing tinfo mtimer
    r <- post ( g
              . path "/conversations"
              . zUser u
              . zConn "conn"
              . contentJson
              . lbytes (encode conv)
              ) <!! const 201 === statusCode
    maybe (error "invalid conv id") return $
        fromByteString $ getHeader' "Location" r

createManagedConv :: HasCallStack => Galley -> TeamId -> UserId -> [UserId] -> Maybe Milliseconds -> Http ConvId
createManagedConv g tid u us mtimer = do
    let tinfo = Just $ ConvTeamInfo tid True
    let conv = NewConv us Nothing (Set.fromList []) Nothing tinfo mtimer
    r <- post ( g
              . path "/i/conversations/managed"
              . zUser u
              . zConn "conn"
              . contentJson
              . lbytes (encode conv)
              ) <!! const 201 === statusCode
    maybe (error "invalid conv id") return $
        fromByteString $ getHeader' "Location" r

deleteTeamConv :: HasCallStack => Galley -> TeamId -> ConvId -> UserId -> Http ()
deleteTeamConv g tid cid u = do
    delete ( g
           . paths ["teams", toByteString' tid, "conversations", toByteString' cid]
           . zUser u
           . zConn "conn"
           ) !!! const 200 === statusCode

deleteTeam :: HasCallStack => Galley -> TeamId -> UserId -> Http ()
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
