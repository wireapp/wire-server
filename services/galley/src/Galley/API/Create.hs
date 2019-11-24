module Galley.API.Create
    ( createGroupConversation
    , internalCreateManagedConversation
    , createSelfConversation
    , createOne2OneConversation
    , createConnectConversation
    ) where

import Imports hiding ((\\))
import Control.Lens hiding ((??))
import Control.Monad.Catch
import Data.Id
import Data.List1 (list1)
import Data.Range
import Data.Set ((\\))
import Data.Time
import Galley.App
import Galley.API.Error
import Galley.API.Mapping
import Galley.API.Util
import Galley.Intra.Push
import Galley.Types
import Galley.Types.Conversations.Roles
import Galley.Types.Teams hiding (EventType (..))
import Galley.Validation
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Predicate hiding (setStatus)
import Network.Wai.Utilities

import qualified Data.Set           as Set
import qualified Data.UUID.Tagged   as U
import qualified Galley.Data        as Data
import qualified Galley.Types.Teams as Teams

----------------------------------------------------------------------------
-- Group conversations

-- | The public-facing endpoint for creating group conversations.
--
-- See Note [managed conversations].
createGroupConversation :: UserId ::: ConnId ::: JsonRequest NewConvUnmanaged -> Galley Response
createGroupConversation (zusr ::: zcon ::: req) = do
    wrapped@(NewConvUnmanaged body) <- fromJsonBody req
    case newConvTeam body of
        Nothing    -> createRegularGroupConv zusr zcon wrapped
        Just tinfo -> createTeamGroupConv zusr zcon tinfo body

-- | An internal endpoint for creating managed group conversations. Will
-- throw an error for everything else.
internalCreateManagedConversation
    :: UserId ::: ConnId ::: JsonRequest NewConvManaged -> Galley Response
internalCreateManagedConversation (zusr ::: zcon ::: req) = do
    NewConvManaged body <- fromJsonBody req
    case newConvTeam body of
        Nothing -> throwM internalError
        Just tinfo -> createTeamGroupConv zusr zcon tinfo body

-- | A helper for creating a regular (non-team) group conversation.
createRegularGroupConv :: UserId -> ConnId -> NewConvUnmanaged -> Galley Response
createRegularGroupConv zusr zcon (NewConvUnmanaged body) = do
    name <- rangeCheckedMaybe (newConvName body)
    uids <- checkedConvSize (newConvUsers body)
    ensureConnected zusr (fromConvSize uids)
    c <- Data.createConversation zusr name (access body) (accessRole body) uids (newConvTeam body) (newConvMessageTimer body) (newConvReceiptMode body) (newConvUsersRole body)
    notifyCreatedConversation Nothing zusr (Just zcon) c
    conversationResponse status201 zusr c

-- | A helper for creating a team group conversation, used by the endpoint
-- handlers above. Allows both unmanaged and managed conversations.
createTeamGroupConv :: UserId -> ConnId -> ConvTeamInfo -> NewConv -> Galley Response
createTeamGroupConv zusr zcon tinfo body = do
    name <- rangeCheckedMaybe (newConvName body)
    teamMems <- Data.teamMembers (cnvTeamId tinfo)
    ensureAccessRole (accessRole body) (newConvUsers body) (Just teamMems)
    void $ permissionCheck zusr CreateConversation teamMems
    otherConvMems <-
        if cnvManaged tinfo then do
            let otherConvMems = filter (/= zusr) $ map (view userId) teamMems
            checkedConvSize otherConvMems
        else do
            otherConvMems <- checkedConvSize (newConvUsers body)
            -- In teams we don't have 1:1 conversations, only regular conversations. We want
            -- users without the 'AddRemoveConvMember' permission to still be able to create
            -- regular conversations, therefore we check for 'AddRemoveConvMember' only if
            -- there are going to be more than two users in the conversation.
            when (length (fromConvSize otherConvMems) > 1) $ do
                void $ permissionCheck zusr AddRemoveConvMember teamMems
            -- Team members are always considered to be connected, so we only check
            -- 'ensureConnected' for non-team-members.
            ensureConnected zusr (notTeamMember (fromConvSize otherConvMems) teamMems)
            pure otherConvMems
    conv <- Data.createConversation zusr name (access body) (accessRole body) otherConvMems (newConvTeam body) (newConvMessageTimer body) (newConvReceiptMode body) (newConvUsersRole body)
    now  <- liftIO getCurrentTime
    let d = Teams.EdConvCreate (Data.convId conv)
    let e = newEvent Teams.ConvCreate (cnvTeamId tinfo) now & eventData .~ Just d
    let notInConv = Set.fromList (map (view userId) teamMems) \\ Set.fromList (zusr : fromConvSize otherConvMems)
    for_ (newPush zusr (TeamEvent e) (map userRecipient (Set.toList notInConv))) push1
    notifyCreatedConversation (Just now) zusr (Just zcon) conv
    conversationResponse status201 zusr conv

----------------------------------------------------------------------------
-- Other kinds of conversations

createSelfConversation :: UserId -> Galley Response
createSelfConversation zusr = do
    c <- Data.conversation (Id . toUUID $ zusr)
    maybe create (conversationResponse status200 zusr) c
  where
    create = do
        c <- Data.createSelfConversation zusr Nothing
        conversationResponse status201 zusr c

createOne2OneConversation :: UserId ::: ConnId ::: JsonRequest NewConvUnmanaged -> Galley Response
createOne2OneConversation (zusr ::: zcon ::: req) = do
    NewConvUnmanaged j <- fromJsonBody req
    other  <- head . fromRange <$> (rangeChecked (newConvUsers j) :: Galley (Range 1 1 [UserId]))
    (x, y) <- toUUIDs zusr other
    when (x == y) $
        throwM $ invalidOp "Cannot create a 1-1 with yourself"
    case newConvTeam j of
        Just ti | cnvManaged ti -> throwM noManagedTeamConv
                | otherwise     -> checkBindingTeamPermissions zusr other (cnvTeamId ti)
        Nothing -> ensureConnected zusr [other]
    n <- rangeCheckedMaybe (newConvName j)
    c <- Data.conversation (Data.one2OneConvId x y)
    maybe (create x y n $ newConvTeam j) (conversationResponse status200 zusr) c
  where
    checkBindingTeamPermissions x y tid = do
        mems <- bindingTeamMembers tid
        void $ permissionCheck zusr CreateConversation mems
        unless (all (flip isTeamMember mems) [x, y]) $
            throwM noBindingTeamMembers

    create x y n tinfo = do
        c <- Data.createOne2OneConversation x y n (cnvTeamId <$> tinfo)
        notifyCreatedConversation Nothing zusr (Just zcon) c
        conversationResponse status201 zusr c

createConnectConversation :: UserId ::: Maybe ConnId ::: JsonRequest Connect -> Galley Response
createConnectConversation (usr ::: conn ::: req) = do
    j      <- fromJsonBody req
    (x, y) <- toUUIDs usr (cRecipient j)
    n      <- rangeCheckedMaybe (cName j)
    conv   <- Data.conversation (Data.one2OneConvId x y)
    maybe (create x y n j) (update n j) conv
  where
    create x y n j = do
        (c, e) <- Data.createConnectConversation x y n j
        notifyCreatedConversation Nothing usr conn c
        for_ (newPush (evtFrom e) (ConvEvent e) (recipient <$> Data.convMembers c)) $ \p ->
            push1 $ p
                  & pushRoute .~ RouteDirect
                  & pushConn  .~ conn
        conversationResponse status201 usr c

    update n j conv = let mems = Data.convMembers conv in
        conversationResponse status200 usr =<< if
            | usr `isMember` mems -> connect n j conv
            | otherwise           -> do
                now <- liftIO getCurrentTime
                mm  <- snd <$> Data.addMember now (Data.convId conv) usr roleNameWireAdmin
                let conv' = conv {
                    Data.convMembers = Data.convMembers conv <> toList mm
                }
                if null mems then
                    connect n j conv'
                else do
                    conv'' <- acceptOne2One usr conv' conn
                    if Data.convType conv'' == ConnectConv
                        then connect n j conv''
                        else return conv''

    connect n j conv
        | Data.convType conv == ConnectConv = do
            n' <- case n of
                Just  x -> do
                    Data.updateConversation (Data.convId conv) x
                    return . Just $ fromRange x
                Nothing -> return $ Data.convName conv
            t <- liftIO getCurrentTime
            let e = Event ConvConnect (Data.convId conv) usr t (Just $ EdConnect j)
            for_ (newPush (evtFrom e) (ConvEvent e) (recipient <$> Data.convMembers conv)) $ \p ->
                push1 $ p
                      & pushRoute .~ RouteDirect
                      & pushConn  .~ conn
            return $ conv { Data.convName = n' }
        | otherwise = return conv

-------------------------------------------------------------------------------
-- Helpers

conversationResponse :: Status -> UserId -> Data.Conversation -> Galley Response
conversationResponse s u c = do
    a <- conversationView u c
    return $ json a & setStatus s . location (cnvId a)

notifyCreatedConversation :: Maybe UTCTime -> UserId -> Maybe ConnId -> Data.Conversation -> Galley ()
notifyCreatedConversation dtime usr conn c = do
    now  <- maybe (liftIO getCurrentTime) pure dtime
    pushSome =<< mapM (toPush now) (Data.convMembers c)
  where
    route | Data.convType c == RegularConv = RouteAny
          | otherwise                      = RouteDirect

    toPush t m = do
        c' <- conversationView (memId m) c
        let e = Event ConvCreate (Data.convId c) usr t (Just $ EdConversation c')
        return $ newPush1 (evtFrom e) (ConvEvent e) (list1 (recipient m) [])
               & pushConn  .~ conn
               & pushRoute .~ route

toUUIDs :: UserId -> UserId -> Galley (U.UUID U.V4, U.UUID U.V4)
toUUIDs a b = do
    a' <- U.fromUUID (toUUID a) & ifNothing invalidUUID4
    b' <- U.fromUUID (toUUID b) & ifNothing invalidUUID4
    return (a', b')

accessRole :: NewConv -> AccessRole
accessRole b = fromMaybe Data.defRole (newConvAccessRole b)

access :: NewConv -> [Access]
access a = case Set.toList (newConvAccess a) of
    []     -> Data.defRegularConvAccess
    (x:xs) -> x:xs
