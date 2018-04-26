{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TupleSections       #-}

module API.User.Invitation (tests) where

import API.User.Util
import Bilge hiding (accept, timeout)
import Bilge.Assert
import Brig.Types
import Brig.Types.Intra
import Brig.Types.User.Auth hiding (user)
import Control.Arrow ((&&&))
import Control.Lens ((^?))
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Data.ByteString.Conversion
import Data.Id hiding (client)
import Data.List1 (List1)
import Data.Maybe
import Data.Monoid ((<>))
import Data.Text (Text)
import Galley.Types
import Gundeck.Types.Notification
import Test.Tasty hiding (Timeout)
import Test.Tasty.Cannon hiding (Cannon)
import Test.Tasty.HUnit
import Web.Cookie (parseSetCookie, setCookieName)
import Util

import qualified Brig.Options                as Opt
import qualified Data.List1                  as List1
import qualified Data.Text.Ascii             as Ascii
import qualified Data.UUID.V4                as UUID
import qualified Network.Wai.Utilities.Error as Error
import qualified Test.Tasty.Cannon           as WS

tests :: ConnectionLimit -> Opt.Timeout -> Maybe Opt.Opts -> Manager -> Brig -> Cannon -> Galley -> TestTree
tests _cl _at _conf p b c g = testGroup "invitation"
    [ test p "post /invitations - 201 accepted"     $ testInvitationEmail b g c
    , test p "post /invitations - 400 inactive"     $ testInvitationNotActivated b
    , test p "post /register - 400 invitee exists"  $ testInvitationCodeExists b
    , test p "post /register - 400 bad code"        $ testInvitationInvalidCode b
    , test p "post /register - 400 no wireless"     $ testInvitationCodeNoIdentity b
    , test p "get /invitations - 200 (paging)"      $ testInvitationPaging b
    , test p "get /invitations/info - 200"          $ testInvitationInfo b
    , test p "get /invitations/info - 400"          $ testInvitationInfoBadCode b
    ]

testInvitationEmail :: Brig -> Galley -> Cannon -> Http ()
testInvitationEmail brig galley cannon = do
    email <- randomEmail
    alice <- userId <$> randomUser brig
    bdy <- decodeBody <$> postInvitation brig alice (invite email)
    let Just invRef = inInvitation <$> bdy
    invCode <- getInvitationCode brig alice invRef

    WS.bracketR cannon alice $ \wsA -> do
        rsp2 <- post ( brig . path "/register"
                     . contentJson
                     . body (accept email invCode)
                     ) <!! const 201 === statusCode

        let Just (bob, Just email2) = (userId &&& userEmail) <$> decodeBody rsp2
        let zuid = parseSetCookie <$> getHeader "Set-Cookie" rsp2
        liftIO $ assertEqual "Wrong cookie" (Just "zuid") (setCookieName <$> zuid)

        -- Verify that they got connected
        assertConnections brig bob   [ConnectionStatus bob alice Accepted]
        assertConnections brig alice [ConnectionStatus alice bob Accepted]

        -- Verify that the invited user is active
        login brig (defEmailLogin email2) PersistentCookie
            !!! const 200 === statusCode

        -- Verify that the conversation IDs are set
        a2b <- decodeBody <$> get (brig . paths ["connections", toByteString' bob] . zUser alice)
        b2a <- decodeBody <$> get (brig . paths ["connections", toByteString' alice] . zUser bob)
        liftIO $ assertEqual "conv ids not equal" (ucConvId =<< a2b) (ucConvId =<< b2a)

        -- Verify that the 1-1 conversation exists
        cnv <- maybe (error "no conv ID") (fmap decodeBody . getConversation galley alice) (ucConvId =<< a2b)
        liftIO $ assertEqual "wrong conv type" (Just One2OneConv) (cnvType <$> cnv)

        -- WS receive timeout
        let t = 5 # Second
        -- Ensure alice gets the right events
        void . liftIO $ WS.assertMatch t wsA $ \n -> do
            -- conversation.member-join
            let e = List1.head (WS.unpackPayload n :: List1 Event)
            evtType e @?= MemberJoin
            evtFrom e @?= alice
        void . liftIO $ WS.assertMatch t wsA $ \n -> do
            -- user.connection
            let (typ, nm, uc) = decodeUCEvent . Object $ List1.head (ntfPayload n)
            typ @?= Just "user.connection"
            nm  @?= Just (Name "Bob")
            uc  @?= a2b
  where
    decodeUCEvent :: Value -> (Maybe Text, Maybe Name, Maybe UserConnection)
    decodeUCEvent e = do
        let ty = e ^? key "type" . _String
        let nm = e ^? key "user" . key "name" . _String
        let uc = maybeFromJSON =<< (e ^? key "connection")
        (ty, Name <$> nm, uc)

    invite email = InvitationRequest email (Name "Bob") (Message "Join me!") Nothing

    accept email code = RequestBodyLBS . encode $ object
        [ "name"            .= ("Bob" :: Text)
        , "email"           .= fromEmail email
        , "password"        .= defPassword
        , "invitation_code" .= code
        ]

testInvitationNotActivated :: Brig -> Http ()
testInvitationNotActivated brig = do
    u <- createAnonUser "Mr. Black" brig
    email <- randomEmail
    postInvitation brig (userId u) (invite email) !!! do
        const 403 === statusCode
        const (Just "no-identity") === fmap Error.label . decodeBody
  where
    invite email = InvitationRequest email (Name "Mr. Pink") (Message ".") Nothing

testInvitationCodeExists :: Brig -> Http ()
testInvitationCodeExists brig = do
    email <- randomEmail

    uid1 <- userId <$> randomUser brig
    rsp <- postInvitation brig uid1 (invite email) <!! const 201 === statusCode

    let Just invRef = inInvitation <$> decodeBody rsp
    invCode <- getInvitationCode brig uid1 invRef

    post (brig . path "/register" . contentJson . body (accept email invCode)) !!!
        const 201 === statusCode

    post (brig . path "/register" . contentJson . body (accept email invCode)) !!! do
        const 409                 === statusCode
        const (Just "key-exists") === fmap Error.label . decodeBody

    postUser "dilbert" "someoneelse@wearezeta.com" invCode Nothing brig !!! do
        const 400                              === statusCode
        const (Just "invalid-invitation-code") === fmap Error.label . decodeBody
  where
    invite email = InvitationRequest email (Name "Bob") (Message "Join me!") Nothing

    accept email code = RequestBodyLBS . encode $ object
        [ "name"            .= ("Bob" :: Text)
        , "email"           .= fromEmail email
        , "password"        .= defPassword
        , "invitation_code" .= code
        ]

testInvitationInvalidCode :: Brig -> Http ()
testInvitationInvalidCode brig = do
    -- Syntactically invalid
    let code1 = InvitationCode (Ascii.unsafeFromText "8z6JVcO1o4o¿9kFeb4Y3N-BmhIjH6b33")
    postUser "dilbert" "foo7@wearezeta.com" (Just code1) Nothing brig !!! do
        const 400 === statusCode
        const (Just "bad-request") === fmap Error.label . decodeBody
    -- Syntactically valid but semantically invalid
    iid <- liftIO $ randomBytes 24
    let code2 = InvitationCode (Ascii.encodeBase64Url iid)
    postUser "dilbert" "foo7@wearezeta.com" (Just code2) Nothing brig !!! do
        const 400 === statusCode
        const (Just "invalid-invitation-code") === fmap Error.label . decodeBody

testInvitationCodeNoIdentity :: Brig -> Http ()
testInvitationCodeNoIdentity brig = do
    uid <- liftIO $ Id <$> UUID.nextRandom
    post (brig . path "/register" . contentJson . body (user uid)) !!! do
        const 403                       === statusCode
        const (Just "missing-identity") === fmap Error.label . decodeBody
  where
    user u = RequestBodyLBS . encode $ object
        [ "name"            .= ("Bob" :: Text)
        , "invitation_code" .= u
        ]

testInvitationPaging :: Brig -> Http ()
testInvitationPaging b = do
    u <- userId <$> randomUser b
    replicateM_ total $ do
        email <- randomEmail
        postInvitation b u (invite email) !!! const 201 === statusCode
    foldM_ (next u 2) (0, Nothing) [2,2,1,0]
    foldM_ (next u total) (0, Nothing) [total,0]
  where
    total = 5

    next :: UserId -> Int -> (Int, Maybe InvitationId) -> Int -> Http (Int, Maybe InvitationId)
    next u step (count, start) n = do
        let count' = count + step
        let range = queryRange (toByteString' <$> start) (Just step)
        r <- get (b . path "/invitations" . zUser u . range) <!!
            const 200 === statusCode
        let (invs, more) = (fmap ilInvitations &&& fmap ilHasMore) $ decodeBody r
        liftIO $ assertEqual "page size" (Just n) (length <$> invs)
        liftIO $ assertEqual "has more" (Just (count' < total)) more
        return . (count',) $ invs >>= fmap inInvitation . listToMaybe . reverse

    invite email = InvitationRequest email (Name "You") (Message "sth") Nothing

testInvitationInfo :: Brig -> Http ()
testInvitationInfo brig = do
    email    <- randomEmail
    uid      <- userId <$> randomUser brig

    let invite = InvitationRequest email (Name "Bob") (Message "Join me!") Nothing
    Just inv <- decodeBody <$> postInvitation brig uid invite

    Just invCode    <- getInvitationCode brig uid (inInvitation inv)
    Just invitation <- getInvitation brig invCode

    liftIO $ assertEqual "Invitations differ" inv invitation

testInvitationInfoBadCode :: Brig -> Http ()
testInvitationInfoBadCode brig = do
    -- The code contains non-ASCII characters after url-decoding
    let icode = "8z6JVcO1o4o%C2%BF9kFeb4Y3N-BmhIjH6b33"
    get (brig . path ("/invitations/info?code=" <> icode)) !!!
        const 400 === statusCode
