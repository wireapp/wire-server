module SMTP where

import qualified Bilge
import Brig.SMTP
import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Text (unpack)
import Data.Text.Lazy (fromStrict)
import Data.Time.Units
import Imports
import Network.Mail.Mime
import qualified Network.Mail.Postie as Postie
import Network.Socket
import qualified Pipes.Prelude
import qualified System.Logger as Logger
import Test.Tasty
import Test.Tasty.HUnit
import Util

tests :: Bilge.Manager -> Logger.Logger -> TestTree
tests m lg =
  testGroup
    "SMTP"
    [ test m "should send mail" $ testSendMail lg,
      test m "should throw exception when SMTP server refuses to send mail (mail without receiver)" $ testSendMailNoReceiver lg,
      test m "should throw when an SMTP transaction is aborted (SMTP error 554: 'Transaction failed')" $ testSendMailTransactionFailed lg,
      test m "should throw an error when the connection cannot be initiated on startup" $ testSendMailFailingConnectionOnStartup lg,
      test m "should throw when the server cannot be reached on sending" $ testSendMailFailingConnectionOnSend lg,
      test m "should throw when sending times out" $ testSendMailTimeout lg,
      test m "should throw an error the initiation times out" $ testSendMailTimeoutOnStartup lg
    ]

testSendMailTimeoutOnStartup :: Logger.Logger -> Bilge.Http ()
testSendMailTimeoutOnStartup lg = do
  let port = 4242
  mbException <-
    liftIO $
      everDelayingTCPServer port $
        handle @ErrorCall
          (\e -> pure (Just e))
          (initSMTP' (500 :: Millisecond) lg "localhost" (Just port) Nothing Plain >> pure Nothing)
  liftIO $ isJust mbException @? "Expected exception (SMTP server action timed out.)"

testSendMailTimeout :: Logger.Logger -> Bilge.Http ()
testSendMailTimeout lg = do
  let port = 4243
  mbException <-
    liftIO $
      withMailServer port (delayingApp (3 :: Second)) $
        do
          conPool <- initSMTP lg "localhost" (Just port) Nothing Plain
          handle @SMTPPoolException
            (\e -> pure (Just e))
            (sendMail' (500 :: Millisecond) lg conPool someTestMail >> pure Nothing)
  liftIO $ isJust mbException @? "Expected exception (SMTP server action timed out.)"
  liftIO $ mbException @?= Just SMTPConnectionTimeout

testSendMailFailingConnectionOnSend :: Logger.Logger -> Bilge.Http ()
testSendMailFailingConnectionOnSend lg = do
  let port = 4244
  receivedMailRef <- liftIO $ newIORef Nothing
  conPool <-
    liftIO $
      withMailServer
        port
        (mailStoringApp receivedMailRef)
        (initSMTP lg "localhost" (Just port) Nothing Plain)
  caughtException <-
    liftIO $
      handle @SomeException
        (const (pure True))
        (sendMail lg conPool someTestMail >> pure False)
  liftIO $ caughtException @? "Expected exception (SMTP server unreachable.)"
  mbMail <- liftIO $ readIORef receivedMailRef
  liftIO $ isNothing mbMail @? "No mail expected (if there is one, the test setup is broken.)"

testSendMailFailingConnectionOnStartup :: Logger.Logger -> Bilge.Http ()
testSendMailFailingConnectionOnStartup lg = do
  let port = 4245
  caughtError <-
    liftIO $
      handle @ErrorCall
        (const (pure True))
        (initSMTP lg "localhost" (Just port) Nothing Plain >> pure False)
  liftIO $ caughtError @? "Expected error (SMTP server unreachable.)"

testSendMailNoReceiver :: Logger.Logger -> Bilge.Http ()
testSendMailNoReceiver lg = do
  let port = 4246
  receivedMailRef <- liftIO $ newIORef Nothing
  liftIO
    . withMailServer port (mailStoringApp receivedMailRef)
    $ do
      conPool <- initSMTP lg "localhost" (Just port) Nothing Plain
      caughtException <-
        handle @SomeException
          (const (pure True))
          (sendMail lg conPool (emptyMail (Address Nothing "foo@example.com")) >> pure False)
      caughtException @? "Expected exception due to missing mail receiver."

testSendMail :: Logger.Logger -> Bilge.Http ()
testSendMail lg = do
  let port = 4247
  receivedMailRef <- liftIO $ newIORef Nothing
  liftIO
    . withMailServer port (mailStoringApp receivedMailRef)
    $ do
      conPool <- initSMTP lg "localhost" (Just port) Nothing Plain
      sendMail lg conPool someTestMail
      mbMail <-
        retryWhileN 3 isJust $ do
          readIORef receivedMailRef
      isJust mbMail @? "Expected to receive mail"
      postieAddressAsString . rmSender <$> mbMail
        @=? (Just . unpack . addressEmail) someTestSender
      postieAddressAsString <$> (concat . maybeToList) (rmReceipients <$> mbMail)
        @=? [(unpack . addressEmail) someTestReceiver]
      let mailContent = (rmContent . fromJust) mbMail
      elem (unpack someTestBody) mailContent @? "Expected the SMTP server to receive the mail body."
      elem ("Subject: " ++ unpack someTestSubject) mailContent @? "Expected the SMTP server to receive the mail subject."
  where
    postieAddressAsString :: Postie.Address -> String
    postieAddressAsString addr =
      toString
        ( B.concat
            [ Postie.addressLocalPart addr,
              C.singleton '@',
              Postie.addressDomain addr
            ]
        )

someTestReceiver :: Address
someTestReceiver = Address Nothing "foo@example.com"

someTestSender :: Address
someTestSender = Address Nothing "bar@example.com"

someTestSubject :: Text
someTestSubject = "Some Subject"

someTestBody :: Text
someTestBody = "Some body"

someTestMail :: Mail
someTestMail =
  simpleMail'
    someTestReceiver
    someTestSender
    someTestSubject
    (fromStrict someTestBody)

toString :: B.ByteString -> String
toString bs = C.foldr (:) [] bs

testSendMailTransactionFailed :: Logger.Logger -> Bilge.Http ()
testSendMailTransactionFailed lg = do
  let port = 4248
  liftIO
    . withMailServer port mailRejectingApp
    $ do
      conPool <- initSMTP lg "localhost" (Just port) Nothing Plain
      caughtException <-
        handle @SomeException
          (const (pure True))
          (sendMail lg conPool mail >> pure False)
      caughtException @? "Expected exception due to missing mail receiver."
  where
    receiver = Address Nothing "foo@example.com"
    sender = Address Nothing "bar@example.com"
    subject = "Some Subject"
    body = "Some body"
    mail =
      simpleMail'
        receiver
        sender
        subject
        body

withMailServer :: PortNumber -> Postie.Application -> IO a -> IO a
withMailServer port app action = do
  bracket
    (forkIO $ Postie.run (portNumberToInt port) app)
    killThread
    (const action)
  where
    portNumberToInt = fromInteger . toInteger

data ReceivedMail = ReceivedMail
  { rmSender :: Postie.Address,
    rmReceipients :: [Postie.Address],
    -- | Contains all data sent to the SMTP server for this mail. (Including
    -- /From:/, /To:/, /Subject:/, ... lines.) I.e. `Postie.mailBody` is half of
    -- a lie; it's way more.
    rmContent :: [String]
  }
  deriving (Eq, Show)

mailStoringApp :: IORef (Maybe ReceivedMail) -> Postie.Application
mailStoringApp receivedMailRef mail = do
  c <- Pipes.Prelude.toListM (Postie.mailBody mail)
  let receivedMail =
        ReceivedMail
          { rmSender = Postie.mailSender mail,
            rmReceipients = Postie.mailRecipients mail,
            rmContent = C.unpack <$> c
          }
  writeIORef receivedMailRef (Just receivedMail)
  pure Postie.Accepted

mailRejectingApp :: Postie.Application
mailRejectingApp = const (pure Postie.Rejected)

mailAcceptingApp :: Postie.Application
mailAcceptingApp = const (pure Postie.Accepted)

delayingApp :: (TimeUnit t) => t -> Postie.Application
delayingApp delay =
  const
    ( (threadDelay . fromInteger . toMicroseconds) delay
        >> pure Postie.Accepted
    )

everDelayingTCPServer :: PortNumber -> IO a -> IO a
everDelayingTCPServer port action = withSocketsDo $ do
  addr <- resolve
  bracket (open addr) close (const action)
  where
    portString :: String
    portString = (show . toInteger) port
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      head <$> getAddrInfo (Just hints) Nothing (Just portString)
    open addr = bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      pure sock
