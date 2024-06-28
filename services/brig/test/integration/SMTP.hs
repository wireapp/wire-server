{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
-- Disabling to stop warnings on HasCallStack
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module SMTP where

import Bilge qualified
import Control.Exception
import Data.Bifunctor
import Data.ByteString qualified as B
import Data.ByteString.Char8 qualified as C
import Data.Streaming.Network (bindRandomPortTCP)
import Data.Text (unpack)
import Data.Text.Lazy (fromStrict)
import Data.Time.Units
import Debug.Trace (traceIO)
import Imports
import Network.Mail.Mime
import Network.Mail.Postie qualified as Postie
import Network.Socket
import Pipes.Prelude qualified
import System.Logger qualified as Logger
import Test.Tasty
import Test.Tasty.HUnit
import Util
import Wire.EmailSending.SMTP

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

testSendMail :: Logger.Logger -> Bilge.Http ()
testSendMail lg = do
  receivedMailRef <- liftIO $ newIORef Nothing
  liftIO
    . withRandomPortAndSocket
    $ \(port, sock) ->
      withMailServer sock (mailStoringApp receivedMailRef) $
        do
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

testSendMailNoReceiver :: Logger.Logger -> Bilge.Http ()
testSendMailNoReceiver lg = do
  receivedMailRef <- liftIO $ newIORef Nothing
  liftIO
    . withRandomPortAndSocket
    $ \(port, sock) ->
      withMailServer sock (mailStoringApp receivedMailRef) $
        do
          traceIO "before initSMTP"
          conPool <- initSMTP lg "localhost" (Just port) Nothing Plain
          traceIO "finished initSMTP"
          caughtException <-
            handle @SomeException
              (const (pure True))
              (sendMail' @Second 1 lg conPool (emptyMail (Address Nothing "foo@example.com")) >> pure False)
          caughtException @? "Expected exception due to missing mail receiver."

testSendMailTransactionFailed :: Logger.Logger -> Bilge.Http ()
testSendMailTransactionFailed lg = do
  liftIO
    . withRandomPortAndSocket
    $ \(port, sock) ->
      withMailServer sock mailRejectingApp $
        do
          conPool <- initSMTP lg "localhost" (Just port) Nothing Plain
          caughtException <-
            handle @SomeException
              (const (pure True))
              (sendMail lg conPool someTestMail >> pure False)
          caughtException @? "Expected exception due to missing mail receiver."

testSendMailFailingConnectionOnStartup :: Logger.Logger -> Bilge.Http ()
testSendMailFailingConnectionOnStartup lg = do
  (port, sock) <- liftIO $ openRandomPortAndSocket
  liftIO $ gracefulClose sock 1000
  caughtError <-
    liftIO $
      handle @ErrorCall
        (const (pure True))
        (initSMTP lg "localhost" (Just port) Nothing Plain >> pure False)
  liftIO $ caughtError @? "Expected error (SMTP server unreachable.)"

testSendMailFailingConnectionOnSend :: Logger.Logger -> Bilge.Http ()
testSendMailFailingConnectionOnSend lg = do
  receivedMailRef <- liftIO $ newIORef Nothing
  conPool <-
    liftIO $
      withRandomPortAndSocket $
        \(port, sock) ->
          withMailServer
            sock
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

testSendMailTimeout :: Logger.Logger -> Bilge.Http ()
testSendMailTimeout lg = do
  mbException <-
    liftIO $
      withRandomPortAndSocket $
        \(port, sock) ->
          withMailServer sock (delayingApp (3 :: Second)) $
            do
              conPool <- initSMTP lg "localhost" (Just port) Nothing Plain
              handle @SMTPPoolException
                (\e -> pure (Just e))
                (sendMail' (500 :: Millisecond) lg conPool someTestMail >> pure Nothing)
  liftIO $ isJust mbException @? "Expected exception (SMTP server action timed out.)"
  liftIO $ mbException @?= Just SMTPConnectionTimeout

testSendMailTimeoutOnStartup :: Logger.Logger -> Bilge.Http ()
testSendMailTimeoutOnStartup lg = do
  mbException <-
    liftIO $
      withRandomPortAndSocket $
        \(port, sock) ->
          everDelayingTCPServer sock $
            handle @ErrorCall
              (\e -> pure (Just e))
              (initSMTP' (500 :: Millisecond) lg "localhost" (Just port) Nothing Plain >> pure Nothing)
  liftIO $ isJust mbException @? "Expected exception (SMTP server action timed out.)"

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

withMailServer :: Socket -> Postie.Application -> IO a -> IO a
withMailServer s app action = do
  bracket
    (forkIO $ Postie.runSettingsSocket Postie.def s app)
    killThread
    (const action)

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
        $> Postie.Accepted
    )

everDelayingTCPServer :: (HasCallStack) => Socket -> IO a -> IO a
everDelayingTCPServer sock action = listen sock 1024 >> action

withRandomPortAndSocket :: (MonadIO m) => ((PortNumber, Socket) -> IO a) -> m a
withRandomPortAndSocket action =
  liftIO $
    bracket
      (liftIO $ openRandomPortAndSocket)
      (\(_, s) -> liftIO $ close s)
      (\(p, s) -> action (p, s))

openRandomPortAndSocket :: IO (PortNumber, Socket)
openRandomPortAndSocket = bindRandomPortTCP "*6" <&> \x -> first fromIntegral x
