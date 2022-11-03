module SMTP where

import qualified Bilge
import Brig.SMTP
import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Text (unpack)
import Data.Text.Lazy (fromStrict)
import Imports
import Network.Mail.Mime
import qualified Network.Mail.Postie as Postie
import qualified Pipes.Prelude
import qualified System.Logger as Logger
import Test.Tasty
import Test.Tasty.HUnit
import Util

-- TODO: Is IO needed here?
tests :: Bilge.Manager -> Logger.Logger -> IO TestTree
tests m lg =
  pure $
    testGroup
      "SMTP"
      [ test m "should send mail" $ testSendMail lg,
        -- TODO: Needs better description string: Actually, the SMTP server
        -- refuses to accept this mail.
        test m "should send no mail without receiver" $ testSendMailNoReceiver lg,
        test m "should throw when an SMTP transaction is aborted (SMTP error 554: 'Transaction failed')" $ testSendMailTransactionFailed lg,
        test m "should throw an error when the connection cannot be initiated on startup" $ testSendMailFailingConnectionOnStartup lg,
        test m "should throw when the server cannot be reached on sending" $ testSendMailFailingConnectionOnSend lg
      ]

testSendMailFailingConnectionOnSend :: Logger.Logger -> Bilge.Http ()
testSendMailFailingConnectionOnSend lg = do
  receivedMailRef <- liftIO $ newIORef Nothing
  conPool <-
    liftIO $
      withMailServer
        (mailStoringApp receivedMailRef)
        (initSMTP lg "localhost" (Just 4242) Nothing Plain)
  caughtException <-
    liftIO $
      handle @SomeException
        (const (pure True))
        (sendMail lg conPool someTestMail >> pure False)
  liftIO $ caughtException @? "Expected exception (SMTP server unreachable.)"

testSendMailFailingConnectionOnStartup :: Logger.Logger -> Bilge.Http ()
testSendMailFailingConnectionOnStartup lg = do
  caughtError <-
    liftIO $
      handle @ErrorCall
        (const (pure True))
        (initSMTP lg "localhost" (Just 4242) Nothing Plain >> pure False)
  liftIO $ caughtError @? "Expected error (SMTP server unreachable.)"

-- TODO: Is Http the best Monad for this?
testSendMailNoReceiver :: Logger.Logger -> Bilge.Http ()
testSendMailNoReceiver lg = do
  receivedMailRef <- liftIO $ newIORef Nothing
  liftIO
    . withMailServer (mailStoringApp receivedMailRef)
    $ do
      conPool <- initSMTP lg "localhost" (Just 4242) Nothing Plain
      caughtException <-
        handle @SomeException
          (const (pure True))
          (sendMail lg conPool (emptyMail (Address Nothing "foo@example.com")) >> pure False)
      caughtException @? "Expected exception due to missing mail receiver."

testSendMail :: Logger.Logger -> Bilge.Http ()
testSendMail lg = do
  receivedMailRef <- liftIO $ newIORef Nothing
  liftIO
    . withMailServer (mailStoringApp receivedMailRef)
    $ do
      conPool <- initSMTP lg "localhost" (Just 4242) Nothing Plain
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
  liftIO
    . withMailServer mailRejectingApp
    $ do
      conPool <- initSMTP lg "localhost" (Just 4242) Nothing Plain
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

withMailServer :: Postie.Application -> IO a -> IO a
withMailServer app action =
  bracket
    (forkIO $ Postie.run 4242 app)
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
