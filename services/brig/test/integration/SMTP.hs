module SMTP where

import Bilge
import Brig.SMTP
import Control.Exception
import Imports
import Network.Mail.Mime
import qualified Network.Mail.Postie as Postie
import qualified System.Logger as Logger
import Test.Tasty
import Test.Tasty.HUnit
import Util

-- TODO: Is IO needed here?
tests :: Manager -> Logger.Logger -> IO TestTree
tests m lg =
  pure $
    testGroup
      "SMTP"
      [ test m "should send no mail without receiver" $ testSendMailNoReceiver lg
      ]

-- TODO: Is Http the best Monad for this?
testSendMailNoReceiver :: Logger.Logger -> Http ()
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

--      traceM "Sent mail"
--      mbMail <-
--        retryWhileN 3 isJust $ do
--          readIORef receivedMailRef
--      isJust mbMail @? "Expected to receive mail"

withMailServer :: Postie.Application -> IO () -> IO ()
withMailServer app action =
  bracket
    (forkIO $ Postie.run 4242 app)
    killThread
    (const action)

mailStoringApp :: IORef (Maybe Postie.Mail) -> Postie.Application
mailStoringApp receivedMailRef mail =
  writeIORef receivedMailRef (Just mail)
    >> pure Postie.Accepted
