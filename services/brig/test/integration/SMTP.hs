module SMTP where

import Bilge
import Brig.SMTP
import Imports
import Network.Mail.Postie
import qualified System.Logger as Logger
import Test.Tasty
import Util

-- TODO: Is IO needed here?
tests :: Manager -> Logger.Logger -> IO TestTree
tests m lg =
  pure $
    testGroup
      "SMTP"
      [ test m "should send mail" $ testSendMail lg
      ]

-- TODO: Is Http the best Monad for this?
testSendMail :: Logger.Logger -> Http ()
testSendMail lg = do
  initSMTP lg Text (Maybe PortNumber) (Maybe (Username, Password)) Plain
