module Brig.Email.Env where

import qualified Brig.Options as Opt
import qualified Brig.SMTP as SMTP
import Brig.Utils
import Control.Lens ((^.))
import Data.Maybe
import Imports
import System.Logger.Class
import Util.Options (epHost, epPort)

data Env = EmailAWS | EmailSMTP SMTP.SMTP | NoEmail

mkEnv :: Logger -> Opt.EmailOpts -> IO Env
mkEnv _ (Opt.EmailAWS _) = return $ EmailAWS
mkEnv _ Opt.NoEndpoint = return NoEmail
mkEnv lgr (Opt.EmailSMTP s) = do
  let host = Opt.smtpEndpoint s ^. epHost
      port = Just $ fromInteger $ toInteger $ (Opt.smtpEndpoint s) ^. epPort
  smtpCredentials <- case Opt.smtpCredentials s of
    Just (Opt.EmailSMTPCredentials u p) -> do
      pass <- initCredentials p
      return $ Just (SMTP.Username u, SMTP.Password pass)
    _ -> return Nothing
  smtp <- SMTP.initSMTP lgr host port smtpCredentials (Opt.smtpConnType s)
  return $ EmailSMTP smtp
