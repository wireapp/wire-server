{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Tests.Ropes.Aws.Ses (tests) where

import Imports
import Aws
import Aws.Ses
import Control.Error
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import Network.Mail.Mime
import Ropes.Aws
import Ropes.Aws.Ses
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Text.Lazy.Encoding as T
import qualified System.Logger as Logger

sesCfg :: SesConfiguration NormalQuery
sesCfg = sesHttpsPost sesEuWest1

tests :: Env -> TestTree
tests e =
    testGroup "AWS SES Integration Tests"
        [ testCase "Successfully send raw mail" (sendRawMailSuccess e)
        , testCase "Fail on invalid access key" (sendMailFailure e)
        ]

sendRawMailSuccess :: Env -> IO ()
sendRawMailSuccess e = do
    r <- runExceptT . trySes $ sendRequest e sesCfg =<< sendRawEmail testMimeMail
    case r of
        Right _ -> return ()
        Left  x -> liftIO $ assertFailure (show x)

sendMailFailure :: Env -> IO ()
sendMailFailure e = do
    l <- Logger.new Logger.defSettings  -- TODO: use mkLogger'?
    x <- newEnv l (getManager e) $ Just (AccessKeyId "abc", SecretAccessKey "eh?")
    r <- runExceptT . trySes $ sendRequest x sesCfg =<< sendRawEmail testMimeMail
    case r of
        Left (SesError _ "InvalidClientTokenId" _) -> return ()
        _ -> assertFailure "Expected error response"

trySes :: MonadIO m => ResourceT IO a -> ExceptT SesError m a
trySes = ExceptT . liftIO . try . runResourceT

testMimeMail :: Mail
testMimeMail = Mail
    { mailFrom    = Address (Just "Mr. Test") "backend-integration@wire.com"
    , mailTo      = [ Address (Just "Mr. Simulator") "success@simulator.amazonses.com" ]
    , mailCc      = []
    , mailBcc     = []
    , mailHeaders = [("Subject", "Just Testing!"), ("X-Zeta-Test", "Test")]
    , mailParts   = [ [ Part
                       { partType     = "text/plain; charset=UTF-8"
                       , partEncoding = QuotedPrintableText
                       , partHeaders  = []
                       , partContent  = PartContent $ T.encodeUtf8 "Hi Bjørn!"
                       , partDisposition = DefaultDisposition
                       }
                    ] ]
    }
