-- This file is part of the Wire Server implementation.
--
-- Copyright (C) 2020 Wire Swiss GmbH <opensource@wire.com>
--
-- This program is free software: you can redistribute it and/or modify it under
-- the terms of the GNU Affero General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option) any
-- later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more
-- details.
--
-- You should have received a copy of the GNU Affero General Public License along
-- with this program. If not, see <https://www.gnu.org/licenses/>.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Tests.Ropes.Aws.Ses
  ( tests,
  )
where

import Aws
import Aws.Ses
import Control.Error
import Control.Monad.Catch
import Control.Monad.Trans.Resource
import qualified Data.Text.Lazy.Encoding as T
import Imports
import Network.Mail.Mime
import Ropes.Aws
import Ropes.Aws.Ses
import qualified System.Logger as Logger
import Test.Tasty
import Test.Tasty.HUnit

sesCfg :: SesConfiguration NormalQuery
sesCfg = sesHttpsPost sesEuWest1

tests :: Env -> TestTree
tests e =
  testGroup
    "AWS SES Integration Tests"
    [ testCase "Successfully send raw mail" (sendRawMailSuccess e),
      testCase "Fail on invalid access key" (sendMailFailure e)
    ]

sendRawMailSuccess :: Env -> IO ()
sendRawMailSuccess e = do
  r <- runExceptT . trySes $ sendRequest e sesCfg =<< sendRawEmail testMimeMail
  case r of
    Right _ -> return ()
    Left x -> liftIO $ assertFailure (show x)

sendMailFailure :: Env -> IO ()
sendMailFailure e = do
  l <- Logger.new Logger.defSettings -- TODO: use mkLogger'?
  x <- newEnv l (getManager e) $ Just (AccessKeyId "abc", SecretAccessKey "eh?")
  r <- runExceptT . trySes $ sendRequest x sesCfg =<< sendRawEmail testMimeMail
  case r of
    Left (SesError _ "InvalidClientTokenId" _) -> return ()
    _ -> assertFailure "Expected error response"

trySes :: MonadIO m => ResourceT IO a -> ExceptT SesError m a
trySes = ExceptT . liftIO . try . runResourceT

testMimeMail :: Mail
testMimeMail =
  Mail
    { mailFrom = Address (Just "Mr. Test") "backend-integration@wire.com",
      mailTo = [Address (Just "Mr. Simulator") "success@simulator.amazonses.com"],
      mailCc = [],
      mailBcc = [],
      mailHeaders = [("Subject", "Just Testing!"), ("X-Zeta-Test", "Test")],
      mailParts =
        [ [ Part
              { partType = "text/plain; charset=UTF-8",
                partEncoding = QuotedPrintableText,
                partHeaders = [],
                partContent = PartContent $ T.encodeUtf8 "Hi Bjørn!",
                partDisposition = DefaultDisposition
              }
          ]
        ]
    }
