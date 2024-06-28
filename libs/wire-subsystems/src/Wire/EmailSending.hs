{-# LANGUAGE TemplateHaskell #-}

module Wire.EmailSending where

import Network.Mail.Mime (Mail)
import Polysemy (makeSem)

data EmailSending m r where
  SendMail :: Mail -> EmailSending m ()

makeSem ''EmailSending
