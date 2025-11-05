{-# LANGUAGE TemplateHaskell #-}

module Wire.BackgroundJobsPublisher where

import Data.Id
import Polysemy
import Wire.API.BackgroundJobs (JobPayload)

data BackgroundJobsPublisher m a where
  PublishJob :: JobId -> JobPayload -> BackgroundJobsPublisher m ()

makeSem ''BackgroundJobsPublisher
