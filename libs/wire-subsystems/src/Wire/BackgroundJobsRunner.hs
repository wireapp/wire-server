{-# LANGUAGE TemplateHaskell #-}

module Wire.BackgroundJobsRunner where

import Polysemy
import Wire.API.BackgroundJobs (Job)

data BackgroundJobsRunner m a where
  RunJob :: Job -> BackgroundJobsRunner m ()

makeSem ''BackgroundJobsRunner
