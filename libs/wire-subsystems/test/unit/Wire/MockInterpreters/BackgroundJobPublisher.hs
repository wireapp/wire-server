module Wire.MockInterpreters.BackgroundJobPublisher where

import Imports
import Polysemy
import Wire.BackgroundJobsPublisher (BackgroundJobsPublisher (..))

noopBackgroundJobsPublisher :: InterpreterFor BackgroundJobsPublisher r
noopBackgroundJobsPublisher = interpret $ \case
  PublishJob {} -> pure ()
