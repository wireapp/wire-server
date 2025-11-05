module Wire.BackgroundJobsPublisher.Null where

import Imports
import Polysemy
import Wire.BackgroundJobsPublisher (BackgroundJobsPublisher (..))

interpretBackgroundJobsPublisherNoConfig :: InterpreterFor BackgroundJobsPublisher r
interpretBackgroundJobsPublisherNoConfig = interpret $ \case
  PublishJob {} -> pure ()
