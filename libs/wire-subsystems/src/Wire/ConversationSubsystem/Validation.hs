module Wire.ConversationSubsystem.Validation where

import Imports
import Polysemy
import Polysemy.Error
import Polysemy.Input
import Wire.ConversationSubsystem.Config

-- Between 0 and (setMaxConvSize - 1)
newtype ConvSizeChecked f a = ConvSizeChecked {fromConvSize :: f a}
  deriving (Functor, Foldable, Traversable)

deriving newtype instance (Semigroup (f a)) => Semigroup (ConvSizeChecked f a)

deriving newtype instance (Monoid (f a)) => Monoid (ConvSizeChecked f a)

checkedConvSize ::
  (Member (Error ConversationSubsystemError) r, Member (Input ConversationSubsystemConfig) r, Foldable f) =>
  f a ->
  Sem r (ConvSizeChecked f a)
checkedConvSize x = do
  limit <- inputs (.maxConvSize)
  if length x <= fromIntegral limit
    then pure $ ConvSizeChecked x
    else throw $ ConvExceedsMaxSize limit
