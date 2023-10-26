module Chapter5 where

newtype LMaybe a = LMaybe { unLMaybe :: Maybe a } deriving (Eq, Ord, Show)
newtype RMaybe a = RMaybe { unRMaybe :: Maybe a } deriving (Eq, Ord, Show)
instance Semigroup (LMaybe a) where
  (<>) (LMaybe Nothing) lm = lm
  (<>) lm _ = lm

instance Monoid (LMaybe a) where
  mempty = LMaybe Nothing

instance Semigroup (RMaybe a) where
  (<>) rm (RMaybe Nothing) = rm
  (<>) _ rm = rm

instance Monoid (RMaybe a) where
  mempty = RMaybe Nothing

(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = \a -> g a >>= f  