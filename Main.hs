module Chapter7 where

import Data.Bifunctor (Bifunctor, bimap)
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

class Monad m => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a

instance Monoid e => Alternative (Either e) where
  empty = Left mempty
  Left _ <|> r = r
  l      <|> _ = l

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> r = r
  l       <|> _ = l

instance Alternative [] where
  empty = []
  (<|>) = (++)

guard :: Alternative f => Bool -> f ()
guard True  = pure ()
guard False = empty

validateAge age = do 
                  --age <- readMaybe age
                  guard (age >= 18)
                  return age

asum :: (Traversable t, Alternative m) => t (m a) -> m a
asum = foldr (<|>) empty

msum :: (Traversable t, MonadPlus m) => t (m a) -> m a
msum = foldr mplus mzero

mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a
mfilter p x = do x' <- x
                 if p x' then return x' else mzero

headZ :: MonadPlus m => [a] -> m a
headZ [] = mzero
headZ (x:_) = return x

atZ :: (MonadPlus m, Alternative m) => [a] -> Int -> m a
atZ xs i = do guard (i < length xs)
              return (xs !! i)

type Person = String

people :: [Person]
people = ["Alejandro", "Elena", "Quique", "John", "Mary", "Tom"]

pcRels :: [(Person, Person)]
pcRels = [("Alejandro", "Quique"), ("Elena", "Quique")
         ,("John", "Mary"), ("John", "Tom"), ("Mary", "Tim")]

gpgcRels :: [(Person, Person)]
gpgcRels = do (p1, c) <- pcRels
              (c', g) <- pcRels
              guard (c == c')
              return (p1, g)

siblingsRels = do (p1, c) <- pcRels
                  (p2, c') <- pcRels
                  guard (c == c' && p1 /= p2)
                  return (p1, p2)

sums :: [Integer] -> [(Integer, Integer, Integer)]
sums xs = do x <- xs
             y <- xs
             z <- xs
             guard (x + y == z)
             return (x, y, z)

pyts :: [Integer] -> [(Integer, Integer, Integer)]
pyts xs = do x <- xs
             y <- xs
             z <- xs
             guard (x^2 + y^2 == z^2)
             return (x, y, z)

triples ns = sums ns <|> pyts ns

class Monad m => MonadError e m where
  throwError :: e -> m a
  catchError :: m a -> (e -> m a) -> m a

instance (MonadError e) (Either e) where
  throwError = Left
  catchError (Left e) f = f e
  catchError r _ = r

instance (MonadError e) Maybe where
  throwError _ = Nothing
  catchError Nothing f = Nothing
  catchError r _ = r

-- instance Bifunctor Either where
--   bimap f _ (Left x) = Left (f x)
--   bimap _ g (Right y) = Right (g y)