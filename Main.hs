{-# LANGUAGE MonadComprehensions #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use evalState" #-}
{-# HLINT ignore "Use const" #-}

module Chapter3 where 

import Data.Char (toUpper)

plus :: Maybe Int -> Maybe Int -> Maybe Int
plus x y = x >>= \x' -> y >>= \y' -> return (x' + y')

lift2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2 f = \ma mb -> ma >>= \a -> mb >>= \b -> return (f a b)

maybeEight = lift2 (+)  (Just 3) (Just 5)

ap :: Monad m => m (b -> c) -> m b -> m c
ap mf mb = mb >>= \b -> mf >>= \f -> return (f b)

lift2' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2' f ma mb = fmap f ma `ap` mb

lift2'' :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2'' f = ap.fmap f

fmap' :: (Monad f) =>  (a -> b) -> f a  -> f b
fmap' f = ap (pure f)

newtype ZipList a = ZipList { getZipList :: [a]}

instance (Show a) => Show (ZipList a) where
  show (ZipList xs) = show xs

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (fmap f xs)

instance Applicative ZipList where
  pure x = ZipList [x]
  ZipList fs <*> ZipList xs = ZipList (zipWith id fs xs)

(<$) :: Functor f => a -> f b -> f a
(<$) = fmap . const

(<*) :: Applicative f => f a -> f b -> f a
(<*) fa fb = const <$> fa <*> fb

(*>) :: Applicative f => f a -> f b -> f b
(*>) fa fb = (\ _ x -> x) <$> fa <*> fb

type Name = String
data Person = Person { name :: Name, age :: Int }

validateName :: Name -> Maybe Name
validateName = undefined

validateAge :: Int -> Maybe Int
validateAge = undefined

validatePerson :: Name -> Int -> Maybe Person
validatePerson name age = validateName name >>= \name' ->
                          validateAge  age  >>= \age'  ->
                          return (Person name' age')

upperName :: Name -> Int -> Maybe [Char]
upperName name age = map toUpper <$> validateName name Prelude.<* validateAge age

upperName' name age = map toUpper Prelude.<$ validateAge age <*> validateName name

person1 :: Name -> Maybe Person
person1 name = Person <$> validateName name <*> pure 20

toNestedTriple :: (a, b, c) -> (a, (b, c))
toNestedTriple (a, b, c) = (a, (b, c))

toNestedQuadruple :: (a, b, c, d) -> (a, (b, (c, d)))
toNestedQuadruple (a, b, c, d) = (a, toNestedTriple (b, c, d))

class Applicative f => Monoidal f where
  unit :: f ()
  unit = pure ()

  (**) :: f a -> f b -> f (a, b)
  (**) fa fb = (,) <$> fa <*> fb

pure' :: (Monoidal f) => a -> f a
pure' a = fmap (\_ -> a) unit

ap' :: (Monoidal f) => f (a -> b) -> f a -> f b
ap' ff fa = fmap (\(g, a) -> g a) (ff Chapter3.** fa)