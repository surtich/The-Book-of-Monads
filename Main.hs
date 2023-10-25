{-# LANGUAGE MonadComprehensions #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use evalState" #-}
{-# HLINT ignore "Use const" #-}

data Tree a = Leaf a | Node (Tree a) (Tree a)

tree = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
ftree = Node (Leaf (+1)) (Leaf (*2))

toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Node l r) = toList l <> toList r

instance (Show a) => Show (Tree a) where
  show = show.toList

instance Functor Tree where
  fmap f (Leaf x)   = Leaf (f x)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Applicative Tree where
  pure = Leaf
  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  Leaf f   <*> Leaf x   = Leaf (f x)
  Leaf f   <*> Node l r = Node (fmap f l) (fmap f r)
  Node l r <*> x        = Node (l <*> x)  (r <*> x)

instance Monad Tree where
  return = pure
  (>>=) :: Tree a -> (a -> Tree b) -> Tree b
  Leaf x   >>= f = f x
  Node l r >>= f = Node (l >>= f) (r >>= f)

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State x) = State (\s -> let (a, s') = x s
                                  in  (f a, s'))

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State (a,)
  (<*>) :: State s (a -> b) -> State s a -> State s b
  State f <*> State x = State (\s -> let (f', s')  = f s
                                         (a , s'') =  x s'
                                     in  (f' a, s''))

instance Monad (State s) where
  return = pure
  (>>=) :: State s a -> (a -> State s b) -> State s b
  State x >>= f = State (\s -> let (a, s') = x s
                                   State y = f a
                               in y s')

relabel :: Tree a -> State Int (Tree (a, Int))
relabel (Leaf x) = State $ \i -> (Leaf (x, i), i + 1)
relabel (Node l r) = relabel l >>= \l' ->
                     relabel r >>= \r' ->
                     return (Node l' r')

rtree = fst $ runState (relabel tree) 0

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

-- do notation

relabel' :: Tree a -> State Int (Tree (a, Int))
relabel' (Leaf x) = State $ \i -> (Leaf (x, i), i + 1)
relabel' (Node l r) = do l' <- relabel' l
                         r' <- relabel' r
                         return (Node l' r')

validatePerson' :: Name -> Int -> Maybe Person
validatePerson' name age = do name' <- validateName name
                              age'  <- validateAge  age
                              return (Person name' age')
put :: s -> State s ()
put s = State $ \_ -> ((), s)

get :: State s s
get = State $ \s -> (s, s)

incCounter :: State Int ()
incCounter = do n <- get
                put (n + 1)

-- Usando MonadComprehensions

relabel'' :: Tree a -> State Int (Tree (a, Int))
relabel'' (Leaf x) = State $ \i -> (Leaf (x, i), i + 1)
relabel'' (Node l r) = [ Node l' r'
                          | l' <- relabel'' l
                          , r' <- relabel'' r ]