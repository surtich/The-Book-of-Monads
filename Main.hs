import Control.Monad.RWS (Sum (Sum))
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f m = State $ \s -> let (a, s') = runState m s
                             in (f a, s')
instance Applicative (State s) where
    pure a = State $ \s -> (a, s)
    mf <*> ma = do
        f <- mf
        f <$> ma
instance Monad (State s) where
    return = pure
    m >>= k  = State $ \s -> let (a, s') = runState m s
                             in runState (k a) s'

nextValue :: State Int Int
nextValue = State (\i -> (i, i + 1))

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State (\_ -> ((), s))

modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))

nextValue' :: State Integer Integer
nextValue' = do i <- get
                put (i+1)
                return i

nextValue'' :: State Integer Integer
nextValue'' = do i <- get
                 modify (+1)
                 return i

modify' :: (s -> s) -> State s ()
modify' f = do s' <- get
               put (f s')

evalState :: State s a -> s -> a
evalState m s = fst (runState m s)

execState :: State s a -> s -> s 
execState m s = snd (runState m s)

data Tree a = Leaf a | Node (Tree a) (Tree a)


relabel :: Tree a -> State Int (Tree Int)
relabel (Leaf _) = do
                    i <- get
                    modify(+1)
                    return (Leaf i)
relabel (Node l r) = relabel l >>= \l' ->
                     relabel r >>= \r' ->
                     return (Node l' r')

toList :: Tree a -> [a]
toList (Leaf x) = [x]
toList (Node l r) = toList l <> toList r

instance (Show a) => Show (Tree a) where
  show = show.toList
tree = Node (Node (Leaf 8) (Leaf 12)) (Leaf 30)
rtree = fst $ runState (relabel tree) 0

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f m = Reader $ \r -> f (runReader m r)

instance Applicative (Reader r) where
    pure a = Reader $ \_ -> a
    mf <*> ma = Reader (\r -> let f = runReader mf r
                                  a = runReader ma r
                             in f a)

instance Monad (Reader r) where
    return = pure
    m >>= k = Reader $ \r -> let a = runReader m r
                             in runReader (k a) r

ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = f <$> ask

withReader :: (r -> s) -> Reader s a -> Reader r a
withReader f m = Reader $ \r -> runReader m (f r)

local :: (r -> r) -> Reader r a -> Reader r a
local = withReader

newtype Writer w a = Writer { runWriter :: (w, a) }

instance Functor (Writer w) where
    fmap f m = Writer $ let (w, a) = runWriter m
                        in (w, f a)

instance (Monoid w) => Applicative (Writer w) where
    pure a = Writer (mempty, a)
    mf <*> ma = Writer $ let (w, f) = runWriter mf
                             (w', a) = runWriter ma
                         in (w <> w', f a)

instance (Monoid w) => Monad (Writer w) where
    return = pure
    m >>= k = Writer $ let (w, a) = runWriter m
                           (w', a') = runWriter (k a)
                       in (w <> w', a')

tell :: w -> Writer w ()
tell w = Writer (w, ())

example :: Writer (Sum Int) String
example = do tell (Sum 3)
             tell (Sum 4)
             return "seven"

pass :: Writer w (b, w -> w) -> Writer w b
pass  (Writer (w, (b, f))) = Writer (f w, b)

censor :: Monoid w => (w -> w) -> Writer w a -> Writer w a
censor f m = pass $ do a <- m
                       return (a, f)

mapWriter :: (v -> w) -> Writer v a -> Writer w a
mapWriter f (Writer (v, a)) = Writer (f v, a)

class Bifunctor f where
    bimap :: (a -> b) -> (c -> d) -> f a c -> f b d
    first :: (a -> b) -> f a c -> f b c
    second :: (c -> d) -> f a c -> f a d

instance Bifunctor Writer where
    bimap f g (Writer (v, a)) = Writer (f v, g a)
    first f (Writer (v, a)) = Writer (f v, a)
    second f (Writer (v, a)) = Writer (v, f a)

newtype Predicate a = Predicate { runPredicate :: a -> Bool }

through :: (a -> b) -> Predicate b -> Predicate a
through f (Predicate p) = Predicate (p . f)

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

instance Contravariant Predicate where
  contramap = through

class Profunctor f where
  lmap :: (v -> w) -> f w a -> f v a
  rmap :: (a -> b) -> f v a -> f v b
  -- Alternatively, map over both at
  dimap :: (v -> w) -> (a -> b) -> f w a -> f v b

instance Profunctor Reader where
  lmap f m = Reader $ \r -> runReader m (f r)
  rmap f m = Reader $ \r -> f (runReader m r)
  dimap f g m = Reader $ \r -> g (runReader m (f r))

newtype Returns r a = R (a -> r)

instance Contravariant (Returns r) where
  contramap f (R g) = R (g . f)