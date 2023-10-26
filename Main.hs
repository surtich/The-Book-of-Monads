module Chapter4 where
import Data.Traversable (for)
import Text.XHtml (action, ins)
import qualified Control.Monad as Chapter4

doesNotWork :: [IO ()]
doesNotWork = map (\name -> print ("Hello, " ++ name)) ["Alejandro", "Elena"]

mapM' :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM' f as = sequence' (map f as)

mapM'' :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM'' f [] = return []
mapM'' f (a:as) = f a >>= \b -> mapM'' f as >>= \bs -> return (b:bs)

works :: IO[()]
works = mapM' (\name -> print ("Hello, " ++ name)) ["Alejandro", "Elena"]

forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
forM = flip mapM'

-- similar to a for loop in other languages
works' = forM ["Alejandro", "Elena"] (\name -> print ("Hello, " ++ name))

void :: Functor m => m a -> m ()
void = fmap (const ())

-- avoid output of the list of IO actions
works'' :: IO ()
works'' = void $ forM ["Alejandro", "Elena"] (\name -> print ("Hello, " ++ name))

forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
forM_ xs f = void $ forM xs f 

sequence' :: (Monad m) => [m a] -> m [a]
sequence' [] = pure []
sequence' (ma:ms) = ma >>= \a -> sequence' ms >>= \as -> return (a:as)

sequence'' :: (Monad m) => [m a] -> m [a]
sequence'' = mapM' id

-- In module Control.Monad, package base
zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM f xs ys = sequence' (zipWith f xs ys)

replicateM :: Monad m => Int -> m a -> m [a]
replicateM f = sequence' . replicate f

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x:xs) = do
                    b      <- p x
                    xs'    <- filterM p xs
                    return (if b then x:xs' else xs')

-- foldM performs a left fold, like foldl
foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldM f a [] = return a
foldM f a (x:xs) = f a x >>= \y -> foldM f y xs

-- In module Control.Monad.Extra, package extra
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM p = foldM f ([], [])
    where f (a, b) x = p x >>= \y -> return (if y then (x:a, b) else (a, x:b))

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = foldM (\a b -> f b >>= \c -> return (a ++ c)) []

-- In module Control.Monad.Loops, package monad-loops
-- and Control.Monad.Extra, package extra
andM :: Monad m => [m Bool] -> m Bool
andM = allM id

orM :: Monad m => [m Bool] -> m Bool
orM = anyM id

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM p = orM . map p

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p = andM . map p

-- In module Control.Monad.Loops, package monad-loops
takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM _ [] = return []
takeWhileM p (x:xs) = p x >>= \y -> if y then takeWhileM p xs >>= \ys -> return (x:ys) else return []

dropWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
dropWhileM _ [] = return []
dropWhileM p (x:xs) = p x >>= \y -> if y then dropWhileM p xs else return (x:xs)

firstM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
firstM _ [] = return Nothing
firstM p (x:xs) = p x >>= \y -> if y then return (Just x) else firstM p xs

when :: Monad m => Bool -> m () -> m ()
when True  action = action
when False _      = return ()

unless :: Monad m => Bool -> m () -> m ()
unless cond = when (not cond)

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM cond th el = do c <-  cond
                    if c then th else el
-- Alternative definition
-- ifM = liftM3 (\c t e -> if c then t else e)

-- how can it return a value of any type b you want? The trick
-- is that forever f never returns, so it is free to promise whatever it wants — it will
-- never fulfill that promise anyway.
forever :: Monad m => m a -> m b
forever action = do action
                    forever action

whileM :: Monad m => m Bool -> m a -> m [a]
whileM cond action = do c <- cond
                        if c
                        then (:) <$> action <*> whileM cond action
                        else return []

whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ cond action = void $ whileM cond action

-- The function iterateWhile also checks a condition until it fails, but it does
-- so on the result of the previous iteration. For that reason, you do not get a list of
-- values, as with whileM , just the one that makes the iteration stop
iterateWhile :: Monad m => (a -> Bool) -> m a -> m a
iterateWhile  cond action = do a <- action
                               if cond a 
                               then iterateWhile cond action
                               else return a

iterateUntilM :: Monad m => (a -> m Bool) -> (a -> m a) -> a -> m a
iterateUntilM cond action a  = do c <- cond a
                                  if c
                                  then return a
                                  else action a >>= iterateUntilM cond action

loopM :: Monad m => (a -> m (Either a b)) -> a -> m b
loopM cond x = do r <- cond x
                  case r of
                    Left next -> loopM cond next
                    Right end -> return end

class Functor f => Traversable f where
  traverse :: Applicative m => (a -> m b) -> f a -> m (f b)

  mapM :: Monad m => (a -> m b) -> f a -> m (f b)
  mapM = Chapter4.traverse

  sequenceA :: Applicative m => f (m a) -> m (f a)
  sequenceA = Chapter4.traverse id

  sequence :: Monad m => f (m a) -> m (f a)
  sequence = Chapter4.sequenceA

instance Chapter4.Traversable Maybe where
  traverse  _ Nothing = pure Nothing
  traverse  f (Just x) = Just <$> f x

allJust :: [Maybe a] -> Maybe [a]
allJust = Prelude.sequenceA

newtype Identity a = Identity a -- do nothing with the value

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity a = Identity (f a)

instance Monad Identity where
  return = pure
  Identity a >>= f = f a
