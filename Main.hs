data Tree a = Leaf a | Node (Tree a) (Tree a)

relabel :: Tree a -> Int -> (Tree (Int, a), Int)
relabel (Leaf x)   = \i -> (Leaf (i, x), i+1)
relabel (Node l r) = \i -> let (l', i1) = relabel l i
                               (r', i2) = relabel r i1
                           in  (Node l' r', i2)

type WithCounter a = Int -> (a, Int)

next :: WithCounter a -> (a -> WithCounter b) -> WithCounter b
f `next` g = \i -> let (r, i') = f i in g r i'

pureWC :: a -> WithCounter a
pureWC x = \i -> (x, i)


relabelWC :: Tree a -> WithCounter (Tree (a, Int))
relabelWC (Leaf x) = \i -> (Leaf (x, i), i + 1)
relabelWC (Node l r) =  relabelWC l `next` \l' ->
                      relabelWC r `next` \r' ->
                      pureWC (Node l' r')

type State s a = s -> (a, s)

pureST :: a -> State s a
pureST x = \i -> (x, i)

nextST :: State s a -> (a -> State s b) -> State s b
f `nextST` g = \i -> let (r, i') = f i in g r i'


relabelST :: Tree a -> State Int (Tree (a, Int))
relabelST (Leaf x) = \i -> (Leaf (x, i), i + 1)
relabelST (Node l r) = relabelST l `next` \l' ->
                       relabelST r `next` \r' ->
                       pureST (Node l' r')

plus :: [a] -> [a] -> [a]
plus xs ys = foldr (:) ys xs

mapList :: (a -> b) -> [a] -> [b]
mapList f = foldr ((:).f) []

singletonList :: a -> [a]
singletonList x = [x]

flattenList :: [[a]] -> [a]
flattenList [] = []
flattenList ([]:xxs) = flattenList xxs
flattenList ((x:xs):xxs) = x:flattenList (xs:xxs)

type Name = String
data Person = Person { name :: Name, age :: Int }

validateName :: Name -> Maybe Name
validateName = undefined

validateAge :: Int -> Maybe Int
validateAge = undefined

validatePerson :: Person -> Maybe Person
validatePerson p = case (validateName (name p), validateAge (age p)) of
                  (Nothing, _) -> Nothing
                  (_, Nothing) -> Nothing
                  _            -> Just p

thenMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
thenMaybe Nothing _ = Nothing
thenMaybe (Just x) f = f x

validatePerson' :: Person -> Maybe Person
validatePerson' p = validateName (name p) `thenMaybe` \name' ->
                    validateAge  (age p) `thenMaybe`  \age'  ->
                    Just (Person name' age')

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _ Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

singletonMaybe :: a -> Maybe a
singletonMaybe = Just

flattenMap :: Maybe (Maybe a) -> Maybe a
flattenMap (Just (Just x)) = Just x
flattenMap _               = Nothing

flattenMap' :: Maybe (Maybe a) -> Maybe a
flattenMap' mma = mma `thenMaybe`     \ma ->
                  ma  `thenMaybe`     \a  ->
                       singletonMaybe  a

flattenMap'' :: Maybe (Maybe a) -> Maybe a
flattenMap'' mma = thenMaybe mma id

thenMaybe' :: Maybe a -> (a -> Maybe b) -> Maybe b
thenMaybe' ma f = flattenMap (mapMaybe f ma)

-- s -> (s -> (a, s), s)
flattenState :: State s (State s a) -> State s a
flattenState f s = let (g, s')  = f s
                   in g s' 

flattenState' :: State s (State s a) -> State s a
flattenState' f = nextST f id

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f xs = flattenList (map f xs)

data Option a = Null | Some a

instance Functor Option where
  fmap _ Null = Null
  fmap f (Some x) = Some $ f x

instance Applicative Option where
  pure = Some
  Null   <*> _      = Null
  _      <*> Null   = Null
  Some f <*> Some a = Some (f a) 

instance Monad Option where
  return = pure
  Null   >>= _   = Null
  Some a >>= f   = f a

