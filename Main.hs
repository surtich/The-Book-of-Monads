--module Main where

fizzBuffFor number
  | number `rem` 15 == 0 = "FizzBuzz"
  | number `rem` 3 == 0 = "Fizz"
  | number `rem` 5 == 0 = "Buzz"
  | otherwise = show number

fizzBuff number =
  if number == 0
  then ""
  else
    let
      head = fizzBuff (number - 1)
      middle = if head == "" then "" else " "
      tail = fizzBuffFor number

    in
      head ++ middle ++ tail


curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (a, b) = f a b

sieve :: [Int]
sieve =
  let
    sieve' :: [Int] -> [Int]
    sieve' list =
      let
        prime = head list
        list' = filter (\number -> number `rem` prime /= 0) list
      in
        prime : sieve' list'
  in
    sieve' [2..]


-- isBalanced xs =
--   let
--     isBalanced' ('(':xs) opened = isBalanced' xs (opened + 1)
--     isBalanced' (')':xs) opened = ((opened > 0) && isBalanced' xs (opened - 1))
--     isBalanced' (x:xs) opened = isBalanced' xs opened
--     isBalanced' [] opened = opened == 0
--   in
--     isBalanced' xs 0

isBalanced s =
  isBalanced' 0 s
  where
    isBalanced' count s
      | null s = count == 0
      | head s == '(' = isBalanced' (count + 1) (tail s)
      | head s == ')' = count > 0 && isBalanced' (count - 1) (tail s)
      | otherwise = isBalanced' count (tail s)

foldl' f acc xs =
  if   null xs
  then acc
  else
    let
      x    = head xs
      xs'  = tail xs
      acc' = f acc x
    in
      foldl' f acc' xs'

foldr' f acc xs =
  if   null xs
  then acc
  else
    let
      x    = head xs
      xs'  = tail xs
      acc' = foldr' f acc xs'
    in
      f x acc'

map' :: (a -> b) -> [a] -> [b]
map' f = foldr' ((:) . f)  []

filter' f = foldr' (\x acc -> if f x then x:acc else acc) []


findFirst :: (a -> Bool) -> [a] -> Maybe a
findFirst predicate =
  foldr (\x acc ->  if predicate x then Just x else acc) Nothing

fib :: [Int]
fib = fib' 0 1
  where
    fib' :: Int -> Int -> [Int]
    fib' a b = a : fib' b (a + b)

reverse' :: [a] -> [a]
reverse' = foldr (\x acc -> acc <> [x]) []

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys)  = f x y : zipWith' f xs ys

zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = [f x y | x <- xs, y <- ys]

zipWith''' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith''' f xs ys = foldr (\(x, y) acc -> f x y :acc ) [] (zip xs ys)

concatMap' :: (a -> b) -> [[a]] -> [b] 
concatMap' f = foldr (\xxs acc -> map f xxs <> acc ) []

