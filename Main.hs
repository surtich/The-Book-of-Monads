import Data.STRef (newSTRef, modifySTRef, readSTRef)
import Control.Monad.ST (runST)
weirdSumm = do x <- newSTRef 1
               y <- newSTRef 1
               modifySTRef y (+1)
               (+) <$> readSTRef x <*> readSTRef y

result = runST weirdSumm

greet :: IO ()
greet = do putStr "Enter your name: "
           name <- getLine
           putStrLn ("Hello, " ++ name ++ "!")