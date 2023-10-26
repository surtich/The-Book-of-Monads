import Control.Monad.Reader (Reader, runReader, ask, asks)
import Control.Applicative (Alternative)
type Name = String

data Expr = Literal Integer | Var Name | Op Op Expr Expr

data Op = Add | Subtract | Multiply | Divide

type Assignment = [(Name, Integer)]

eval :: Expr -> Assignment -> Maybe Integer
eval (Literal n) _ = return n
eval (Var v)     a = lookup v a
eval (Op o x y)  a = do
  u <- eval x a
  v <- eval y a
  case o of
    Add      -> return (u + v)
    Subtract -> return (u - v)
    Multiply -> return (u * v)
    Divide   -> if v == 0 then Nothing
                          else return (u `div` v)

eval' :: Expr -> Reader Assignment (Maybe Integer)
eval' (Literal n) = return (Just n)
eval' (Var v)     = ask >>= return . lookup v
eval' (Op o x y)  = do
  u <- eval' x
  v <- eval' y
  case (u, v) of
    (Nothing, _) -> return Nothing
    (_, Nothing) -> return Nothing
    (Just u', Just v') ->
      case o of
        Add      -> return (Just (u' + v'))
        Subtract -> return (Just (u' - v'))
        Multiply -> return (Just (u' * v'))
        Divide   -> if v' == 0 then return Nothing
                    else return (Just (u' ` div ` v'))
expr = Op Multiply
              (Op Add (Literal 3) (Var "x"))
              (Op Subtract
                (Op Multiply (Literal 2) (Var "y"))
                (Op Divide (Var "z") (Literal 4)))

vars = [("x", 3), ("y", 2), ("z", 8)]

newtype Evaluator a = Evaluator { runEvaluator :: Reader Assignment (Maybe a) }

instance Functor Evaluator where
  fmap f (Evaluator r) = Evaluator (fmap (fmap f) r)

instance Applicative Evaluator where
  pure = Evaluator . pure . pure
  (<*>) :: Evaluator (a -> b) -> Evaluator a -> Evaluator b
  Evaluator f <*> Evaluator r = Evaluator $ (<*>) <$> f <*> r

instance Monad Evaluator where
  return = pure
  (Evaluator x) >>= f = Evaluator $ x >>= \x' -> case x' of
    Nothing -> return Nothing
    Just x'' -> let Evaluator y = f x'' in y

evalFail :: Evaluator a
evalFail = Evaluator $ return Nothing

eval'' :: Expr -> Evaluator Integer
eval'' (Literal n) = return n
eval'' (Var v) = do
  a <- Evaluator $ asks Just
  maybe evalFail return (lookup v a)
eval'' (Op o x y) = do
  u <- eval'' x
  v <- eval'' y
  case o of
    Add      -> return (u + v)
    Subtract -> return (u - v)
    Multiply -> return (u * v)
    Divide   -> if v == 0 then evalFail
                          else return (u `div` v)
newtype (f :.: g) a = Compose (f (g a))

instance (Functor f, Functor g) => Functor (f :.: g) where
  fmap f (Compose x) = Compose $ fmap (fmap f) x

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure = Compose . pure . pure
  Compose f <*> Compose x = Compose $ (<*>) <$> f <*> x


main :: IO ()
main = do
  print $ eval expr vars
  print $ runReader (eval' expr) vars
  print $ (runReader $ runEvaluator (eval'' expr)) vars