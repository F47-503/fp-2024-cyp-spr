module Eval where

import Expr
import qualified Data.Map.Strict as Map
import Control.Monad.State

checkErrors :: (Either Error a, Either Error a) -> Either Error (a, a)
checkErrors (Left x, _) = Left x
checkErrors (_, Left y) = Left y
checkErrors (Right x, Right y) = Right (x, y)

alwaysCorrect :: a -> a -> Bool
alwaysCorrect _ _ = True

computeOrError :: (a -> a -> a) -> (a -> a -> Bool) -> String -> Either Error a -> Either Error a -> Either Error a
computeOrError f predicate errorMsg expA expB =
  case checkErrors (expA, expB) of
  Left err -> Left err
  Right (x, y) -> if predicate x y then Right (f x y) else Left (Error errorMsg)

computeShell f x y = f <$> eval x <*> eval y

eval :: (Floating a, Ord a) => Expr a -> State (Map.Map String a) (Either Error a)
eval (Var x) = do
    vars <- get
    return (case Map.lookup x vars of
      Nothing -> Left (Error "Unknown variable: x")
      (Just r) -> Right r)
eval (Expr x) = return (Right x)
eval (Sq x) = computeShell (computeOrError (const sqrt) (\_ -> (<=) 0) "sqrt from negative taken") x x
eval ((:+) x y) = computeShell (computeOrError (+) alwaysCorrect "") x y
eval ((:-) x y) = computeShell (computeOrError (-) alwaysCorrect "") x y
eval ((:*) x y) = computeShell (computeOrError (*) alwaysCorrect "") x y
eval ((:/) x y) = computeShell (computeOrError (/) (\_ -> (/=) 0) "divided by zero") x y
eval ((:^) x y) = computeShell (computeOrError (**) (\a _ -> (<) 0 a) "power used with non-positive base") x y

runEval :: (Floating a, Ord a) => Expr a -> Map.Map String a -> Either Error a
runEval m = evalState (eval m)