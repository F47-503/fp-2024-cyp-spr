module Main (main) where
import Eval
import Expr
import Simplify
import Parser (runExprParser)
import qualified Data.Map.Strict as Map
import Text.Read (readMaybe)

mapValuesParser s = case words s of
    _:_:_:_ -> Left "Too many arguments"
    [x, y] -> case readMaybe y of
        Just r -> Right (x, r)
        Nothing -> Left "Cannot read second argument to Double"
    _ -> Left "Too few arguments"


readMap :: IO (Map.Map String Double)
readMap = do
    putStrLn "Please input a variables list in form of: \"Variable Value\", e.g. \"x 3.0\". If you are done with that, leave the blank line."
    readLinesToMap Map.empty
  where
    readLinesToMap mp = do
        line <- getLine
        case line of
            "" -> return mp
            s -> case mapValuesParser s of
                Left err -> do
                    putStrLn err
                    readLinesToMap mp
                Right (x, y) -> do
                    readLinesToMap (Map.insert x y mp)


main :: IO ()
main = do
    putStrLn "Pleas input an expression in prefix notation, e.g. \"+ 123 xyz\""
    line <- getLine
    case runExprParser line of
        Right e -> do
            putStrLn ("Let's simplify your expression: " ++ show (simplify e))
            mp <- readMap
            case runEval e mp of
                Left err -> print err
                Right res -> putStrLn ("Your expression is equal to: " ++ show res)
        Left x -> print x
