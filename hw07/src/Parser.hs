{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use void" #-}
module Parser where
import Expr
import Control.Applicative (asum, Alternative (..))
import Data.Char (isAlphaNum, isDigit, isSpace)

newtype Parser a = Parser {parse :: String -> Either ParseError (String, a)}

newtype ParseError = ParseError String
  deriving Eq

instance Show ParseError where
  show (ParseError x) = "Parse Error: " ++ show x

failedParse :: String -> Parser a
failedParse err = Parser (const (Left (ParseError err)))

instance Functor Parser where
  fmap f x = Parser (fmap (fmap f) . parse x)
instance Applicative Parser where
  pure x = Parser (\s -> Right (s, x))
  (<*>) x y = Parser appliance
    where appliance s =
            case parse x s of
            Right (t, f) -> case parse y t of
                    Right (t1, el) -> Right (t1, f el)
                    Left err -> Left err
            Left err -> Left err

instance Monad Parser where
  (>>=) x f = Parser binding
    where binding s =
            case parse x s of
            Right (t, y) -> parse (f y) t
            Left err -> Left err

instance Alternative Parser where
  empty = failedParse "Parser is empty"
  (<|>) x y = Parser chosenParse
    where chosenParse s =
            case parse x s of
            Right r -> Right r
            Left _ -> parse y s

parseIf :: (Char -> Bool) -> Parser Char
parseIf cond = Parser parsePattern
  where parsePattern s =
          case s of
          h : t -> if cond h then Right (t, h) else Left (ParseError "Wrong symbol")
          _ -> Left (ParseError "Nothing to parse")

filterChar :: Char -> Parser Char
filterChar char = parseIf (char ==)

parseNumber :: (Read a, Num a) => Parser (Expr a)
parseNumber = Expr . read <$> some (parseIf isDigit)

parseToken :: [Char] -> Parser [Char]
parseToken = mapM filterChar

parseSpaces :: Parser [Char]
parseSpaces = some (parseIf isSpace)

parseVar :: Parser (Expr a)
parseVar = do
  var <- some (parseIf isAlphaNum)
  if var == "sqrt" then failedParse ("Inappropriate variable " ++ var) else return (Var var)

parseSq :: (Read a, Num a) => Parser (Expr a)
parseSq = do
  _ <- parseToken "sqrt"
  _ <- parseSpaces
  Sq <$> parseExpr

parseOp :: (Read a, Num a) => Char -> (Expr a -> Expr a -> Expr a) -> Parser (Expr a)
parseOp opChar op = do
  _ <- filterChar opChar
  _ <- parseSpaces
  x <- parseExpr
  _ <- parseSpaces
  op x <$> parseExpr

binOpsList :: [(Char, Expr a -> Expr a -> Expr a)]
binOpsList = [('+', (:+))
        , ('-', (:-))
        , ('*', (:*))
        , ('/', (:/))
        , ('^', (:^))
        ]

binOps :: (Read a, Num a) => [Parser (Expr a)]
binOps = map (uncurry parseOp) binOpsList

allOps :: (Read a, Num a) => [Parser (Expr a)]
allOps = parseNumber : parseVar : parseSq : binOps

parseExpr :: (Read a, Num a) => Parser (Expr a)
parseExpr = asum allOps

runExprParser :: (Read a, Num a) => String -> Either ParseError (Expr a)
runExprParser s = do
  (t, x) <- parse parseExpr s
  if null t then return x else Left (ParseError ("Unexpected continuation " ++ s))