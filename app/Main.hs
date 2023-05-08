module Main where

import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Text.Megaparsec.Debug
import Control.Monad
import GHC.IO.Handle
import GHC.IO.Handle.FD (stdout)

data Grammar = Var String | Expression String Grammar | Application Grammar Grammar | LambdaIdentity deriving (Show)
  
instance Semigroup Grammar where
  (<>) gram (Var str) = Expression str gram
  (<>) gram (Expression str gram2) = Expression str (gram <> gram2)
  (<>) gram (Application gram1 gram2) = gram1 <> gram2
  (<>) gram LambdaIdentity = LambdaIdentity

instance Monoid Grammar where
  mempty = LambdaIdentity

--data Lexeme = LambdaSym | Identifier | Point

type Bindings = [String] 

type Parser = Parsec Void String

symLambda = "λ"
symPoint = "->"

parseVar :: Parser String 
parseVar = many alphaNumChar >>= (\str -> if str /= "" then return str else fail "incomplete expression")

parseExpression :: Parser Grammar
parseExpression = do
  string symLambda
  many spaceChar
  ident <- parseVar
  many spaceChar
  string symPoint
  many spaceChar
  expr <- parseLambdaCalculus <|> Var <$> parseVar
  return $ Expression ident expr

parseExprVar :: Parser Grammar
parseExprVar = parseExpression <|> Var <$> parseVar

parseApplication :: Parser Grammar
parseApplication = do
  char '('
  expr1 <- parseExprVar
  spaceChar
  many spaceChar
  expr2 <- parseExprVar
  case expr1 of
    Var _ -> fail "the first operand for an application must be a lambda."
    _ -> do
      char ')'
      return $ Application expr1 expr2

parseLambdaCalculus :: Parser Grammar
parseLambdaCalculus = do 
  grammar <- parseExpression <|> parseApplication
  eof -- reject garbage at the end
  return grammar

betaReduce :: Grammar -> Grammar
betaReduce gram = case gram of
                    Var str -> Var str
                    Expression str gram1 -> Expression str $ betaReduce gram1
                    Application (Expression strExp gramExp) (Var expVar) -> case gramExp of
                                                                              Var str -> Var expVar
                                                                              _ -> betaReduce gramExp
                    Application gram1 gram2 -> betaReduce gram1 <> betaReduce gram2



main :: IO ()
main = forever (do
                  putStrLn "Enter the expression: "
                  expr <- getLine
                  let results = parse parseLambdaCalculus "" expr
                  putStrLn "=== lexing expression ==="
                  print results
                  putStrLn "=== beta reducing expression ==="
                  case results of
                    Left _ -> putStrLn "the expression was not reduced since it could not be lexed"
                    Right lexResults -> print $ betaReduce lexResults

               )
