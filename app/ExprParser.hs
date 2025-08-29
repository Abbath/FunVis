{-# LANGUAGE OverloadedStrings #-}

module ExprParser where

import Control.Monad.Combinators.Expr
import Data.Text (
  Text,
  pack,
 )
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

data Cond = Equal | Less deriving (Show, Eq)

data FunType = Sin | Abs | Sqrt | Log | Inv deriving (Show, Eq)

-- AST
data Expr
  = Param Text
  | Num Double
  | Add Expr Expr
  | Mul Expr Expr
  | Pow Double Expr
  | Fun FunType Expr
  | If Cond Expr Expr Expr Expr
  deriving (Show)

-- Parser type
type Parser = Parsec Void String

-- Lexer helpers
sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Number parser
pNum :: Parser Expr
pNum = Num <$> (try (lexeme L.float) <|> lexeme L.decimal)

-- Variable parser
pVar :: Parser Expr
pVar = Param . pack <$> lexeme (symbol "x" <|> symbol "y")

-- Function parser (like sin(expr), log(expr), abs(expr))
pFun :: Parser Expr
pFun = do
  fname <- lexeme (some letterChar)
  arg <- parens pExpr
  return
    ( Fun
        ( case fname of
            "sin" -> Sin
            "abs" -> Abs
            "sqrt" -> Sqrt
            "log" -> Log
            "inv" -> Inv
            _ -> error "Wrong function name"
        )
        arg
    )

pIfArgs :: Parser (Cond, Expr, Expr, Expr, Expr)
pIfArgs = do
  arg1 <- pExpr
  c1 <- lexeme (symbol "==" <|> symbol "<")
  arg2 <- pExpr
  _ <- symbol ","
  arg3 <- pExpr
  _ <- symbol ","
  arg4 <- pExpr
  return (if c1 == "==" then Equal else Less, arg1, arg2, arg3, arg4)

pIf :: Parser Expr
pIf = do
  _ <- symbol "if"
  (cond, a, b, c, d) <- parens pIfArgs
  return $ If cond a b c d

-- Atomic expression
pAtom :: Parser Expr
pAtom =
  choice
    [ try pIf
    , try pFun -- must come before pVar
    , try pNum
    , try pVar
    , parens pExpr
    ]

-- Operator precedence table
table :: [[Operator Parser Expr]]
table =
  [
    [ Postfix
        ( do
            _ <- symbol "^"
            n <- pNum
            case n of
              Num 2.0 -> return $ Pow 2.0
              Num 3.0 -> return $ Pow 3.0
              _ -> fail "Only ^2  and ^3 are supported"
        )
    ]
  , [Prefix (Mul (Num (-1)) <$ symbol "-")]
  , [InfixL (Mul <$ symbol "*")]
  , [InfixL (Add <$ symbol "+")]
  ]

-- Expression parser
pExpr :: Parser Expr
pExpr = makeExprParser pAtom table

-- Top-level runner
parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr = parse (sc *> pExpr <* eof) ""
