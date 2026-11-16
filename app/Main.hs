{- HLINT ignore "Use lambda-case" -}
module Main where
import Data.Complex ( Complex(..) )
import Text.Yoda
    ( chainl1,
      chainr1,
      char,
      choice,
      parse,
      satisfy,
      string,
      (<|>),
      some,
      many,
      Parser )
import Data.Char (isDigit)

main :: IO ()
main = do
    putStrLn "Enter expression:"
    l <- getLine
    let lExpr = sTo parseExpr l
    let lLang = toLang lExpr
    putStrLn $ l ++ " gives the expression:"
    print lExpr
    putStrLn "which evaluates to:"
    print lLang
    putStrLn "which is: "
    print $ emlSolve lLang

data Lang = One | EML Lang Lang deriving Show

posInf :: Complex Double -> Bool
posInf (a :+ b) = isInfinite a && a > 0 && b == 0

exp' :: Complex Double -> Complex Double
exp' (a :+ b)
    | isInfinite a && a < 0 && not (isInfinite b || isNaN b) = 0 -- exp(-inf) = 0
    | posInf (a :+ b) = (1/0) :+ 0                               -- exp(inf)  = inf
    | otherwise = exp (a :+ b)

log' :: Complex Double -> Complex Double
log' (a :+ b)
    | a == 0 && b == 0 = (- (1/0)) :+ 0 -- log(0)   = -inf
    | posInf (a :+ b) = (1/0) :+ 0      -- log(inf) = inf
    | otherwise = log (a :+ b)

emlSolve :: Lang -> Complex Double
emlSolve One = 1
emlSolve (EML a b) = exp' (emlSolve a) - log' (emlSolve b)

data Expr =
      Lit (Complex Int) -- don't want to construct every real number
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr deriving Show

digit :: Parser Char
digit = satisfy isDigit

int :: Parser Int
int = read <$> some digit

parseReal :: Parser Int
parseReal = do
  sign  <- string "-" <|> pure ""
  whole <- some digit
  return (read (sign ++ whole))

parseComplex :: Parser (Complex Int)
parseComplex = do
  r <- parseReal
  i <- (string ":+" *> parseReal) <|> pure 0
  return (r :+ i)

lexeme :: Parser a -> Parser a
lexeme p = p <* many (char ' ')

term :: Parser Expr
term = lexeme (char '(') *> parseExpr <* lexeme (char ')')
   <|> Lit <$> lexeme parseComplex

op :: Char -> Parser Char
op = lexeme . char

parseExpr :: Parser Expr
parseExpr = ((term `chainr1` (Pow <$ op '^'))
                   `chainl1` (Mul <$ op '*' <|> Div <$ op '/'))
            `chainl1` (Add <$ op '+' <|> Sub <$ op '-')

lExp, lLn, lNeg, lInv :: Lang -> Lang
lSub, lAdd, lMul, lDiv, lPow :: Lang -> Lang -> Lang
lExp   x = EML x One
lLn    x = EML One (EML (EML One x) One)
lSub x y = EML (lLn x) (lExp y)
lNeg   x = lLn One `lSub` x
lAdd x y = x `lSub` lNeg y
lMul x y = lExp $ lLn x `lAdd` lLn y
lInv   x = lExp $ lNeg (lLn x)
lDiv x y = x `lMul` lInv y
lPow x y = lExp $ y `lMul` lLn x

-- i = exp(1/2 * ln(-1))
lI :: Lang
lI = lExp $ lDiv One (One `lAdd` One) `lMul` lLn (lNeg One)

lSuc, lCus :: Lang -> Int -> Lang
lSuc lang 0 = lang
lSuc lang n = lSuc (lang `lAdd` One) (n-1)

lCus lang 0 = lang
lCus lang n = lCus (lang `lSub` One) (n+1)

intToLang :: Int -> Lang
intToLang x
    | x == 0 = lLn One
    | x > 0 = lSuc One (x-1)
    | otherwise = lCus (lNeg One) (x+1)

toLang :: Expr -> Lang
toLang (Lit (a :+ b))
    -- need case distinction otherwise multiplying by zero i.e. log(0) = -inf
    | b == 0    = intToLang a
    | a == 0    = intToLang b `lMul` lI
    | otherwise = intToLang a `lAdd` (intToLang b `lMul` lI)
toLang (Add expr1 expr2) = lAdd (toLang expr1) (toLang expr2)
toLang (Sub expr1 expr2) = lSub (toLang expr1) (toLang expr2)
toLang (Mul expr1 expr2) = lMul (toLang expr1) (toLang expr2)
toLang (Div expr1 expr2) = lDiv (toLang expr1) (toLang expr2)
toLang (Pow expr1 expr2) = lPow (toLang expr1) (toLang expr2)

sTo :: Parser c -> String -> c
sTo p = fst . head . parse p

-- parsing from eml's and ones to Lang
between' :: Parser a -> (Char, Char) -> Parser a
between' px (left, right) = char left *> px <* char right

brackets :: Parser a -> Parser a
brackets xs = choice $ between' xs <$> [('(' , ')'), ('[' , ']'), ('{' , '}')]

parseEml :: Parser Lang
parseEml = parseOne <|> (string "eml" *> brackets parseComma)
    where
        parseOne :: Parser Lang
        parseOne = One <$ char '1'

        parseComma :: Parser Lang
        parseComma = EML <$> parseEml <* lexeme (char ',') <*> parseEml
