module Logic (makeTruth) where

import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

data Expr
  = EBool Bool
  | EOr Expr Expr
  | EAnd Expr Expr
  | ENot Expr
  | EVar Var
  deriving (Show)

type Var = String

type Result = [([Bool], Bool)]

type Lookup = [(Var, Bool)]

-- Parser
-- not the best use case for LL(1) parsers
type Parser = Parsec Void String

expr :: Parser Expr
expr = choice [EBool <$> tf, EVar <$> var, notp, try orp, andp]

tf :: Parser Bool
tf = label "boolean" $ True <$ string' "true" <|> False <$ string' "false"

orp :: Parser Expr
orp =
  between
    (char '(')
    (char ')')
    ( do
        e1 <- expr
        string "||"
        EOr e1 <$> expr
    )

andp :: Parser Expr
andp =
  between
    (char '(')
    (char ')')
    ( do
        e1 <- expr
        string "&&"
        EAnd e1 <$> expr
    )

notp :: Parser Expr
notp = ENot <$> (char '!' >> between (char '(') (char ')') expr)

var :: Parser Var
var = label "variable" $ some alphaNumChar

readExpr :: String -> Either (ParseErrorBundle String Void) Expr
readExpr = parse expr "input"

-- making truth table
truth :: [Var] -> Expr -> Result
truth vs ex = map (\lk -> (map snd lk, eval ex lk)) $ enumerate vs

getVars :: Expr -> [Var]
getVars ex = nub $ getVars' ex
  where
    getVars' (EVar v) = [v]
    getVars' (EOr a b) = getVars a ++ getVars b
    getVars' (EAnd a b) = getVars a ++ getVars b
    getVars' (ENot a) = getVars a
    getVars' _ = []

enumerate :: [Var] -> [Lookup]
enumerate = foldr (\v -> ((:) <$> [(v, False), (v, True)] <*>)) [[]]

eval :: Expr -> Lookup -> Bool
eval (EBool b) _ = b
eval (EOr a b) lk = eval a lk || eval b lk
eval (EAnd a b) lk = eval a lk && eval b lk
eval (ENot a) lk = not $ eval a lk
eval (EVar v) lk = fromJust $ lookup v lk

printResult :: [Var] -> Result -> IO ()
printResult vs res = print vs >> mapM_ print res

-- | Create truth table with boolean expression
-- /many/ parenthesis are needed
--
-- >>> makeTruth "(a||(!(a)&&b))"
-- ["a","b"]
-- ([False, False], False)
-- ([False, True], True)
-- ([True, False], True)
-- ([True, True], True)
makeTruth :: String -> IO ()
makeTruth s = case readExpr s of
  Left err -> putStrLn $ errorBundlePretty err
  Right e -> printResult vars $ truth vars e
    where
      expr = readExpr s
      vars = getVars e
