module Logic (makeTruth) where

import Control.Applicative ((<**>))
import Data.List (nub)
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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

--
-- Parser
--

-- E -> T E'
-- E' -> "||" E | e
-- T -> P T'
-- T' -> "&&" T | e
-- P -> '(' E ')' | '!' P | "true" | "false" | Variable
type Parser = Parsec Void String

skipSpace :: Parser ()
skipSpace = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme skipSpace

parseE :: Parser Expr
parseE = parseT <**> parseE'

parseE' :: Parser (Expr -> Expr)
parseE' = flip EOr <$> (string "||" >> skipSpace >> parseE) <|> return id

parseT :: Parser Expr
parseT = parseP <**> parseT'

parseT' :: Parser (Expr -> Expr)
parseT' = flip EAnd <$> (string "&&" >> skipSpace >> parseT) <|> return id

parseP :: Parser Expr
parseP = lexeme $ choice [between (lexeme $ char '(') (char ')') parseE, parseNot, EBool <$> tf, EVar <$> var]

parseNot :: Parser Expr
parseNot = ENot <$> (char '!' >> skipSpace >> parseP)

tf :: Parser Bool
tf = label "boolean" $ True <$ string' "true" <|> False <$ string' "false"

var :: Parser Var
var = label "variable" $ some alphaNumChar

-- the main parsing function
parseExpr :: String -> Either (ParseErrorBundle String Void) Expr
parseExpr = parse (between skipSpace eof parseE) "input"

--
-- Making truth table
--

truth :: [Var] -> Expr -> Result
truth vs ex = map (\lk -> (map snd lk, eval ex lk)) $ enumerate vs

-- Get a list of variables used in the expression
getVars :: Expr -> [Var]
getVars ex = nub $ getVars' ex
  where
    getVars' (EVar v) = [v]
    getVars' (EOr a b) = getVars a ++ getVars b
    getVars' (EAnd a b) = getVars a ++ getVars b
    getVars' (ENot a) = getVars a
    getVars' _ = []

-- Generate all possible combinations of true/false
enumerate :: [Var] -> [Lookup]
enumerate = foldr (\v -> ((:) <$> [(v, False), (v, True)] <*>)) [[]]

-- Evaluate a expression with given assignment of variables
eval :: Expr -> Lookup -> Bool
eval (EBool b) _ = b
eval (EOr a b) lk = eval a lk || eval b lk
eval (EAnd a b) lk = eval a lk && eval b lk
eval (ENot a) lk = not $ eval a lk
eval (EVar v) lk = fromJust $ lookup v lk

-- Pretty(?) print the result
printResult :: [Var] -> Result -> IO ()
printResult vs res = print vs >> mapM_ print res

-- | Create truth table with boolean expression
-- Operator precedence: NOT > AND > OR
--
-- >>> makeTruth "(a || b) && ! c"
-- ["a", "b", "c"]
-- ([False, False, False], False)
-- ([False, False, True], False)
-- ([False, True, False], True)
-- ([False, True, True], False)
-- ([True, False, False], True)
-- ([True, False, True], False)
-- ([True, True, False], True)
-- ([True, True, True], False)
-- >>> makeTruth "True"
-- []
-- ([], True)
makeTruth :: String -> IO ()
makeTruth s = case parseExpr s of
  Left err -> putStrLn $ errorBundlePretty err
  Right e -> printResult vars $ truth vars e
    where
      vars = getVars e
