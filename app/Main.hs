import System.Environment
import System.IO
import Data.Char

-- Lexer -- 

data JToken = STRING String | INTEGER Integer | FLOAT Double | LPAR | RPAR | LBRACKET | RBRACKET | COLON | COMMA | TRUE | FALSE | NULL | ERROR
  deriving (Show)

-- Parse String
lexString :: String -> String -> [JToken]
lexString [] _ = [ERROR]
lexString (c:rest) tokens = case c of 
  '"' -> STRING (reverse tokens) : lexNext rest
  _ -> lexString rest (c : tokens)

-- Parse Numbers
emitInteger :: String -> Integer
emitInteger tokens = read (reverse tokens) :: Integer

emitDouble :: String -> Double
emitDouble tokens = read (reverse tokens) :: Double

lexNumberMinus :: String -> String -> [JToken]
lexNumberMinus [] _ = [ERROR]
lexNumberMinus (c:rest) tokens = case c of
  '0' -> lexNumberZero rest (c:tokens)
  x | isDigit x -> lexNumber rest (c:tokens)
    | otherwise -> [ERROR]

lexNumberZero :: String -> String -> [JToken]
lexNumberZero [] tokens = [INTEGER (emitInteger tokens)]
lexNumberZero (c:rest) tokens = case c of
  'E' -> lexNumberExp rest (c:tokens)
  'e' -> lexNumberExp rest (c:tokens)
  '.' -> lexNumberDot rest (c:tokens)
  _ -> [INTEGER (emitInteger tokens)]

lexNumberDot :: String -> String -> [JToken]
lexNumberDot [] _ = [ERROR]
lexNumberDot (c:rest) tokens 
  | isDigit c = lexNumberDecimal rest (c:tokens)
  | otherwise = [ERROR]

lexNumberDecimal :: String -> String -> [JToken]
lexNumberDecimal [] tokens = [FLOAT (emitDouble tokens)]
lexNumberDecimal (c:rest) tokens = case c of
  'e' -> lexNumberExp rest (c:tokens)
  'E' -> lexNumberExp rest (c:tokens)
  x | isDigit x -> lexNumberDecimal rest (c:tokens)
    | otherwise -> [FLOAT (emitDouble tokens)]

lexNumberExp :: String -> String -> [JToken]
lexNumberExp [] _ = [ERROR]
lexNumberExp (c:rest) tokens = case c of 
  '+' -> lexNumberExpSign rest (c:tokens)
  '-' -> lexNumberExpSign rest (c:tokens)
  x | isDigit x -> lexNumberEnd rest (c:tokens)
    | otherwise -> [ERROR]

lexNumberExpSign :: String -> String -> [JToken]
lexNumberExpSign [] _ = [ERROR]
lexNumberExpSign (c:rest) tokens 
  | isDigit c = lexNumberEnd rest (c:tokens)
  | otherwise = [ERROR]

lexNumberEnd :: String -> String -> [JToken]
lexNumberEnd [] tokens = [FLOAT (emitDouble tokens)]
lexNumberEnd (c:rest) tokens
  | isDigit c = lexNumberEnd rest (c:tokens)
  | otherwise = [FLOAT (emitDouble tokens)]

lexNumber :: String -> String -> [JToken]
lexNumber [] tokens = [INTEGER (emitInteger tokens)]
lexNumber (c:rest) tokens = case c of
  '.' -> lexNumberDot rest (c:tokens)
  'e' -> lexNumberExp rest (c:tokens)
  'E' -> lexNumberExp rest (c:tokens)
  x | isDigit x -> lexNumber rest (c:tokens)
    | otherwise -> INTEGER (emitInteger tokens) : lexNext rest

lexNext :: String -> [JToken]
lexNext [] = []
lexNext (c:rest) = case c of 
  ' ' -> lexNext rest
  '\t' -> lexNext rest
  '\n' -> lexNext rest
  '{' -> LPAR : lexNext rest
  '}' -> RPAR : lexNext rest
  '[' -> RBRACKET : lexNext rest
  ']' -> RBRACKET : lexNext rest
  ':' -> COLON : lexNext rest
  ',' -> COMMA : lexNext rest
  '"' -> lexString rest []
  '0' -> lexNumberZero (c:rest) []
  '-' -> lexNumberMinus (c:rest) []
  x | isDigit x -> lexNumber (c:rest) []
    | otherwise -> ERROR : lexNext rest
    -- TODO: null, true and false

-- Parser --
-- TODO

-- Json Representation
data JNode = JNode { key :: String, value :: Json}
data Json = JInt Int | JFlt Float | JString String | JBool Bool | JArray [Json] | JDict [JNode] | JNull


main :: IO ()
main = do
  args <- getArgs

  case args of
    [schema_file, data_file] -> do
      putStrLn "Valid"
      withFile data_file ReadMode (\handle -> do
        contents <- hGetContents handle
        let tokens = lexNext contents in print tokens)
    _ -> do
      putStrLn "Expects exactly two arguments"
  return ()

