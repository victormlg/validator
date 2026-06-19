import System.Environment
import System.IO
import Data.Char

-- Lexer -- 

data JToken = STRING String | INTEGER Integer | FLOAT Double | LBRACE | RBRACE | LBRACKET | RBRACKET | COLON | COMMA | TRUE | FALSE | NULL | ERROR
  deriving (Show)

-- Lex Strings
lexString :: String -> String -> [JToken]
lexString [] _ = [ERROR]
lexString (c:rest) tokens = case c of 
  '"' -> STRING (reverse tokens) : lexJson rest
  _ -> lexString rest (c : tokens)

-- Lex Numbers
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
  _ -> INTEGER (emitInteger tokens) : lexJson (c:rest)

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
    | otherwise -> FLOAT (emitDouble tokens) : lexJson (c:rest)

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
  | otherwise = FLOAT (emitDouble tokens) : lexJson (c:rest)

lexNumber :: String -> String -> [JToken]
lexNumber [] tokens = [INTEGER (emitInteger tokens)]
lexNumber (c:rest) tokens = case c of
  '.' -> lexNumberDot rest (c:tokens)
  'e' -> lexNumberExp rest (c:tokens)
  'E' -> lexNumberExp rest (c:tokens)
  x | isDigit x -> lexNumber rest (c:tokens)
    | otherwise -> INTEGER (emitInteger tokens) : lexJson (c:rest)

-- Lex Identifiers
emitIdentifier :: String -> JToken
emitIdentifier identifier = let 
  reversed = reverse identifier 
  in case reversed of
  "true" -> TRUE
  "false" -> FALSE
  "null" -> NULL
  _ -> ERROR

isTokenDelimiter :: Char -> Bool
isTokenDelimiter c = case c of
  ' ' -> True
  '\n' -> True
  '\t' -> True
  '}' -> True
  ']' -> True
  ':' -> True
  ',' -> True
  _ -> False

lexIdentifier :: String -> String -> [JToken]
lexIdentifier [] _ = [ERROR]
lexIdentifier (c:rest) identifier 
  | isTokenDelimiter c = emitIdentifier identifier : lexJson (c:rest)
  | otherwise = lexIdentifier rest (c:identifier)

-- Lex Json
lexJson :: String -> [JToken]
lexJson [] = []
lexJson (c:rest) = case c of 
  ' ' -> lexJson rest
  '\t' -> lexJson rest
  '\n' -> lexJson rest
  '{' -> LBRACE : lexJson rest
  '}' -> RBRACE : lexJson rest
  '[' -> LBRACKET : lexJson rest
  ']' -> RBRACKET : lexJson rest
  ':' -> COLON : lexJson rest
  ',' -> COMMA : lexJson rest
  '"' -> lexString rest []
  '0' -> lexNumberZero (c:rest) []
  '-' -> lexNumberMinus (c:rest) []
  x | isDigit x -> lexNumber (c:rest) []
    | x == 't' || x == 'f' || x == 'n' -> lexIdentifier (c:rest) []
    | otherwise -> ERROR : lexJson rest

-- Parser --
data JNode = JNode { key :: String, value :: Json}
  deriving (Show)
data Json = JInt Integer | JFlt Double | JString String | JBool Bool | JArray [Json] | JDict [JNode] | JNull | JError 
  deriving (Show)



parseValue :: [JToken] -> (Json, [JToken])
parseValue [] = (JError, [])
parseValue (token:rest) = case token of
  STRING s -> (JString s, rest)
  INTEGER i -> (JInt i, rest)
  FLOAT f -> (JFlt f, rest)
  TRUE -> (JBool True, rest)
  FALSE -> (JBool False, rest)
  NULL -> (JNull, rest)
  LBRACKET -> parseArray rest []
  LBRACE -> parseObject rest []
  _ -> (JError, rest)


parseArray :: [JToken] -> [Json] -> (Json, [JToken])
parseArray [] _ = (JError, [])
parseArray (token:rest) acc = case token of
  RBRACKET -> (JArray (reverse acc), rest)
  COMMA -> parseArray rest acc
  _ ->
    let (val, rest') = parseValue (token:rest)
    in parseArray rest' (val : acc)


parseObject :: [JToken] -> [JNode] -> (Json, [JToken])
parseObject [] _ = (JError, [])
parseObject (token:rest) acc = case token of
  RBRACE -> (JDict (reverse acc), rest)
  COMMA -> parseObject rest acc
  STRING key -> case rest of
    (COLON:rest') ->
      let (val, rest'') = parseValue rest'
      in parseObject rest'' (JNode key val : acc)
    _ -> (JError, rest)
  _ -> (JError, rest)
    

parseJson :: [JToken] -> Json
parseJson tokens = fst (parseValue tokens)

-- Main --

main :: IO ()
main = do
  args <- getArgs

  case args of
    [schema_file, data_file] -> do
      putStrLn "Valid"
      withFile data_file ReadMode (\handle -> do
        contents <- hGetContents handle
        let tokens = lexJson contents
            json = parseJson tokens
        print json)
    _ -> do
      putStrLn "Expects exactly two arguments"
  return ()

