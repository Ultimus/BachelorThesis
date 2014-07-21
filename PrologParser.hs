import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad

data MyType = Atom String | Number Integer | Bool Bool | S String

{-instance Show MyType where show = showVal

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~'"

spaces :: Parser ()
spaces = skipMany1 (space <|> symbol)


parseString :: Parser MyType
parseString = do
      char '"'
      x <- many (noneOf"\"")
      char '"'
      return $ String x

parseNumber :: Parser MyType
parseNumber = liftM (Number . read) $ many1 digit

parseAtom :: Parser MyType
parseAtom = do
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ case atom of
                           "pred_true" -> Bool True
                           "pred_false" -> Bool False
                           _    -> Atom atom



parseExpr :: Parser MyType
parseExpr = parseNumber
    <|> parseString
    <|> parseAtom


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> show val

showVal :: MyType -> String
showVal (String contents) = "'" ++ contents ++"'"
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "pred_True"
showVal (Bool False) = "pred_False"
-}

instance Show MyType where show = showVal


test :: Parser [MyType]
test = do
    string "Hello("
    x <- parseString
    char ','
    y <- parseString
    char ')'
    return [x,y]

showVal (Number contents) = show contents
showVal (S contents) = show contents

readExpr :: String -> String
readExpr input = case parse test "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> show val


parseLine :: Parser [MyType]
parseLine = do
    string "packed_visited_expression("
    ident <- parseNumber
    char ','
    bindVar <- bVar
    string ")."
    return [ident, bindVar]

bVar :: Parser [MyType]
bVar = do
    string "'$bind_var'("
    arg <- argument1
    char ','
    arg1 <- argument1
    char ','
    next <- bVar <|> pVar  -- don't forgett empty List
    return [arg , arg1 , next]

pVar :: Parser [MyType]
pVar = do
    first <- parseNumber
    char ','
    next <- bVar
    char ','
    return [first, next]

argument1 :: Parser MyType
argument1 =
    parseString
    <|> parseNumber
    <|> parseAtom
    <|> parseBool
    <|> parseEmptyList
    <|> avlExpr  --this might not work, because of the type

parseNumber :: Parser MyType
parseNumber = liftM (Number . read) $ many1 digit

parseString :: Parser MyType
parseString = do
    char '\''
    x <- many letter
    --liftM (String . read) $ many1 letter
    char '\''
    return $ S x

parseBool :: Parser MyType
parseBool = do
    first <- lower
    rest <- many (letter <|> digit)
    let atom = first:rest
    return $ case atom of
        "pred_true" -> Bool True
        "pred_false" -> Bool False

parseAtom :: Parser MyType
parseAtom = do
    x <- many lower
    return  $ Atom x


avlExpr :: Parser MyType
avlExpr = do
    x <- try avl1 <|> avl2
    char ')'
    return x

avl1 :: Parser [MyType]
avl1 = do
       string "$avl_bv("
       number <- parseNumber
       char ','
       arg <- parseString
       char ')'
       return [number, arg]

avl2 :: Parser MyType
avl2 = do
     string "$avl_expr("
     x <- parseList
     char ')'
     return x

parseList :: Parser [MyType]
parseList = do
     x <- try sArgument1 <|> sArgument2 <|> parseEmptyList
     return x


parseEmptyList :: Parser [MyType]
parseEmptyList = string "[]"
    return []

sArgument1 :: Parser [MyType] --just the numbers are interessting
sArgument1 = do
    char '\''
    q <-parseSpecialString
    x <- parseNumber
    string "\',\'"
    p <- parseSpecialString
    y <- parseNumber
    char '\''
    return [x,y]

sArgument2 :: Parser [MyType]
sArgument2 = do
    x <- sArgument1
    char ','
    y <- parseList
    return [x,y]  --need to check this


parseSpecialString :: Parser MyType
parseSpecialString = do
    x <- many (noneOf"123456789")
    return $ S x


{-
parseInput :: String -> Either ParseError [[String]]
parseInput input = do
    result <- many line
    return result
-}





