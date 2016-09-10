import System.IO
import Control.Monad
import Control.Applicative hiding ((<|>), many)

import Data.List
import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import qualified Text.PrettyPrint as PP


-- Data types
data Primitive = B Bool
               | S String
               | N String
               deriving(Show, Eq)

data KeyValue = KVP String Primitive
              | KVB String Block
              deriving(Show, Eq)

data Block = BL [Element]
           deriving(Show, Eq)

data Element = EK KeyValue
             | EP Primitive
             | EB Block
             deriving(Show, Eq)

------------------ Element -----------------------
elementP :: Parser Element
elementP = lexeme $
    try (EK <$> keyValueP)
    <|> (EP <$> primitiveP)
    <|> (EB <$> blockP)

------------------- Block ------------------------
blockP :: Parser Block
blockP = lexeme $
  char '(' *> (BL <$> blockElementsP) <* char ')'

blockElementsP :: Parser [Element]
blockElementsP =
  lexeme $ (elementP `sepBy` (lexeme $ char ';'))

------------------ Key Value ---------------------
keyValueP :: Parser KeyValue
keyValueP = lexeme $
  keyValuePrimP <|> keyValueBlockP

keyValuePrimP :: Parser KeyValue
keyValuePrimP = do
    s <- stringP
    char '='
    p <- primitiveP
    return $ KVP s p

keyValueBlockP :: Parser KeyValue
keyValueBlockP = do
    s <- stringP
    char '='
    b <- blockP
    return $ KVB s b

------------------ Primitive ---------------------
primitiveP :: Parser Primitive
primitiveP = lexeme $
    (S <$> stringP)  <|>
    (B <$> booleanP) <|>
    (N <$> numberP)

stringP :: Parser String
stringP =
    char '\'' *> (many $ noneOf ['\'']) <* char '\''

numberP :: Parser String
numberP = lexeme (many1 digit)

booleanP :: Parser Bool
booleanP = booleanTrue <|> booleanFalse

booleanTrue :: Parser Bool
booleanTrue = string "true" *> pure True

booleanFalse :: Parser Bool
booleanFalse = string "false" *> pure False

------------------ Whitespace --------------------
lexeme p = ws *> p <* ws

ws :: Parser String
ws = many $ oneOf " \t\n"

-- Main
main :: IO ()
main = forever $ do
    hSetBuffering stdin LineBuffering
    cgxline <- getLine
    parseTest elementP cgxline
    return ()
