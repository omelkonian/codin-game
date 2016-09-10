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
data Primitive = Boolean Bool
               | Str String
               | Number String
               deriving(Show, Eq)

-- type Block = [Element]

-- data KeyValue = KeyValueBlock String Block
--               | KeyValuePrimitive String Primitive

-- data Element = Block
--              | KeyValue
--              | Primitive

-- elementP :: Parser Element
-- elementP = blockP <|> keyValueP <|> primitiveP

-- blockP :: Parser Block
-- blockP = do
--     char '('
--     el <- many1 elementP
--     char ')'
--     return  el

-- keyValueP :: Parser KeyValue
-- keyValueP = keyValueBlockP <|> keyValuePrimP

-- keyValueBlockP :: Parser KeyValue
-- keyValueBlockP = do
--     s <- stringP
--     char '='
--     b <- blockP
--     return $ KeyValueBlock s b

-- keyValuePrimP :: Parser KeyValue
-- keyValuePrimP = do
--     s <- stringP
--     char '='
--     p <- primitiveP
--     return $ KeyValuePrimitive s p

-- Parsers
primitiveP :: Parser Primitive
primitiveP =
    (Str <$> stringP) <|>
    (Boolean <$> booleanP) <|>
    (Number <$> numberP)

stringP :: Parser String
stringP =
    char '\'' *> (many (noneOf ['\''])) <* char '\''

numberP :: Parser String
numberP = many1 digit

booleanP :: Parser Bool
booleanP = booleanTrue <|> booleanFalse

booleanTrue :: Parser Bool
booleanTrue = string "true" *> pure True

booleanFalse :: Parser Bool
booleanFalse = string "false" *> pure False

-- Main
main :: IO ()
main = forever $ do
    cgxline <- getLine
    parseTest primitiveP cgxline
    return ()
