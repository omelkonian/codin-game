import System.IO
import Control.Monad
import Control.Applicative hiding ((<|>), many)

import Data.List
import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String


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
    <|> (try (EB <$> blockP))
    <|> (EP <$> primitiveP)

------------------- Block ------------------------
blockP :: Parser Block
blockP =
  (lexeme $ char '(') *>
    (BL <$> blockElementsP)
  <* (lexeme $ char ')')

blockElementsP :: Parser [Element]
blockElementsP =
  lexeme $ (elementP `sepBy` (lexeme $ char ';'))

------------------ Key Value ---------------------
keyValueP :: Parser KeyValue
keyValueP = lexeme $
  (try keyValueBlockP) <|> keyValuePrimP

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
main = do
  x <- parseFromFile elementP "cgx.txt"
  print x
  case x of
    Right element -> do
      pp 0 element
      return ()

--------------- Pretty Printing ------------------
pp indent (EP p) = pp_p indent p
pp indent (EK kv) = pp_kv indent kv
pp indent (EB b) = pp_b indent b

pp_els i [l] =
  ln >> pp i l
pp_els i (l:ls) =
  ln >> pp i l >> putStr ";" >> pp_els i ls

pp_b indent (BL els) = do
  put indent "("
  pp_els (indent + 1) els
  ln
  put indent ")"

pp_kv indent (KVP s p) = do
  put indent $ "'" ++ s ++ "'="
  pp_p 0 p
pp_kv indent (KVB s b) = do
  put indent $ "'" ++ s ++ "'="
  ln
  pp_b indent b

pp_p indent (S s) = put indent s
pp_p indent (B b) = put indent $ if b then "true" else "false"
pp_p indent (N n) = put indent $ show n


ln = putStrLn ""

put indent s = do
  replicateM indent $ do
    putStr "\t"
    return ()
  putStr s

-------------------- Main ------------------------
main2 :: IO ()
main2 = forever $ do
    hSetBuffering stdin LineBuffering
    cgxline <- getLine
    parseTest elementP cgxline
    return ()
