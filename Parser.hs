module Parser (HtmlElement (..), parseHtml, Parser (..), htmlValue) where

import Control.Applicative
import Data.Char

data HtmlElement = HtmlElement String [(String, String)] String [HtmlElement] deriving (Show)

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
  return = pure
  (Parser p1) >>= f =
    Parser $ \input -> do
      (input', a) <- p1 input
      runParser (f a) input'

spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Just (rest, token)

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"'

htmlTextLiteral :: Parser String
htmlTextLiteral = spanP (/= '<')

whiteSpace :: Parser String
whiteSpace = spanP isSpace

charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

stringP :: String -> Parser String
stringP = traverse charP

manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end = (end *> pure []) <|> ((:) <$> p <*> manyTill p end)

attributesParser :: String -> Parser [(String, String)]
attributesParser ender = manyTill (whiteSpace *> attributeParser) (whiteSpace *> stringP ender)
  where
    attributeParser = (,) <$> (spanP (/= '=') <* charP '=') <*> stringLiteral

nameParser :: Parser String
nameParser = spanP isAlpha

elementsParser :: Parser [HtmlElement]
elementsParser = many (whiteSpace *> htmlValue)

dElemWithTxtParser :: Parser HtmlElement
dElemWithTxtParser = HtmlElement <$> (charP '<' *> nameParser <* whiteSpace) <*> (attributesParser ">" <|> (pure [] <* stringP ">")) <*> htmlTextLiteral <*> pure [] <* stringP "</" <* nameParser <* whiteSpace <* charP '>'

dElemWithChildrenParser :: Parser HtmlElement
dElemWithChildrenParser = HtmlElement <$> (charP '<' *> nameParser <* whiteSpace) <*> (attributesParser ">" <|> (pure [] <* stringP ">")) <*> pure "" <*> elementsParser <* stringP "</" <* nameParser <* whiteSpace <* charP '>'

sElemParser :: Parser HtmlElement
sElemParser = HtmlElement <$> (charP '<' *> nameParser <* whiteSpace) <*> (attributesParser "/>" <|> (pure [] <* stringP "/>")) <*> pure "" <*> pure []

htmlValue :: Parser HtmlElement
htmlValue = dElemWithTxtParser <|> dElemWithChildrenParser <|> sElemParser

parseHtml :: String -> Maybe HtmlElement
parseHtml input = snd <$> runParser htmlValue input
