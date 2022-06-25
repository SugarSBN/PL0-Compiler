module ParserCombinator where

import Control.Applicative
import Data.Char

newtype Parser a = Parser {
  runParser :: String -> Maybe (String, a)
}

instance Functor Parser where
  fmap f (Parser x) = Parser $ \s ->
    case (x s) of
      Nothing       -> Nothing
      Just (s', x') -> Just (s', f x')

instance Applicative Parser where
  pure x = Parser $ \s -> Just (s, x)
  Parser f <*> Parser p = Parser $ \s -> 
    case f s of
      Nothing          -> Nothing
      Just (s', f') -> case p s' of
        Nothing        -> Nothing
        Just (s'', x') -> Just (s'', f' x') 

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  Parser p1 <|> Parser p2 = Parser $ \s ->
    p1 s <|> p2 s

predicate :: (Char -> Bool) -> Parser String
predicate f = Parser $ \s -> 
  let (token, rest) = (span f s) in
    case token of
      "" -> Nothing
      _  -> Just (rest, token)

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

  
