module Parser where

import Data.Char (isDigit, isSpace, isAlpha)


newtype Parser a
  = Parser { runParser :: String -> Maybe (a, String) }


instance Functor Parser where
  fmap f (Parser p) =
    Parser (\inp ->
              case p inp of
                Nothing -> Nothing
                Just (v,rem) -> Just (f v, rem))


instance Applicative Parser where
  pure x = Parser (\inp -> Just (x, inp))
  (Parser f) <*> (Parser x) =
    Parser (\inp ->
              case f inp of
                Nothing -> Nothing
                Just (f,rem) ->
                  case x rem of
                    Nothing -> Nothing
                    Just (x, rem') ->
                      Just (f x, rem'))


instance Monad Parser where
  return = pure
  (Parser m) >>= f =
    Parser (\inp ->
              case m inp of
                Nothing -> Nothing
                Just (x,rem) -> runParser (f x) rem)


eval :: Parser a -> String -> Maybe a
eval p = (fst <$>) . runParser p


failParse :: Parser a
failParse = Parser (\_ -> Nothing)


parseEither :: Parser a -> Parser a -> Parser a
parseEither pa pb =
  Parser (\inp ->
            case runParser pa inp of
              Nothing -> runParser pb inp
              success -> success)
  

parseMany :: Parser a -> Parser [a]
parseMany p =
  Parser (\inp ->
            case runParser p inp of
              Nothing -> Just ([], inp)
              Just (x, rem) ->
                case runParser (parseMany p) rem of
                  Nothing -> Just ([x], rem)
                  Just (xs, rem') -> Just (x:xs, rem'))


parseChar :: (Char -> Bool) -> Parser Char
parseChar f =
  Parser (\inp ->
            case inp of
              c:rem | f c -> Just (c, rem)
              _ -> Nothing)
  

parseDigits :: Parser String
parseDigits = parseMany parseDigit


parseAlphas :: Parser String
parseAlphas = parseMany parseAlpha


parseWhiteSpaces :: Parser ()
parseWhiteSpaces = do
  _ <- parseMany parseWhiteSpace
  return ()


parseNumber :: Parser Int
parseNumber = do
  parseWhiteSpaces
  ds <- parseDigits
  if null ds
    then failParse
    else do
       parseWhiteSpaces
       return (read ds)
         


parseAlpha :: Parser Char
parseAlpha = parseChar isAlpha

  
parseDigit :: Parser Char
parseDigit = parseChar isDigit
  

parseWhiteSpace :: Parser Char
parseWhiteSpace = parseChar isSpace
