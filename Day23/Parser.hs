module Parser where

import Data.Char (isDigit, isSpace, isAlpha)
import Data.List (isPrefixOf)


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
failParse = Parser (const Nothing)


parseList :: Char -> Char -> Char -> Parser a -> Parser [a]
parseList lBrac rBrac sep p =
  parseBetween (parseChar lBrac) (parseChar rBrac) $ p `parseSepBy` (parseChar sep)


parseBetween :: Parser l -> Parser r -> Parser a -> Parser a
parseBetween pL pR pA = pL *> pA <* pR


parseSepBy :: Parser a -> Parser sep -> Parser [a]
parseSepBy pa ps = parseEither (parseSepBy1 pa ps) (pure [])


parseSepBy1 :: Parser a -> Parser sep -> Parser [a]
parseSepBy1 pa ps = (:) <$> pa <*> parseEither (ps *> parseSepBy1 pa ps) (pure [])


parseOneOf :: [Parser a] -> Parser a
parseOneOf (a:b:rest) = parseOneOf (parseEither a b : rest)
parseOneOf [p] = p
parseOneOf _ = failParse


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


parseAny :: Parser Char
parseAny =
  Parser (\inp ->
            case inp of
              c:rem -> Just (c, rem)
              _ -> Nothing)


parsePred :: (Char -> Bool) -> Parser Char
parsePred f =
  Parser (\inp ->
            case inp of
              c:rem | f c -> Just (c, rem)
              _ -> Nothing)


parseString :: String -> Parser ()
parseString s =
  Parser (\inp ->
            if s `isPrefixOf` inp
            then Just ((), drop (length s) inp)
            else Nothing)


parseChar :: Char -> Parser ()
parseChar c = parsePred (== c) *> pure ()


parseDigits :: Parser String
parseDigits = parseMany parseDigit


parseAlphas :: Parser String
parseAlphas = parseMany parseAlpha


parseN :: Parser a -> Int -> Parser [a]
parseN p 0 = pure []
parseN p n = do
  a <- p
  as <- parseN p (n-1)
  return $ a:as


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


parseNegNumber :: Parser Int
parseNegNumber = do
  parseWhiteSpaces
  parseChar '-'
  ds <- parseDigits
  if null ds
    then failParse
    else do
       parseWhiteSpaces
       return (negate $ read ds)


parseInt :: Parser Int
parseInt =
  parseEither parseNegNumber parseNumber


parseAlpha :: Parser Char
parseAlpha = parsePred isAlpha


parseDigit :: Parser Char
parseDigit = parsePred isDigit


parseWhiteSpace :: Parser Char
parseWhiteSpace = parsePred isSpace


ignoreWhiteSpace :: Parser ()
ignoreWhiteSpace = do
  _ <- parseMany parseWhiteSpace
  return ()
