module Parser where

import Control.Applicative
import Control.Monad

newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

failure :: Parser a
failure = Parser $ const Nothing

instance Functor Parser where
  fmap f (Parser p) = Parser $
    p >=> \(a,cs') -> pure (f a, cs')

instance Applicative Parser where
  pure a = Parser $ \cs -> Just (a,cs)
  (<*>) = ap

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser $ p >=> \(a,cs') -> parse (f a) cs'
  --fail _ = failure

instance Alternative Parser where
  empty = failure
  (Parser p) <|> (Parser q) = Parser $ \cs ->
    case p cs of
      Nothing -> q cs
      res     -> res
