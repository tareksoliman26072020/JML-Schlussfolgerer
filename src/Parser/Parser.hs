module Parser.Parser where

import Control.Monad.State(StateT(..))

type Parser a = StateT String Maybe a
--newtype Parser a = Parser { parse :: String -> Maybe (a, String) }

failure :: Parser a
failure = StateT $ const Nothing
