module Main where

import Examples
import ParseExpressions
import ParseStatements
import Parser
import PrimitiveFunctionality
import Data.Char
import Data.Maybe
import Control.Applicative
import Types
import Data.List
import Data.List.Split(splitOn)
import ParseExternalDeclarations
import ToJML
import JMLTypes
import RefineParsed
import Control.Exception(throw)

main :: IO ()
main = --print $ parse parseFunDef example109
  --print $ getRequireBehavior False (example109++example110) "boo24"
  putStr $ jmlify example107