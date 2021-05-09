module Main where

import Examples
import Parser
import ParseExternalDeclarations
import Control.Monad.State(StateT(..))
import ToJML(jmlify)
import Data.List(isInfixOf)
import Data.List.Split(splitOn)
import System.Environment(getArgs)
import Text.Printf(printf)

main :: IO()
--main = putStr $ jmlify example107
main = do
  path <- getArgs
  inp <- readFile (head path)
  let res = jmlify inp
  let newFilePath = editFileName $ head path
  let line = replicate (16 + length newFilePath) '-'
  writeFile newFilePath res
  putStrLn res
  putStrLn $ printf "|%s|\n|%s|\nFile is created: %s\n|%s|\n|%s|" line line newFilePath line line
  where
    editFileName :: FilePath -> FilePath
    editFileName path
      | isInfixOf ".java" path =
          let splitted = splitOn ".java" path
          in head splitted ++ "_JML" ++ ".java"
      | otherwise = path ++ "_JML"