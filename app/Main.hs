module Main where

import Examples
import Parser.ParseStmt
import Text.ParserCombinators.Parsec
import Data.Either
import JML.ToJML(jmlify,toJML)
import Data.List(isInfixOf)
import Data.List.Split(splitOn)
import System.Environment(getArgs)
import Text.Printf(printf)
import Data.List(foldl')
import Parser.Print

main :: IO()
--main = putStr $ jmlify example107
main = do
  path <- getArgs
  inp <- readFile (head path)
  let extDeclList = fromRight undefined (parse (many parseExtDecl) "" inp)
  --let prettyP = foldl' (\acc elm -> acc ++ showDecl elm) "" extDeclList
  let res = jmlify extDeclList
  let prettyP = foldl' (\acc (jml,extDecl) -> acc ++ toJML jml ++ "\n" ++ showDecl extDecl ++ "\n\n") "" res
  let newFilePath = editFileName $ head path
  let line = replicate (16 + length newFilePath) '-'
  writeFile newFilePath prettyP
  putStrLn prettyP
  putStrLn $ printf "|%s|\n|%s|\nFile is created: %s\n|%s|\n|%s|" line line newFilePath line line
  where
    editFileName :: FilePath -> FilePath
    editFileName path
      | isInfixOf ".java" path =
          let splitted = splitOn ".java" path
          in head splitted ++ "_JML" ++ ".java"
      | otherwise = path ++ "_JML"
