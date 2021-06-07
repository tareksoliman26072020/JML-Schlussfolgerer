module Main where

import Examples
import Parser.ParseStmt
import Text.ParserCombinators.Parsec
import Data.Either
import JML.ToJML(jmlify,toJML)
import System.Environment(getArgs)
import Text.Printf(printf)
import Data.List(foldl',isPrefixOf)
import Parser.Print

main :: IO()
--main = putStr $ jmlify example107
main = do
  path <- getArgs
  inp <- readFile (head path)
  let extDeclList = fromRight undefined (parse parseDeclList "" inp)
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
    editFileName path =
      let splitted = splitAt' (".java" `isPrefixOf`) path
      in fst splitted ++ "_JML" ++ snd splitted

    -- Unlike splitAt',
    -- it uses a predicate instead of a position in the list
    splitAt' :: ([a] -> Bool) -> [a] -> ([a],[a])
    splitAt' f list = f1 f ([],list)

    f1 :: ([a] -> Bool) -> ([a],[a]) -> ([a],[a])
    f1 f (l1,l2) | f l2 = (l1,l2)
    f1 _ (l1,[])        = (l1,[])
    f1 f (l1,l2)        = f1 f (l1 ++ [head l2], tail l2)
