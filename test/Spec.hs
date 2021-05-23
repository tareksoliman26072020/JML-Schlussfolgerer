import Parser.ParseStmt (parseDeclList)
import Parser.Print (showDecl)

import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = do
  let f = "test.java"
      o = f ++ ".out"
  s <- readFile f
  case parse parseDeclList f s of
    Left e -> print e
    Right l -> do
      writeFile o . unlines $ map showDecl l
      putStrLn $ "written file: " ++ o
