import Parser.ParseStmt (parseDeclList)
import Parser.Print (showDecl)
import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = run "test.java"

run :: String -> IO ()
run file = do
  let o = file ++ ".out"
  s <- readFile file
  case parse parseDeclList file s of
    Left e -> print e
    Right extDeclList ->
      writeFile o . unlines $ map showDecl extDeclList
