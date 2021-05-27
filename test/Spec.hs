import Parser.ParseStmt (parseDeclList)
import JML.ToJML(jmlify,toJML)
import Parser.Print (showDecl)
import Data.List(foldl')
import Text.Printf(printf)
import Text.ParserCombinators.Parsec (parse)

main :: IO ()
main = mapM_ run
  ["test" ++ n ++ ".java" | n <- [""]]

run :: String -> IO ()
run file = do
  let o = file ++ ".out"
  s <- readFile file
  case parse parseDeclList file s of
    Left e -> print e
    Right extDeclList -> do
      let jmlified = jmlify extDeclList
          prettyP = foldl' (\acc (jml,extDecl) -> acc ++ toJML jml ++ "\n" ++ showDecl extDecl ++ "\n\n") "" jmlified
          line = replicate (16 + length o) '-'
          doneMsg = printf "|%s|\n\
                           \|%s|\n\
                           \File is created: %s\n\
                           \|%s|\n\
                           \|%s|\n"
                    line line o line line

      writeFile o prettyP
      putStrLn prettyP <* putStrLn doneMsg