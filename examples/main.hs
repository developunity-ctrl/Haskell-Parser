module Main where

import Debug.Trace
import HParser.Declarations
import HParser.Combinators

printResult :: ParseResult (Char, String) -> IO ()
printResult (Success (value, _)) = print $ show value
printResult
  (Failure (label, error', parserPosition)) =
    let failureCaret = (replicate colPos ' ') ++ "^" ++ show error'
        errorLine = show $ ppCurrentLine parserPosition
        colPos = ppColumn parserPosition
        linePos = show $ ppLine parserPosition
     in putStrLn ("Line: " ++ linePos ++ " Col: " ++ show colPos ++ " Error Parsing " ++ show label ++ "\n" ++ errorLine ++ "\n" ++ failureCaret)

-- parsers :: [Parser Char]
-- parsers = [pchar 'A', pchar 'B', pchar 'C']

-- combined :: Parser String
-- combined = sequence' parsers

-- intArray = between (pchar '[') (sepBy1 pint (pchar ',')) (pchar ']')

exampleError =
  Failure
    ( ParserLabel "indentifier",
      ParserError "unexpected |",
      ParserPosition
        { ppCurrentLine = "123 ab|cd",
          ppLine = 1,
          ppColumn = 6
        }
    )

main = do
  putStrLn "\nStarted NTParser\n"
  printResult exampleError
  putStrLn "\nComplete NTParser"
