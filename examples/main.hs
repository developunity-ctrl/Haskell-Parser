module Main where

import Debug.Trace
import HParser.Combinators
import HParser.Declarations
import HParser.Parsers

-- parsers :: [Parser Char]
-- parsers = [pchar 'A', pchar 'B', pchar 'C']

-- combined :: Parser String
-- combined = sequence' parsers

-- intArray = between (pchar '[') (sepBy1 pint (pchar ',')) (pchar ']')

exampleError :: ParseResult (Char, String)
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
  print $ run pfloat "-12.32325Z"
  putStrLn "\nComplete NTParser"
