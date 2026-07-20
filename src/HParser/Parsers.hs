module HParser.Parsers where

import Control.Applicative (Alternative (..), optional)
import Data.List (intersperse)
import Debug.Trace (trace)
import HParser.Combinators
import HParser.Declarations
import HParser.InputState

satisfy :: (Char -> Bool) -> String -> Parser Char
satisfy predicate label' =
  Parser
    { parseFn = innerFn,
      pLabel = ParserLabel label'
    }
  where
    innerFn input =
      let (remainingInput, charOpt) = nextChar input
       in case charOpt of
            Nothing -> Failure (ParserLabel label', ParserError "No more input", parserPositionFromInputState input)
            Just first ->
              if predicate first
                then
                  Success (first, remainingInput)
                else
                  Failure (ParserLabel label', ParserError ("Unexpected " ++ [first]), parserPositionFromInputState input)

pchar :: Char -> Parser Char
pchar c = satisfy (== c) [c]

pstring :: String -> Parser String
pstring = sequence' . (map pchar)

pint :: Parser Int
pint =
  optional (pchar '-') .>>. digits |>> resultToInt
  where
    digits = setLabel (some $ anyOf ['1' .. '9']) $ ParserLabel "digits"
    resultToInt (sign, digitsList) =
      let integer = (read digitsList :: Int)
       in case sign of
            Just _ -> -integer
            Nothing -> integer

choice :: (Foldable f) => f (Parser a) -> Parser a
choice = foldr (<|>) empty

anyOf :: [Char] -> Parser Char
anyOf listOfChars =
  satisfy (`elem` listOfChars) ""
    <?> ParserLabel ("any of " ++ intersperse ';' listOfChars)
