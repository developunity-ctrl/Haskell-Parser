module HParser.Parsers where

import Control.Applicative (Alternative (..), optional)
import Data.List (intersperse)
import qualified Data.Text as T
import Debug.Trace (trace)
import HParser.Combinators
import HParser.Declarations
import HParser.InputState

satisfy :: (Char -> Bool) -> T.Text -> Parser Char
satisfy predicate label' =
  Parser
    { parseFn = innerFn,
      pLabel = ParserLabel label'
    }
  where
    innerFn input@(nextChar -> (remainingInput, charOpt))
      | Nothing <- charOpt = Failure (ParserLabel label', ParserError "No more input", parserPositionFromInputState input)
      | Just first <- charOpt, predicate first = Success (first, remainingInput)
      | Just first <- charOpt = Failure (ParserLabel label', ParserError ("Unexpected " <> (T.singleton first)), parserPositionFromInputState input)

pchar :: Char -> Parser Char
pchar c = satisfy (== c) (T.singleton c)

pstring :: T.Text -> Parser T.Text
pstring = (T.foldr' ((liftA2 T.cons) . pchar) (pure ""))

pint :: Parser Int
pint =
  optional (pchar '-') .>>. digits |>> resultToInt <?> (ParserLabel "integer")
  where
    digits = some $ anyOf ['1' .. '9']
    resultToInt (sign, digitsList) =
      let integer = (read digitsList :: Int)
       in case sign of
            Just _ -> -integer
            Nothing -> integer

pfloat :: Parser Double
pfloat =
  (optional (pchar '-') .>>. digits .>>. pchar '.' .>>. digits) |>> resultToFloat <?> (ParserLabel "float")
  where
    digits = some $ anyOf ['0' .. '9']
    resultToFloat (((sign, digits1), point), digits2) =
      let float = (read (digits1 ++ "." ++ digits2) :: Double)
       in case sign of
            Just _ -> -float
            Nothing -> float

choice :: (Foldable f) => f (Parser a) -> Parser a
choice = foldr (<|>) empty

anyOf :: [Char] -> Parser Char
anyOf listOfChars =
  let text = T.pack listOfChars
   in satisfy (`T.elem` text) ""
        <?> ParserLabel ("any of " <> T.intersperse ';' text)
