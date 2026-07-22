module HParser.Declarations where

import qualified Data.Text as T
import HParser.InputState

newtype ParserLabel = ParserLabel T.Text deriving (Eq)

instance Show ParserLabel where
  show (ParserLabel label') = T.unpack label'

newtype ParserError = ParserError T.Text deriving (Eq)

instance Show ParserError where
  show (ParserError error') = T.unpack error'

data ParseResult a
  = Success a
  | Failure (ParserLabel, ParserError, ParserPosition)
  deriving (Eq)

instance (Show b) => Show (ParseResult (b, c)) where
  show (Success (value, _)) = (show value)
  show (Failure (label, error', parserPosition)) =
    let failureCaret = (replicate (colPos + 1) ' ') ++ "^" ++ show error'
        errorLine = show $ ppCurrentLine parserPosition
        colPos = ppColumn parserPosition
        linePos = show $ ppLine parserPosition
     in ("Line: " ++ linePos ++ " Col: " ++ show colPos ++ " Error Parsing " ++ show label ++ "\n" ++ errorLine ++ "\n" ++ failureCaret)

data Parser t = Parser
  { parseFn :: InputState -> ParseResult (t, InputState),
    pLabel :: ParserLabel
  }

data ParserPosition = ParserPosition
  { ppCurrentLine :: T.Text,
    ppLine :: Int,
    ppColumn :: Int
  }
  deriving (Show, Eq)

-- buildFailure :: String -> String -> ParseResult a
-- buildFailure label error' =
--   Failure (ParserLabel label, ParsersError error')

incrCol pos@(Position {pColumn = column}) = pos {pColumn = column + 1}

incrLine pos@(Position {pLine = line}) = pos {pLine = line + 1, pColumn = 0}

parserPositionFromInputState :: InputState -> ParserPosition
parserPositionFromInputState inputState =
  ParserPosition
    { ppCurrentLine = currentLine inputState,
      ppLine = pLine $ position inputState,
      ppColumn = pColumn $ position inputState
    }

currentLine :: InputState -> T.Text
currentLine (InputState {position = position', lines' = lines''})
  | linePos < (length lines'') = lines'' !! linePos
  | otherwise = "end of file"
  where
    linePos = pLine position'

nextChar :: InputState -> (InputState, Maybe Char)
nextChar state@(InputState {position = position', lines' = lines''})
  | linesPos >= (length lines'') =
      (state, Nothing)
  | colPos < (T.length currentLine') =
      (state {position = incrCol position'}, Just $ T.index currentLine' colPos)
  | otherwise =
      (state {position = incrLine position'}, Just '\n')
  where
    currentLine' = currentLine state
    linesPos = pLine position'
    colPos = pColumn position'

readAllChars input =
  let (remainingInput, charOpt) = nextChar input
   in case charOpt of
        Nothing -> []
        Just ch -> ch : readAllChars remainingInput
