module HParser.InputState where

import qualified Data.Text as T

data InputState = InputState
  { lines' :: [T.Text],
    position :: Position
  }
  deriving (Show)

data Position = Position
  { pLine :: Int,
    pColumn :: Int
  }

instance Show Position where
  show (Position {pLine = pLine, pColumn = pColumn}) =
    "line: " ++ show pLine ++ ", column: " ++ show pColumn

fromStr str = InputState {lines' = T.lines str, position = initialPos}

initialPos = Position {pLine = 0, pColumn = 0}
