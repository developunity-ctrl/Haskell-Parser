module HParser.InputState where

data InputState = InputState
  { lines' :: [String],
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

fromStr str = InputState {lines' = lines str, position = initialPos}

initialPos = Position {pLine = 0, pColumn = 0}
