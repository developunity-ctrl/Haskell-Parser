module HParser.Combinators where

import Control.Applicative
import qualified Data.Text as T
import HParser.Declarations
import HParser.InputState

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f = flip (>>=) (pure . f)

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure x =
    Parser
      { parseFn = \input -> Success (x, input),
        pLabel = ParserLabel $ "unknown"
      }

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) fP xP = do
    f <- fP
    x <- xP
    pure $ f x

  -- liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
  liftA2 f pX pY = pure f <*> pX <*> pY

instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) p f =
    Parser
      { parseFn =
          \input -> case runOnInput p input of
            Failure (label, err, pos) -> Failure (label, err, pos)
            Success (result, remaining) -> runOnInput (f result) remaining,
        pLabel = ParserLabel "unknown"
      }

instance Alternative Parser where
  empty =
    Parser
      { parseFn = \input -> emptyFailure,
        pLabel = ParserLabel "empty"
      }

  -- (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) pA@(Parser {pLabel = ParserLabel labelA}) pB@(Parser {pLabel = ParserLabel labelB}) =
    Parser
      { parseFn = \input -> (runOnInput pA input) `combine` (runOnInput pB input),
        pLabel = ParserLabel $ labelA <> " orElse " <> labelB
      }
    where
      combine result@(Success _) _ = result
      combine _ result@(Success _) = result
      combine fail1@(Failure (_, _, pos1)) fail2@(Failure (_, _, pos2)) =
        if (ppColumn pos1) > (ppColumn pos2) then fail1 else fail2

runOnInput :: Parser t -> InputState -> ParseResult (t, InputState)
runOnInput (Parser {parseFn}) input = parseFn input

run :: Parser t -> T.Text -> ParseResult (t, InputState)
run parser inputStr = runOnInput parser $ fromStr inputStr

setLabel parser newLabel =
  Parser
    { parseFn =
        ( \input -> case runOnInput parser input of
            Success s -> Success s
            Failure (oldLabel, err, pos) -> Failure (newLabel, err, pos)
        ),
      pLabel = newLabel
    }

getLabel :: Parser a -> ParserLabel
getLabel (Parser {pLabel}) = pLabel

andThen :: Parser a -> Parser b -> Parser (a, b)
andThen pA@(Parser {pLabel = (ParserLabel labelA)}) pB@(Parser {pLabel = (ParserLabel labelB)}) = do
  pAResult <- pA
  pBResult <- pB
  pure (pAResult, pBResult) <?> ParserLabel (labelA <> " andThen " <> labelB)

between :: Parser a -> Parser b -> Parser c -> Parser b
between pA pB pC = pA >>. pB .>> pC

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep =
  p .>>. many (sep >>. p) |>> (\(p, pList) -> p : pList)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

(.>>.) :: Parser a -> Parser b -> Parser (a, b)
(.>>.) = andThen

(|>>) :: Parser a -> (a -> b) -> Parser b
(|>>) x f = f <$> x

(.>>) :: Parser a -> Parser b -> Parser a
pA .>> pB = (pA .>>. pB) |>> (\(a, b) -> a)

(>>.) :: Parser a -> Parser b -> Parser b
pA >>. pB = (pA .>>. pB) |>> (\(a, b) -> b)

(<?>) :: Parser a -> ParserLabel -> Parser a
(<?>) = setLabel

(>>%) :: Parser a -> b -> Parser b
(>>%) p x = p |>> (\_ -> x)
