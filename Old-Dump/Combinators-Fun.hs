--  only version without typeclass implementations

run :: Parser t -> String -> ParseResult (t, String)
run (Parser {parseFn = parse}) input = parse input

setLabel parser newLabel =
  Parser
    { parseFn =
        ( \input -> case run parser input of
            Success s -> Success s
            Failure (oldLabel, err) -> Failure (newLabel, err)
        ),
      label = newLabel
    }

getLabel :: Parser a -> ParserLabel
getLabel (Parser {label = label}) = label

andThen :: Parser a -> Parser b -> Parser (a, b)
andThen pA@(Parser {label = labelA}) pB@(Parser {label = lableB}) =
  (pA >>= \pAResult -> pB >>= \pBResult -> returnP (pAResult, pBResult))
    <?> ParserLabel
      (show labelA ++ " andThen " ++ show lableB)

orElse :: Parser a -> Parser a -> Parser a
orElse pA@(Parser {label = labelA}) pB@(Parser {label = labelB}) =
  Parser
    { parseFn = \input -> case run pA input of
        result@(Success _) -> result
        Failure _ -> run pB input,
      label = ParserLabel $ show labelA ++ " orElse " ++ show labelB
    }

mapP :: (a -> b) -> Parser a -> Parser b
mapP f = bindP (returnP . f)

returnP :: a -> Parser a
returnP x =
  Parser
    { parseFn = \input -> Success (x, input),
      label = ParserLabel $ show "unknown"
    }

applyP :: Parser (a -> b) -> Parser a -> Parser b
applyP fP xP = fP >>= (\f -> xP >>= (\x -> returnP (f x)))

lift2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
lift2 f pX pY = returnP f <*> pX <*> pY

sequence' :: [Parser a] -> Parser [a]
sequence' = foldr (lift2 (:)) (returnP [])

parseZeroOrMore :: Parser a -> String -> ([a], String)
parseZeroOrMore parser input =
  case run parser input of
    Failure err -> ([], input)
    Success (firstValue, inputAfterFirstValue) ->
      let (subsequentValues, remainingInput) = parseZeroOrMore parser inputAfterFirstValue
       in (firstValue : subsequentValues, remainingInput)

many :: Parser a -> Parser [a]
many p@(Parser {label = pLabel}) =
  Parser
    { parseFn = \input -> Success (parseZeroOrMore p input),
      label = ParserLabel $ "many " ++ show pLabel
    }

many1 :: Parser a -> Parser [a]
many1 parser =
  parser >>= (\head -> many parser >>= (\tail -> returnP (head : tail)))

opt :: Parser a -> Parser (Maybe a)
opt p =
  let some = p |>> Just
      none = returnP Nothing
   in some <|> none

between :: Parser a -> Parser b -> Parser c -> Parser b
between pA pB pC = pA >>. pB .>> pC

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep =
  p .>>. many (sep >>. p) |>> (\(p, pList) -> p : pList)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> returnP []

bindP :: (a -> Parser b) -> Parser a -> Parser b
bindP f p =
  Parser
    { parseFn =
        \input -> case run p input of
          Failure (label, err) -> Failure (label, err)
          Success (result, remaining) -> run (f result) remaining,
      label = ParserLabel "unknown"
    }

(.>>.) :: Parser a -> Parser b -> Parser (a, b)
(.>>.) = andThen

(<|>) :: Parser t -> Parser t -> Parser t
(<|>) = orElse

(<!>) :: (a -> b) -> Parser a -> Parser b
(<!>) = mapP

(|>>) :: Parser a -> (a -> b) -> Parser b
(|>>) x f = mapP f x

(<*>) :: Parser (a -> b) -> Parser a -> Parser b
(<*>) = applyP

(.>>) :: Parser a -> Parser b -> Parser a
pA .>> pB = (pA .>>. pB) |>> (\(a, b) -> a)

(>>.) :: Parser a -> Parser b -> Parser b
pA >>. pB = (pA .>>. pB) |>> (\(a, b) -> b)

(>>=) :: Parser a -> (a -> Parser b) -> Parser b
p >>= f = bindP f p

(<?>) :: Parser a -> ParserLabel -> Parser a
(<?>) = setLabel
