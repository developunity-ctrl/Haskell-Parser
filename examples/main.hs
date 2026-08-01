{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Applicative
import Data.Char (chr, isDigit)
import Data.List (intercalate)
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text.Read (double, signed)
import Debug.Trace
import GHC.Arr (array)
import HParser.Combinators
import HParser.Declarations
import HParser.Parsers
import Numeric (readHex)
import Text.RawString.QQ
import Text.Read (readMaybe)

data JValue
  = JString String
  | JNumber Double
  | JBool Bool
  | JNull
  | JObject (Map.Map String JValue)
  | JArray [JValue]

-- basic pretty print
instance Show JValue where
  show = pretty 2 0
    where
      pretty :: Int -> Int -> JValue -> String
      pretty step indent a =
        case a of
          JNull -> "null"
          JString str -> "\"" ++ str ++ "\""
          JArray arr -> "[" ++ (intercalate ", " . map (pretty step indent)) arr ++ "]"
          JNumber num -> show num
          JBool bool -> show bool
          JObject obj
            | Map.null obj -> "{}"
            | otherwise -> "{\n" ++ prettyPair obj ++ "\n" ++ (spaces indent) ++ "}"
        where
          nextIndent = indent + step
          spaces n = replicate n ' '
          prettyPair = (intercalate (",\n") . map (\(k, v) -> (spaces nextIndent) ++ show k ++ ": " ++ pretty step nextIndent v) . Map.toList)

jNull = (pstring "null" >>% JNull) <?> (ParserLabel "null")

jBool =
  (jtrue <|> jfalse) <?> (ParserLabel "bool")
  where
    jtrue = pstring "true" >>% JBool True
    jfalse = pstring "false" >>% JBool False

jUnescapedChar =
  satisfy (\ch -> ch /= '\\' && ch /= '\"') "char"

jEscapedChar =
  ( choice $
      map
        (\(toMatch, result) -> pstring toMatch >>% result)
        [ ("\\\"", '\"'),
          ("\\\\", '\\'),
          ("\\/", '/'),
          ("\\b", '\b'),
          ("\\f", '\f'),
          ("\\n", '\n'),
          ("\\r", '\r'),
          ("\\t", '\t')
        ]
  )
    <?> ParserLabel "escaped char"

jUnicodeChar =
  (pchar '\\') >>. (pchar 'u') >>. fourHexDigits |>> convertToChar
  where
    fourHexDigits :: Parser (((Char, Char), Char), Char)
    fourHexDigits =
      let hexDigit = anyOf (['0' .. '9'] ++ ['A' .. 'F'] ++ ['a' .. 'f'])
       in hexDigit
            .>>. hexDigit
            .>>. hexDigit
            .>>. hexDigit

    convertToChar :: (((Char, Char), Char), Char) -> Char
    convertToChar (((h1, h2), h3), h4) =
      let hexStr = [h1, h2, h3, h4]
       in case readHex hexStr of
            [(val, _)] -> chr val

quotedString =
  let quote = pchar '\"' <?> ParserLabel "quote"
      jchar = jUnescapedChar <|> jEscapedChar <|> jUnicodeChar
   in quote >>. (many jchar) .>> quote

jString = quotedString |>> JString <?> ParserLabel "quoted string"

jNumber =
  let optSign = optional (pchar '-')
      zero = pstring "0"
      digitOneNine = satisfy (\ch -> isDigit ch && ch /= '0') "1-9"
      digit = satisfy isDigit "digit"
      point = pchar '.'
      e = pchar 'e' <|> pchar 'E'
      optPlusMinus = optional (pchar '-' <|> pchar '+')
      nonZeroInt = digitOneNine .>>. (many digit |>> T.pack) |>> (\(first, rest) -> first `T.cons` rest)
      intPart = zero <|> nonZeroInt
      fractionPart = point >>. some digit |>> T.pack
      exponentPart = e >>. optPlusMinus .>>. (some digit |>> T.pack)

      (|>?) (Nothing) f = ""
      (|>?) (Just x) f = f x

      convertToJNumber (((optSign, intPart), fractionPart), expPart) =
        let signStr = optSign |>? T.singleton
            fractionPartStr = fractionPart |>? (\digits -> T.cons '.' digits)
            expPartStr = expPart |>? (\(optSign, digits) -> T.concat ["e", (optSign |>? T.singleton), digits])
            numberStr = T.concat [signStr, intPart, fractionPartStr, expPartStr]
         in case fmap fst (signed double numberStr) of
              Left failure -> error failure
              Right number -> JNumber number
   in (optSign .>>. intPart .>>. optional fractionPart .>>. optional exponentPart) |>> convertToJNumber <?> ParserLabel "number"

jNumber_ = jNumber .>>. someWhitespace

jValue = jNumber <|> jString <|> jBool <|> jNull <|> jArray <|> jObject

jArray =
  let left = pchar '[' .>> manyWhitespaces
      right = pchar ']' .>> manyWhitespaces
      comma = pchar ',' .>> manyWhitespaces
      value = jValue .>> manyWhitespaces
      values = sepBy value comma
   in between left values right |>> JArray <?> ParserLabel "array"

jObject =
  let left = manyWhitespaces >>. pchar '{' .>> manyWhitespaces
      right = pchar '}' .>> manyWhitespaces
      colon = pchar ':' .>> manyWhitespaces
      comma = pchar ',' .>> manyWhitespaces
      key = quotedString .>> manyWhitespaces
      value = jValue .>> manyWhitespaces

      keyValuePair = (key .>> colon) .>>. value
      keyValuePairs = sepBy keyValuePair comma
   in between left keyValuePairs right |>> Map.fromList |>> JObject

main = do
  putStrLn "\nStarted NTParser\n"
  print $
    run
      jObject
      [r|{
        "name" : "Scott",
        "isMale" : true,
        "bday" : {"year":2001, "month":12, "day":25 },
        "favouriteColors" : ["blue", "green"],
        "emptyArray" : [],
        "emptyObject" : {},
        "nestedEvenMore": {
          "evenMore": {
            "even": 1
          }
        }
      }|]
  putStrLn "\nComplete NTParser"
