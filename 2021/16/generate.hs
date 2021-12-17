import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec.Char

import Data.Char (digitToInt)
import Data.List (foldl', foldr)
import Numeric

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ( NonEmpty( (:|) ), (<|) ) 

-- import Helpers (chain)

import Debug.Trace
ttrace x = trace (show x) x

type NE = NE.NonEmpty



-- ====================================================
--                        IO
-- ====================================================


main = do
  input <- getContents
  expr <- parseExpression input
  putStrLn $ take 1000 $ encodeHex $ encodeBITS 7 $ expr

parseExpression :: String -> IO Expression
parseExpression input = do
  let parseResult = parse expression "(unknown)" input
  case parseResult of
    (Left err) -> fail $ show err
    (Right answer) -> return answer


-- =======================================================

data Expression = Number Int
                  | SumExp (NE Expression)
                  | ProdExp (NE Expression)
                  | MinExp (NE Expression)
                  | MaxExp (NE Expression)
                  | LTExp (Expression, Expression)
                  | GTExp (Expression, Expression)
                  | EQExp (Expression, Expression) 
instance Show Expression where
  show (Number n) = show n
  show (SumExp vals) = showBracket '+' vals
  show (ProdExp vals) = showBracket '*' vals
  show (MinExp vals) = showBracket 'v' vals
  show (MaxExp vals) = showBracket '^' vals
  show (LTExp vals) = showPair '<' vals
  show (GTExp vals) = showPair '>' vals
  show (EQExp vals) = showPair '=' vals

showBracket sep (x:|xs) = "(" ++ (show x) ++ concatMap ((sep:) . show) xs ++ ")"
showPair sep (x,y) = "(" ++ (show x) ++ sep:(show y) ++ ")"

encodeBITS :: Int -> Expression -> String
encodeBITS version (Number n) = versionBinary ++ typeIDBinary ++ val
  where

    versionBinary = padToLength 3 $ showBinary version
    typeIDBinary = "100"

    -- val = valV2
    val = if valV1 == valV2
            then valV1
            else trace ("encode strats diverged! \nV1: " ++ valV1 ++ "\nV2: " ++ valV2) valV1
    valV1 = blocks padded
    valV2 = padFirstBlock $ foldr makeBlock ([], []) rawBits


    rawBits = showBinary n
    padded = (take (4 - (length rawBits `mod` 4) `mod` 4) $ repeat '0') ++ rawBits

    blocks xs@(x1:x2:x3:x4:[]) = '0':xs
    blocks xs = ('1':(take 4 xs)) ++ blocks (drop 4 xs)


    padFirstBlock ([], xs) = xs
    padFirstBlock (extras, xs) = padToLength 5 extras ++ xs
    -- make last block
    makeBlock x1 (block@(x2:x3:x4:[]), []) = ([], '0':x1:x2:x3:x4:[])
    makeBlock x (block, []) = (x:block, [])
    -- make other blocks
    makeBlock x1 (block@(x2:x3:x4:[]), blocks) = ([], '1':x1:x2:x3:x4:blocks)
    makeBlock x (block, blocks) = (x:block, blocks)

encodeBITS version (SumExp vals) = encodeOperator version 0 vals
encodeBITS version (ProdExp vals) = encodeOperator version 1 vals
encodeBITS version (MinExp vals) = encodeOperator version 2 vals
encodeBITS version (MaxExp vals) = encodeOperator version 3 vals
encodeBITS version (GTExp (x,y)) = encodeOperator version 5 (x:|[y])
encodeBITS version (LTExp (x,y)) = encodeOperator version 6 (x:|[y])
encodeBITS version (EQExp (x,y)) = encodeOperator version 7 (x:|[y])

encodeOperator :: Int -> Int -> NE Expression -> String
encodeOperator version typeID vals = versionBinary ++ typeIDBinary ++ valsBin
  where
    versionBinary = padToLength 3 $ showBinary version
    typeIDBinary = padToLength 3 $ showBinary typeID
    valsBin = encodePacketsCount version vals

encodePacketsBitLength :: Int -> NE Expression -> String
encodePacketsBitLength version vals = lengthType ++ lengthVal ++ valsBin
  where
    lengthType = "0"
    lengthVal = padToLength 15 $ showBinary $ length valsBin
    valsBin = concatMap (encodeBITS version) vals

encodePacketsCount :: Int -> NE Expression -> String
encodePacketsCount version vals = lengthType ++ lengthVal ++ valsBin
  where
    lengthType = "1"
    lengthVal = padToLength 11 $ showBinary $ length vals
    valsBin = concatMap (encodeBITS version) vals






padToLength :: Int -> String -> String
padToLength n xs = (take (n - length xs) $ repeat '0') ++ xs


showBinary :: Int -> String
showBinary n = reverse $ go n
  where
    go 0 = "0"
    go 1 = "1"
    go n = ((go $ n`mod`2)++) $ go (n `div` 2)

decodeHex :: Char -> String
decodeHex '0' = "0000"
decodeHex '1' = "0001"
decodeHex '2' = "0010"
decodeHex '3' = "0011"
decodeHex '4' = "0100"
decodeHex '5' = "0101"
decodeHex '6' = "0110"
decodeHex '7' = "0111"
decodeHex '8' = "1000"
decodeHex '9' = "1001"
decodeHex 'A' = "1010"
decodeHex 'B' = "1011"
decodeHex 'C' = "1100"
decodeHex 'D' = "1101"
decodeHex 'E' = "1110"
decodeHex 'F' = "1111"


encodeHex :: String -> String
encodeHex "0000" = "0"
encodeHex "0001" = "1"
encodeHex "0010" = "2"
encodeHex "0011" = "3"
encodeHex "0100" = "4"
encodeHex "0101" = "5"
encodeHex "0110" = "6"
encodeHex "0111" = "7"
encodeHex "1000" = "8"
encodeHex "1001" = "9"
encodeHex "1010" = "A"
encodeHex "1011" = "B"
encodeHex "1100" = "C"
encodeHex "1101" = "D"
encodeHex "1110" = "E"
encodeHex "1111" = "F"
encodeHex xs 
  | length (take 4 xs) < 4 = encodeHex (xs ++ "0") 
  | otherwise = encodeHex (take 4 xs) ++ encodeHex  (drop 4 xs)





binToInt :: String -> Int
binToInt = foldl' (\acc x -> acc*2 + digitToInt x) 0


-- =========================================================
--                             Parsers
-- =========================================================



expression = try numExp <|> 
              try sumExp <|> 
              try prodExp <|>
              try minExp <|>
              try maxExp <|>
              try ltExp <|>
              try gtExp <|>
              try eqExp

numExp = Number <$> (integer <|> do
                          bracket <- oneOf "({[<"
                          val <- integer
                          close bracket
                          return val
                        )

sumExp = SumExp <$> bracketed '+'
prodExp = ProdExp <$> bracketed '*'
minExp = MinExp <$> bracketed 'v'
maxExp = MaxExp <$> bracketed '^'

ltExp = LTExp <$> pair '<'
gtExp = GTExp <$> pair '>'
eqExp = EQExp <$> pair '='

bracketed sep = do
  bracket <- oneOf "({[<"
  first <- expression
  char sep
  rest <- sepBy expression (char sep)
  close bracket
  return $ first :| rest

pair sep = do
  bracket <- oneOf "({[<"
  x <- expression
  char sep
  y <- expression
  close bracket
  return (x,y)

close :: Char -> Parser Char
close '(' = char ')'
close '[' = char ']'
close '{' = char '}'
close '<' = char '>'

-- commonly used
integer :: Parser Int
integer = read <$> many1 digit

end :: Parser ()
end = (endOfLine >> return ()) <|> eof 