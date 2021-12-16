import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec (Parsec, modifyState, putState, getState)
import Text.Parsec.Char

import Control.Monad.State
import Control.Monad.Identity

import Data.Char (digitToInt)

import Data.List (foldl')
-- import Helpers (chain)


import Debug.Trace
ttrace x = trace (show x) x

-- example

type Answer = Int


part1 = True

main = do
  vals <- getAnswer
  print vals

-- basically solve
getAnswer :: IO Answer
getAnswer = do
  input <- getContents
  let parseResult = runParser puzzle 0 "(unknown)" $ concatMap decodeHex input
  case parseResult of
    (Left err) -> fail $ show err
    (Right answer) -> return answer


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


binToInt :: String -> Int
binToInt = foldl' (\acc x -> acc*2 + digitToInt x) 0


-- =========================================================
--                             Parsers
-- =========================================================

--example
puzzle :: Parsec String Int Answer
puzzle = do
  result <- packet
  many zero
  eof
  return result

packet :: Parsec String Int Answer
packet = do
  v <- version
  t <- typeID

  temp <- getState


  -- switch on type ID
  case t of
    4 -> (const v) <$> literal
    n -> (\x -> v + x) <$> operator n


version = binToInt <$> nbits 3 

typeID = binToInt <$> nbits 3

literal :: Parsec String Int String
literal = flip (<?>) "literal" $ litBlocks

litBlocks = do
  litLastBlock <|> moreBlocks

moreBlocks = do
  first <- litBlock
  (first++) <$> litBlocks





operator :: Int ->  Parsec String Int Answer
operator n = flip (<?>) "operator" $ do
  mode <- bit

  case mode of
    '0' -> operatorMode0 n
    '1' -> operatorMode1 n


operatorMode0 :: Int ->  Parsec String Int Answer
operatorMode0 n = do
  packetsTotalLength <- binToInt <$> nbits 15
  
  bitsSoFar <- getState
  putState 0

  packs <- getPacketsUntilBitLength packetsTotalLength

  -- make sure the bit count persists
  putState (bitsSoFar + packetsTotalLength)

  return packs

operatorMode1 :: Int -> Parsec String Int Answer
operatorMode1 n = do
  numPackets <- binToInt <$> nbits 11
  getNPackets numPackets


getNPackets :: Int -> Parsec String Int Answer
getNPackets n = do
  if n==0
    then return 0
    else do
      pack <- packet
      (pack +) <$> (getNPackets (n-1))

-- uses the internal state
getPacketsUntilBitLength :: Int -> Parsec String Int Answer
getPacketsUntilBitLength n = do
  cumsum <- getState
  if cumsum == n
    then return 0
    else do
      pack <- packet
      (pack +) <$> getPacketsUntilBitLength n

npackets 0 = return 0
npackets n = do
  pack <- packet
  (pack +) <$> npackets (n-1)

nbits 0 = return ""
nbits n = do
  ch <- bit
  (ch:) <$> nbits (n-1)

litBlock = do
  one
  nbits 4

litLastBlock = do
  zero
  nbits 4


one =  flip (<?>) "0" $ do
  char '1'
  modifyState (+1)
  return '1'
zero =  flip (<?>) "1" $ do
  char '0'
  modifyState (+1)
  return '0'

bit =  flip (<?>) "bit" $ do
  b <- oneOf "01" 
  modifyState (+1)
  return b




-- commonly used

integer :: Parser Int
integer = read <$> many1 digit

end :: Parser ()
end = (endOfLine >> return ()) <|> eof