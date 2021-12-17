import Text.ParserCombinators.Parsec hiding (State)
import Text.Parsec (Parsec, modifyState, putState, getState)
import Text.Parsec.Char

import Control.Monad.State
import Control.Monad.Identity
import Data.Monoid (Sum(..), Product(..))

import Data.Char (digitToInt)
import GHC.Integer

import Data.List (foldl', foldl1)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ( NonEmpty( (:|) ), (<|) ) 

-- import Helpers (chain)


import Debug.Trace
ttrace x = trace (show x) x

-- example

type NE = NE.NonEmpty
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

  -- switch on type ID
  case t of
    -- all total on NonEmpty
    0 -> sum <$> operator
    1 -> product <$> operator
    2 -> minimum <$> operator 
    3 -> maximum <$> operator
    4 -> binToInt <$> literal
    -- These still have incomplete patterns...
    5 -> (\(x:|(y:xs)) -> if x>y then 1 else 0) <$> operator
    6 -> (\(x:|(y:xs)) -> if x<y then 1 else 0) <$> operator
    7 -> (\(x:|(y:xs)) -> if x==y then 1 else 0) <$> operator
    n -> fail $ "Unknown operator typeID: " ++ show n 


version = binToInt <$> nbits 3 

typeID = binToInt <$> nbits 3

literal :: Parsec String Int String
literal = flip (<?>) "literal" $ litBlocks

litBlocks = do
  litLastBlock <|> moreBlocks

moreBlocks = do
  first <- litBlock
  (first++) <$> litBlocks



operator :: Parsec String Int (NE Answer)
operator = flip (<?>) "operator" $ do
  mode <- bit

  case mode of
    '0' -> operatorMode0
    '1' -> operatorMode1


binOperator :: Parsec String Int (Answer, Answer)
binOperator = flip (<?>) "operator" $ do
  mode <- bit

  case mode of
    '0' -> do
      bitsExpected <- binToInt <$> nbits 15
      --

      result <- (,) <$> packet <*> packet
      return result

    '1' -> do
      binToInt <$> nbits 11 -- 
      result <- (,) <$> packet <*> packet

      return result



-- #TODO actually could abstract over the monoid used for the answer
operatorMode0 :: Parsec String Int (NE Answer)
operatorMode0 = do
  packetsTotalLength <- binToInt <$> nbits 15
  getPacketsUntilBitLength packetsTotalLength

operatorMode1 :: Parsec String Int (NE Answer)
operatorMode1 = do
  numPackets <- binToInt <$> nbits 11
  npackets numPackets


-- uses the internal state
getPacketsUntilBitLength :: Int -> Parsec String Int (NE Answer)
getPacketsUntilBitLength n = do
                              bitsSoFar <- getState
                              putState 0

                              packs <- go n

                              -- make sure the bit count persists
                              putState (bitsSoFar + n)
                              return packs
  where go n = do
        pack <- packet

        cumsum <- getState
        case compare cumsum n of
          EQ -> return $ pack :| []
          LT -> (pack <|) <$> go n
          GT -> fail $ "Parsed " ++ show cumsum ++ " bits trying to parse exactly " ++ show n

npackets 1 = (:| []) <$> packet
npackets n = do
  pack <- packet
  (pack <|) <$> npackets (n-1)

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