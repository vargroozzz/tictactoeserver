{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import           Network.HTTP.Simple
import           GHC.Generics                   ( Generic )
import           Data.Aeson
import           Data.List.Split                ( chunksOf )
import           GHC.Read
import qualified Text.Read.Lex                 as L
import           Text.Read                      ( pfail )
import           Data.Time
import           System.Random

data TurnReq = TurnReq { sideTurn :: Side, gridReq :: [[String]] } deriving (Generic, Show)
data TurnRes = TurnRes { sideWon :: Maybe Side, gridRes :: [[String]] } deriving (Generic, Show)

instance ToJSON Side where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON Side

instance ToJSON TurnReq where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TurnReq

instance ToJSON TurnRes where
  toEncoding = genericToEncoding defaultOptions
instance FromJSON TurnRes

data Side = X | O | Draw deriving (Generic, Eq, Read, Show)

data Cell = Input Side | Empty
  deriving (Eq)

instance Show Cell where
  show (Input X   ) = "X"
  show (Input O   ) = "O"
  show (Input Draw) = "Draw"
  show Empty        = " "

instance Read Cell where
  readPrec = parens
    (do
      L.Ident s <- lexP
      case s of
        "X"    -> return (Input X)
        "O"    -> return (Input O)
        "Draw" -> return (Input Draw)
        " "    -> return Empty
        _      -> pfail
    )

type Row = [Cell]

type Grid = [Row]

main :: IO ()
main = do
  startTime <- getCurrentTime
  request'  <- parseRequest "GET http://127.0.0.1:8080/game"
  first50   <- sequence (playNTimes request' 50 X)
  second50  <- sequence (playNTimes request' 50 O)
  print $ first50 ++ second50
  endTime <- getCurrentTime
  putStrLn $ "played 100 games in " ++ show (diffUTCTime endTime startTime)
 where
  play request' side table = do
    newTable <- if side == X || not (and (elem "" <$> table))
      then makeRandomTurn side table
      else return table
    let request = setRequestBodyJSON (TurnReq side newTable) request'
    response <- httpJSON request :: IO (Response TurnRes)
    case sideWon . getResponseBody $ response of
      Nothing       -> play request' side (gridRes . getResponseBody $ response)
      (Just winner) -> return winner
  playNTimes request' n side = play request' side <$> replicate n emptyTable

makeTurn :: Side -> [[String]] -> [[String]]
makeTurn X grid = chunksOf (length grid) (grid1 ++ grid2)
 where
  grid1 = takeWhile (/= " ") (concat grid)
  grid2 = "O" : tail (dropWhile (/= " ") (concat grid))

makeTurn O grid = chunksOf (length grid) (grid1 ++ grid2)
 where
  grid1 = takeWhile (/= " ") (concat grid)
  grid2 = "X" : tail (dropWhile (/= " ") (concat grid))

makeRandomTurn :: Side -> [[String]] -> IO [[String]]
makeRandomTurn side grid = do
  (row, col) <- helper
  return (insert row col (show side) grid)
 where
  insert r c el list = r1 ++ ((c1 ++ (el : c2)) : r2)
   where
    (r1, (c' : r2)) = splitAt r list
    (c1, (_ : c2) ) = splitAt c c'
  helper = do
    row <- randCell (length grid - 1)
    col <- randCell (length grid - 1)
    if grid !! row !! col == " " then return (row, col) else helper


randCell :: Int -> IO Int
randCell size = getStdRandom (randomR (0, size))

emptyTable :: [[String]]
emptyTable = [[" ", " ", " "], [" ", " ", " "], [" ", " ", " "]]
