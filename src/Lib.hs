{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Lib
  ( startApp
  , app
  )
where

import           System.Random                  ( Random(randomIO) )
import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           GHC.Generics                   ( Generic )
import           Data.List.Split                ( chunksOf )
import           Data.List                      ( transpose )


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

data Side = X | O deriving (Generic, Eq, Read, Show)

data Cell = Input Side | Empty
  deriving (Eq, Read)

instance Show Cell where
  show (Input X) = "X"
  show (Input O) = "O"
  show Empty     = " "

type Row = [Cell]

type Grid = [Row]

type API = "table" :> ReqBody '[JSON] TurnReq :> Get '[JSON] TurnRes

-- type Side = String     "{"name": "Upendra", "job": "Programmer"}"


startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

-- server :: Server API
-- server (TurnReq cell grid) = return (TurnRes Nothing (makeTurn cell grid))

server :: Server API
server (TurnReq cell grid)
  | gameSolved grid = return (TurnRes (Just X) grid)
  | gameSolved (makeTurn cell grid) = return
    (TurnRes (Just O) (makeTurn cell grid))
  | otherwise = return (TurnRes Nothing (makeTurn cell grid))
    -- (case gameSolved grid of
    --   (Just X) -> makeTurn cell grid
    --   (Just O) -> makeTurn cell grid
    --   Nothing  -> makeTurn cell grid
    -- )


makeTurn :: Side -> [[String]] -> [[String]]
makeTurn X grid = chunksOf (length grid) (grid1 ++ grid2)
 where
  grid1 = takeWhile (/= " ") (concat grid)
  grid2 = "O" : tail (dropWhile (/= " ") (concat grid))
  -- grid2       = "O" : tail grid2helper
makeTurn O grid = chunksOf (length grid) (grid1 ++ grid2)
 where
  grid1 = takeWhile (/= " ") (concat grid)
  grid2 = "X" : tail (dropWhile (/= " ") (concat grid))
  -- grid2       = "X" : tail grid2helper

randCell :: IO Int
randCell = do
  num <- randomIO :: IO Int
  return (num `mod` 9)


gameSolved :: [[String]] -> Bool
gameSolved grid =
  rowsSolved || columnsSolved || mainDiagonalSolved || sideDiagonalSolved
 where
  rowsSolved         = any solved grid
  columnsSolved      = any solved (transpose grid)
  mainDiagonalSolved = solved . getDiagonal $ grid
  sideDiagonalSolved = solved . getDiagonal $ (reverse <$> grid)
  solved row = (row == xs) || (row == os)
  solved' row =
    ((length . filter (== Input X) $ row) >= 3)
      || ((length . filter (== Input O) $ row) >= 3)
  xs = replicate (length grid) "X"
  os = replicate (length grid) "O"
  getDiagonal xs = zipWith (!!) xs [0 ..]

-- gameSolved :: [[String]] -> Maybe Side
-- gameSolved grid | Just X `elem` solveds = Just X
--                 | Just O `elem` solveds = Just O
--                 | otherwise             = Nothing
--  where
--   rowsSolved         = solved <$> grid
--   columnsSolved      = solved <$> transpose grid
--   mainDiagonalSolved = solved . getDiagonal $ grid
--   sideDiagonalSolved = solved . getDiagonal $ (reverse <$> grid)
--   solved row | row == xs = Just X
--              | row == os = Just O
--              | otherwise = Nothing
--   solved' row =
--     ((length . filter (== Input X) $ row) >= 3)
--       || ((length . filter (== Input O) $ row) >= 3)
--   xs = replicate (length grid) "X"
--   os = replicate (length grid) "O"
--   getDiagonal xs = zipWith (!!) xs [0 ..]
--   solveds =
--     mainDiagonalSolved : sideDiagonalSolved : rowsSolved ++ columnsSolved
