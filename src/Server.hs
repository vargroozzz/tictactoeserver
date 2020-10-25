{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Server
  ( startApp
  , app
  )
where

import           Data.Aeson

import           Network.Wai.Handler.Warp       ( run )
import           Servant
import           GHC.Generics                   ( Generic )
import           Data.List                      ( transpose )
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

instance Random (Int,Int) where
  random g1 = ((r1, r2), g3)
   where
    (r1, g2) = random g1
    (r2, g3) = random g2
  randomR ((a1, a2), (b1, b2)) g1 = ((a, b), g3)
   where
    (a, g2) = randomR (a1, b1) g1
    (b, g3) = randomR (a2, b2) g2


data Side = X | O | Draw deriving (Generic, Eq, Read, Show)

data Cell = Input Side | Empty
  deriving (Eq, Read)

instance Show Cell where
  show (Input X) = "X"
  show (Input O) = "O"
  show Empty     = " "

type Row = [Cell]

type Grid = [Row]

type API = "game" :> ReqBody '[JSON] TurnReq :> Get '[JSON] TurnRes

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server (TurnReq cell grid)
  | gameSolved grid
  = return $ TurnRes (Just cell) grid
  | not (any (elem " ") grid)
  = return $ TurnRes (Just Draw) grid
  | gameSolved (makePseudoRandomTurn (otherSide cell) grid)
  = return $ TurnRes (Just $ otherSide cell)
                     (makePseudoRandomTurn (otherSide cell) grid)
  | not (any (elem " ") (makePseudoRandomTurn (otherSide cell) grid))
  = return $ TurnRes (Just Draw) (makePseudoRandomTurn (otherSide cell) grid)
  | otherwise
  = return (TurnRes Nothing (makePseudoRandomTurn (otherSide cell) grid))
 where
  otherSide X = O
  otherSide O = X

makePseudoRandomTurn :: Side -> [[String]] -> [[String]]
makePseudoRandomTurn side grid = (insert row col (show side) grid)
 where
  (row, col) = finalRandCell randCells
  finalRandCell (c@(row', col') : cs) =
    if grid !! row' !! col' == " " then c else finalRandCell cs
  randCells = randomRs ((0, 0), (length grid - 1, length grid - 1))
                       (mkStdGen seed)
   where
    seed = sum (accum 0 (concat grid))
    accum _ []       = []
    accum n (x : xs) = seedhelper x * n : accum (n + 1) xs
    seedhelper " " = 0
    seedhelper "X" = 1
    seedhelper "O" = -1
  insert r c el list = r1 ++ ((c1 ++ (el : c2)) : r2)
   where
    (r1, (c' : r2)) = splitAt r list
    (c1, (_ : c2) ) = splitAt c c'

gameSolved :: [[String]] -> Bool
gameSolved grid =
  rowsSolved || columnsSolved || mainDiagonalSolved || sideDiagonalSolved
 where
  rowsSolved         = any solved grid
  columnsSolved      = any solved (transpose grid)
  mainDiagonalSolved = solved . getDiagonal $ grid
  sideDiagonalSolved = solved . getDiagonal $ (reverse <$> grid)
  solved row = (row == xs) || (row == os)
  xs = replicate (length grid) "X"
  os = replicate (length grid) "O"
  getDiagonal xs = zipWith (!!) xs [0 ..]
