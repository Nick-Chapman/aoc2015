module Day18 (main) where

import Prelude hiding (head)
import Misc (check,head)
import Par4 (Par,parse,separated,many,alts,nl,lit)
import Data.Map (Map)
import Data.Map qualified as Map

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day18.sam"
  inp <- parse gram <$> readFile "input/day18.input"
  print ("day18, part1 (sample)", check 4 $ part1 4 sam)
  print ("day18, part1", check 768 $ part1 100 inp)
  print ("day18, part2 (sample)", check 17 $ part2 5 sam)
  print ("day18, part2", check 781 $ part2 100 inp)

{-run :: Grid -> IO ()
run g = do
  let ws = take 3 $ iterate (sticky . step) (sticky (toWorld g))
  mapM_ (print . fromWorld) ws

fromWorld :: World -> Grid
fromWorld World{size=n,m} =
  Grid [ [ maybe undefined id $ Map.lookup (x,y) m | x <- [0..n-1] ] | y <- [0..n-1] ]
-}

data Grid = Grid [[Bool]] deriving Eq

instance Show Grid where
  show (Grid xss) =
    unlines [ map (\case True -> '#'; False -> '.') xs | xs <- xss ]

gram :: Par Grid
gram = Grid <$> separated nl (many cell)
  where
    cell = alts [ do lit '.'; pure False
                , do lit '#'; pure True ]

data World = World { size :: Int, m :: Map Pos Bool }
type Pos = (Int,Int)

part1 :: Int -> Grid -> Int
part1 c g0 = sizeWorld $ head $ drop c $ iterate step (toWorld g0)

part2 :: Int -> Grid -> Int
part2 c g0 = sizeWorld $ head $ drop c $ iterate (sticky . step) (sticky (toWorld g0))

sticky :: World -> World
sticky w@World{size=n,m} =
  w { m = foldl (\m p -> Map.insert p True m) m
          [ (0,0), (0,n-1), (n-1,0), (n-1,n-1)] }

sizeWorld :: World -> Int
sizeWorld World {m} = length [ () | (_,b) <- Map.toList m, b ]

toWorld :: Grid -> World
toWorld (Grid bss) = World
  { size = length bss
  , m = Map.fromList [ ((x,y),b)
                     | (y,bs) <- zip [0..] bss
                     , (x,b) <- zip [0..] bs ]
  }

step :: World -> World
step World{size=n,m} = do
  let
    isOn :: Pos -> Bool
    isOn pos = maybe err id $ Map.lookup pos m
      where err = error (show pos)
  let
    f :: Pos -> Bool
    f pos = do
      let i = length [ () | pos' <- neighbors n pos, isOn pos' ]
      if isOn pos
      then i == 2 || i == 3
      else i == 3
  let m' = Map.fromList [ ((x,y), f (x,y)) | x <- [0..n-1], y <- [0..n-1] ]
  World { size = n, m = m' }

neighbors :: Int -> Pos -> [Pos]
neighbors n (x0,y0) = [ (x,y)
                      | x <- [x0-1..x0+1]
                      , x >= 0
                      , x < n
                      , y <- [y0-1..y0+1]
                      , y >= 0
                      , y < n
                      , (x,y) /= (x0,y0)
                      ]
