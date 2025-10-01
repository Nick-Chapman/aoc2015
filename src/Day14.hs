module Day14 (main) where

import Data.Map qualified as Map
import Misc (check,hist)
import Par4 (Par,parse,separated,int,word,key,nl)

main :: IO ()
main = do
  sam <-  parse gram <$> readFile "input/day14.sam"
  inp <-  parse gram <$> readFile "input/day14.input"
  print ("day14, part1 (sample)", check 1120 $ part1 1000 sam)
  print ("day14, part1", check 2660 $ part1 2503 inp)
  print ("day14, part2 (sample)", check 689 $ part2 1000 sam)
  print ("day14, part2", check 1256 $ part2 2503 inp)

data Reindeer = Reindeer Int Int Int deriving Show

gram :: Par [(String,Reindeer)]
gram = separated nl pair
  where pair = do n <- word; key " can fly "; a <- int; key " km/s for "; b <- int
                  key " seconds, but then must rest for "; c <- int; key " seconds."
                  pure $ (n, Reindeer a b c)

part1 :: Int -> [(String,Reindeer)] -> Int
part1 maxTime pairs = maximum [ howFar maxTime r | (_,r) <- pairs ]

part2 :: Int -> [(String,Reindeer)] -> Int
part2 maxTime pairs =
  maximum $ Map.elems $ hist [ w | t <- [1..maxTime], w <- inLeadAt t ]
  where
    inLeadAt :: Int -> [String]
    inLeadAt t = [ n | (n,d) <- ps, d==maxd ]
      where
        ps = [ (name, howFar t r) | (name,r) <- pairs ]
        maxd = maximum [ d | (_,d) <- ps ]

howFar :: Int -> Reindeer -> Int
howFar maxTime (Reindeer a b c) = do
  let cycleTime = b + c
  let distPerCycle = a*b
  let numCompleteCycles = maxTime `div` cycleTime
  let leftOverTime = maxTime - (numCompleteCycles * cycleTime)
  let finalSpurt = (min leftOverTime b) * a
  numCompleteCycles * distPerCycle + finalSpurt
