module Day19 (main) where

import Prelude hiding (seq)
import Misc (check,nub)
import Par4 (Par,parse,terminated,nl,key,word)
import Data.List (stripPrefix)

main :: IO ()
main = do
  inp <- parse gram <$> readFile "input/day19.input"
  res1 <- part1 inp
  print ("day19, part1", check 518 $ res1)

part1 :: Puz -> IO Int
part1 Puz{reps,start} = do
  let xs = nub $ [ end | rep <- reps, end <- allReps rep start ]
  pure $ length xs

allReps :: Rep -> String -> [String]
allReps rep@(Rep a b) = \case
  [] -> []
  s@(x:s') -> do
    case stripPrefix a s of
      Nothing -> more
      Just t -> (b++t) : more
    where
      more = [ x:xs | xs <- allReps rep s' ]

data Puz = Puz { reps :: [Rep], start :: String } deriving Show
data Rep = Rep String String deriving Show

gram :: Par Puz
gram = do
  reps <- terminated nl rep
  nl
  start <- word
  pure Puz { reps, start }
  where
    rep :: Par Rep
    rep = do
      a <- word
      key " => "
      b <- word
      pure $ Rep a b
