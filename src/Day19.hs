module Day19 (main) where

import Prelude hiding (seq)
import Misc (check,nub)
import ParE (Par,parseN,parse1,terminated,nl,key,word,dot,many,alts)
import Data.List (stripPrefix)

main :: IO ()
main = do
  inp <- parse1 gram <$> readFile "input/day19.input"
  --print inp
  res1 <- part1 inp
  print ("day19, part1", check 518 $ res1)
  part2 inp

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


part2 :: Puz -> IO ()
part2 Puz{start=target} = do
  print (take 20 target ++ "...")

  let res = parseN top target
  mapM_ print res
  print "done"

  where
    top :: Par (Tree,Int)
    top = do
      a <- e
      b <- rest
      pure (a,b)

    rest :: Par Int
    rest = length <$> many dot

    e = node "e" [ [h,f]
                 , [n,al]
                 , [o,mg]
                 ]

    h = node "H" [ [c,rn] --,al,ar
                 , [c,rn,f,y,f,y,ar]
                 , [c,rn,f,y,mg,ar]
                 , [c,rn,mg,y,f,ar]
--                 , [h,_ca]
                 -- +lots
                 ]

    f = node "F" [ [si] --,al
                 -- +2
                 ]

    c = base "C"
    rn = base "Rn"
    si = base "Si"
    al = base "Al"
    ar = base "Ar"
    o = base "O"
    y = base "Y"
    n = base "N"
    mg = base "Mg"
    _ca = base "Ca"

base :: String -> P
base s = do
  key s
  pure $ L s

node :: String -> [[P]] -> P
node n xss =
  alts (base n : [ seq n xs | xs <- xss ])

type P = Par Tree

data Tree = L String | T String [Tree]

instance Show Tree where
  show = \case
    L s -> s
    T n ts -> n ++ show ts

seq :: String -> [P] -> P
seq n ps = T n <$> sequence ps
