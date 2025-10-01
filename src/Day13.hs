module Day13 (main) where

import Data.List (permutations)
import Data.Map qualified as Map
import Misc (check,nub,tail)
import Par4 (Par,parse,separated,lit,alts,int,word,key,nl)
import Prelude hiding (tail)

main :: IO ()
main = do
  sam <-  parse gram <$> readFile "input/day13.sam"
  inp <-  parse gram <$> readFile "input/day13.input"
  print ("day13, part1", check 330 $ part1 sam)
  print ("day13, part1", check 618 $ part1 inp)
  print ("day13, part2", check 601 $ part2 inp)
    where
      part1 = compute sum
      part2 = compute $ \xs -> sum xs - minimum xs

type Con = ((String,String),Int)

gram :: Par [Con]
gram = separated nl con
  where
    con = do x <- word; key " would "; f <- sign; lit ' '; n <- int
             key " happiness units by sitting next to "; y <- word; lit '.'; pure ((x,y),f n)
    sign = alts [ do key "gain"; pure id
                , do key "lose"; pure negate ]

compute :: ([Int] -> Int) -> [Con] -> Int
compute howToSum cons = maximum [ happiness order | order <- permutations names ]
  where
    names :: [String]
    names = nub [ name | ((x,y),_) <- cons, name <- [x,y] ]

    score :: (String,String) -> Int
    score (a,b) = f (a,b) + f (b,a)
      where
        f k = maybe undefined id $ Map.lookup k m
          where m = Map.fromList cons

    happiness :: [String] -> Int
    happiness xs = howToSum [ score p | p <- [(f,l)] ++ zip xs (tail xs) ]
      where (f,l) = (xs!!0,xs!!(length xs -1))
