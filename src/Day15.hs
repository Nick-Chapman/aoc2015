module Day15 (main) where

import Misc (check)
import Par4 (Par,parse,separated,int,word,key,nl,alts,lit)

main :: IO ()
main = do
  sam <-  parse gram <$> readFile "input/day15.sam"
  inp <-  parse gram <$> readFile "input/day15.input"
  print ("day15, part1 (sample)", check 62842880 $ part1 sam)
  print ("day15, part1", check 222870 $ part1 inp)
  print ("day15, part2 (sample)", check 57600000 $ part2 sam)
  print ("day15, part2", check 117936 $ part2 inp)
    where
      part1 = compute (\_ -> True)
      part2 = compute (\(_,_,_,_,cal) -> cal==500)

type Score = (Int,Int,Int,Int,Int)

gram :: Par [Score]
gram = separated nl line
  where
    sint = alts [int, do lit '-'; negate <$> int]
    line = do
      _n <- word
      key ": capacity "; a <- sint
      key ", durability "; b <- sint
      key ", flavor "; c <- sint
      key ", texture "; d <- sint
      key ", calories "; e <- int
      pure (a,b,c,d,e)

compute :: (Score -> Bool) -> [Score] -> Int
compute pred ps =
  maximum [ collapse score
          | qs <- split 100 (length ps)
          , let score = foldr1 add [ mul m score | (score,m) <- zip ps qs ]
          , pred score ]
  where
    mul :: Int -> Score -> Score
    mul n (a,b,c,d,e) = (n*a,n*b,n*c,n*d,n*e)

    add :: Score -> Score -> Score
    add (a,b,c,d,e) (f,g,h,i,j) = (a+f,b+g,c+h,d+i,e+j)

    collapse :: Score -> Int
    collapse (a,b,c,d,_e) = noneg a * noneg b * noneg c * noneg d
      where noneg x = max 0 x

split :: Int -> Int -> [[Int]]
split x n =
  if n < 1 then error "split" else
    if n == 1 then [[x]] else
      [ a:as | a <- [0..x], as <- split (x-a) (n-1) ]
