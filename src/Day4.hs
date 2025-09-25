module Day4 (main) where

import Prelude hiding (head,tail)

import Misc (check,head)
import Data.Hash.MD5 qualified as MD5

main :: IO ()
main = do
  print ("day4, part1 (md5-A)", check "000001dbbfa" $ take 11 $ md5 "abcdef609043")
  print ("day4, part1 (md5-B)", check "000006136ef" $ take 11 $ md5 "pqrstuv1048970")
  print ("day4, part1 (example-A)", check 609043 $ part1 "abcdef")
  print ("day4, part1 (example-B)", check 1048970 $ part1 "pqrstuv")
  let myKey = "yzbqklnj"
  print ("day4, part1", check 282749 $ part1 myKey)
  print ("day4, part2", check 9962624 $ part2 myKey)

part1 :: String -> Int
part1 key = head [ n | n <- [1..], fiveZeros (md5 (key ++ show n)) ]

part2 :: String -> Int
part2 key = head [ n | n <- [1..], sixZeros (md5 (key ++ show n)) ]

fiveZeros :: String -> Bool
fiveZeros = \case '0':'0':'0':'0':'0':_ -> True; _ -> False

sixZeros :: String -> Bool
sixZeros = \case '0':'0':'0':'0':'0':'0':_ -> True; _ -> False

md5 :: String -> String
md5 s = MD5.md5s (MD5.Str s)
