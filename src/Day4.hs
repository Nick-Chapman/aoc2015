module Day4 (main) where

import Prelude hiding (head,tail)

import Misc (check,head)
import MD5 (md5)

main :: IO ()
main = do

  print ("day4, part1 (md5-A)", check "000001dbbfa" $ take 11 $ md5 "abcdef609043")
  print ("day4, part1 (md5-B)", check "000006136ef" $ take 11 $ md5 "pqrstuv1048970")

  print ("md5 (wiki-quick)",
         check "9e107d9d372bb6826bd81d3542a419d6" $
          md5 "The quick brown fox jumps over the lazy dog")

  print ("md5 (wiki-quick-fullstop)",
         check "e4d909c290d0fb1ca068ffaddf22cbd0" $
          md5 "The quick brown fox jumps over the lazy dog.")

  print ("md5 (spec-empty)",
         check "d41d8cd98f00b204e9800998ecf8427e" $
          md5 "")

  print ("md5 (spec-a)",
         check "0cc175b9c0f1b6a831c399e269772661" $
          md5 "a")

  print ("md5 (spec-ABC)",
         check "900150983cd24fb0d6963f7d28e17f72" $
          md5 "abc")

  print ("md5 (spec-mes)",
         check "f96b697d7cb7938d525a2f31aaf161d0" $
          md5 "message digest")

  print ("md5 (spec-lower)",
         check "c3fcd3d76192e4007dfb496cca67e13b" $
          md5 "abcdefghijklmnopqrstuvwxyz")

  {-print ("md5 (spec-LONG)",
         check "d174ab98d277d9f5a5611c2c9f419d9f" $
          md5 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")-}

  --print ("day4, part1 (example-A)", check 609043 $ _part1 "abcdef")
  --print ("day4, part1 (example-B)", check 1048970 $ _part1 "pqrstuv")
  let myKey = "yzbqklnj"
  print ("day4, part1", check 282749 $ _part1 myKey)
  --print ("day4, part2", check 9962624 $ _part2 myKey)

_part1 :: String -> Int
_part1 key = head [ n | n <- [1..], fiveZeros (md5 (key ++ show n)) ]

_part2 :: String -> Int
_part2 key = head [ n | n <- [1..], sixZeros (md5 (key ++ show n)) ]

fiveZeros :: String -> Bool
fiveZeros = \case '0':'0':'0':'0':'0':_ -> True; _ -> False

sixZeros :: String -> Bool
sixZeros = \case '0':'0':'0':'0':'0':'0':_ -> True; _ -> False
