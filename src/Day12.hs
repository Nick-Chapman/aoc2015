module Day12 (main) where

import Misc (check)
import Par4 (Par,parse,separated,lit,alts,int,word)

main :: IO ()
main = do
  inp <-  parse gram <$> readFile "input/day12.input"
--  echo $(cat input/day12.input | tr ':,[]{}' '\n\n\n\n\n\n' | grep [0-9] | sed 's/$/ +/') 0 | bc
  print ("day12, part1", check 191164 $ part1 inp)
  print ("day12, part2", check 87842 $ part2 inp)

data J = JNum Int | JString String | JList [J] | JObject [(String,J)]

gram :: Par J
gram = json
  where
    json = alts [num,string,list,object]
    num = alts [ JNum <$> int, do lit '-'; (JNum . negate) <$> int ]
    string = JString <$> stringLit
    list = do lit '['; xs <- separated (lit ',') json; lit ']'; pure (JList xs)
    stringLit = do lit '"'; w <- word; lit '"'; pure w
    object = do lit '{'; xs <- separated (lit ',') bind; lit '}'; pure (JObject xs)
    bind = do k <- stringLit; lit ':'; v <- json; pure (k,v)

part1 :: J -> Int
part1 = eval
  where
    eval = \case
      JNum n -> n
      JString{} -> 0
      JList js -> sum (map eval js)
      JObject bs -> sum [ eval j | (_,j) <- bs ]

part2 :: J -> Int
part2 = eval
  where
    eval = \case
      JNum n -> n
      JString{} -> 0
      JList js -> sum (map eval js)
      JObject bs -> if anyRed then 0 else sum [ eval j | (_,j) <- bs ]
        where anyRed = any (\case (_,JString "red") -> True; _ -> False) bs
