functionC x y = case x > y of
                  True  -> x
                  False -> y

ifEvenAdd2 n = case even n of
                 True  -> n + 2
                 False -> n

numbers x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1