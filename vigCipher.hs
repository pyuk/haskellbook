module VigCipher where

vigCipher :: String -> String -> String
vigCipher xs ys =
  let convert = conv xs ys 1
      vigC [] _ = []
      vigC (a:as) (b:bs) = cipher (ord b - 97) a : vigC as bs
  in vigC xs convert

unCipher :: String -> String -> String
unCipher xs ys =
  let convert = conv xs ys 1
      vigC [] _ = []
      vigC (a:as) (b:bs) = cipher (negate $ ord b - 97) a : vigC as bs
  in vigC xs convert
