toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 0 = []
toDigitsRev n =
    remainder : toDigitsRev quotient 
    where
      remainder = mod n 10 
      quotient = fromIntegral (div n 10) 

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev
