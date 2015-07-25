toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 0 = []
toDigitsRev n =
    remainder : toDigitsRev quotient 
    where
      remainder = mod n 10 
      quotient = fromIntegral (div n 10) 

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:xs) = x : y * 2 : doubleEveryOther xs
doubleEveryOther a = a

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) =
    firstDigit + lastDigit + sumDigits xs
    where
      firstDigit = if div x 10 == 0 then 0 else div x 10 
      lastDigit = if mod x 10 == x then x else mod x 10
