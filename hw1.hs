toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 0 = []
toDigitsRev n =
    remainder : toDigitsRev quotient 
    where
      remainder = mod n 10 
      quotient = fromIntegral (div n 10) 

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOtherLeft :: [Integer] -> [Integer]
doubleEveryOtherLeft (x:y:xs) = x : y * 2 : doubleEveryOtherLeft xs
doubleEveryOtherLeft a = a

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherLeft . reverse

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) =
    firstDigit + lastDigit + sumDigits xs
    where
      firstDigit = if div x 10 == 0 then 0 else div x 10 
      lastDigit = if mod x 10 == x then x else mod x 10

validate :: Integer -> Bool
validate = (== 0) . (\x -> mod x 10) . sumDigits . doubleEveryOther . toDigits
