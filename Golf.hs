module Golf where

skips :: [a] -> [[a]]
skips [] = []
skips [x] = [[x]]
skips xs = skips' 1 xs
  where
    skips' :: Int -> [a] -> [[a]]
    skips' n xs
      | n > length xs = []
      | otherwise     = everyNth n xs : skips' (n+1) xs

everyNth :: Int -> [a] -> [a]
everyNth 0 _ = []
everyNth 1 xs = xs
everyNth n xs = everyNth' 1 n xs
  where
    everyNth' :: Int -> Int -> [a] -> [a]
    everyNth' _ _ [] = []
    everyNth' cur n (x:xs)
      | cur == n  = x : everyNth' 1 n xs
      | otherwise = everyNth' (cur+1) n xs
