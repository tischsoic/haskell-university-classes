take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n (x : xs) = x : take' (n - 1) xs

take2 :: Int -> [a] -> [a]
take2 ys s = take'
