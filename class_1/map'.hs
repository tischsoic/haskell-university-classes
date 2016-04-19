map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' fn (x:xs) = fn x : map' fn xs
