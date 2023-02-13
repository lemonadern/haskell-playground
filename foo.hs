factrical :: Int -> Int
factrical 0 = 1
factrical n = n * factrical (n - 1)