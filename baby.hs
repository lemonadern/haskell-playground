doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x * 2

doubleSmallNumber' x = succ $ doubleSmallNumber x

boomBangs xs = [if x < 10 then "Boom!" else "Bang!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

extractUppercase st = [x | x <- st, x `elem` ['A' .. 'Z']]
