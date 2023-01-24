import Distribution.Compat.CharParsing (CharParsing (char))

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x * 2

doubleSmallNumber' x = succ $ doubleSmallNumber x

boomBangs xs = [if x < 10 then "Boom!" else "Bang!" | x <- xs, odd x]

length' xs = sum [1 | _ <- xs]

extractUppercase :: String -> String
extractUppercase st = [x | x <- st, x `elem` ['A' .. 'Z']]

triangles = [(a, b, c) | c <- [1 .. 10], b <- [1 .. c], a <- [1 .. b], a ^ 2 + b ^ 2 == c ^ 2, a + b + c == 24]

addThree :: Int -> Int -> Int -> Int
addThree x y z = x + y + z

l = [1 .. 5]

lengthPlusThreeAsFloat = fromIntegral (length l) + 3.0

lucky :: (Integral a) => a -> String
lucky 7 = "Luckey Number 7!!!"
lucky x = "Sorry, you're out of luck..."

factorical :: (Integral a) => a -> a
factorical 0 = 1
factorical x = x * factorical (x - 1)

charName :: Char -> String
charName 'a' = "alpha"
charName 'b' = "Beta"

addVectors :: (Integral a) => (a, a) -> (a, a) -> (a, a)
-- addVectors a b = (fst a + fst b, snd a + snd b) こうするのではなく
addVectors (ax, ay) (bx, by) = (ax + bx, ay + by)