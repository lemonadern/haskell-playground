doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100 then x else x * 2

doubleSmallNumber' x = succ $ doubleSmallNumber x

boomBangs xs = [if x < 10 then "Boom!" else "Bang!" | x <- xs, odd x]

-- length' xs = sum [1 | _ <- xs]

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

first (e, _, _) = e

second (_, e, _) = e

third (_, _, e) = e

head' :: [a] -> a
head' [] = error "Empty list"
head' (h : _) = h

length' :: [a] -> Integer
length' [] = 0
length' (_ : xs) = 1 + length' xs

-- sum' :: (Num a) => [a] -> a
-- sum' [] = 0
-- sum' (h : res) = h + sum' res

capital :: String -> String
capital "" = error "empty string"
capital all@(x : _) = "Name is " ++ all ++ ", head is " ++ [x]

-- bmiTell :: (RealFloat a) => a -> String
-- bmiTell bmi
--   | bmi <= 18.5 = "underweight"
--   | bmi <= 25.0 = "normal"
--   | bmi <= 30.0 = "fat"
--   | otherwise = "???"

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b = a
  | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
myCompare a b
  | a > b = GT
  | a == b = EQ
  | otherwise = LT

-- bmiTell :: (RealFloat a) => a -> a -> String
-- bmiTell weight height
--   | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"
--   | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
--   | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"
--   | otherwise = "You're a whale, congratulations!"

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny = "You're underweight, you emo, you!"
  | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= fat = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ 2
    (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials first last = [f] ++ "." ++ [l] ++ "."
  where
    (f : _) = first
    (l : _) = last

--   where
--     f = head first
--     l = head last

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where
    bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea

head'' :: [a] -> a
head'' xs = case xs of
  [] -> error "empty list."
  (x : _) -> x

describeList :: [a] -> String
describeList xs =
  "The list is " ++ case xs of
    [] -> "empty"
    [x] -> "singleton"
    xs -> "longer"

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [s] = s
maximum' (x : xs)
  | x > maxTail = x
  | otherwise = maxTail
  where
    maxTail = maximum' xs

replicate' :: (Integral n) => n -> a -> [a]
replicate' 0 _ = []
replicate' n el = el : replicate' (n - 1) el

replicate'' :: (Num n, Ord n) => n -> a -> [a]
replicate'' n x
  | n <= 0 = []
  | otherwise = x : replicate'' (n - 1) x

take' :: (Num n, Ord n) => n -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (h : xs) = reverse' xs ++ [h]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (l : lxs) (r : rxs) = (l, r) : zip' lxs rxs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' t (x : xs)
  | t == x = True
  | otherwise = elem' t xs

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
  let biggerSorted = quickSort [a | a <- xs, a >= x]
      smallerSorted = quickSort [a | a <- xs, a < x]
   in smallerSorted ++ [x] ++ biggerSorted
