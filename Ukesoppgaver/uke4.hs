-- A

-- Rekursjon
fjern :: String -> Char -> String
fjern [] _ = []

fjern (x:xs) c 
    | x == c = fjern xs c
    | otherwise = x : fjern xs c
    
{--
fjern (x:xs) c =
    if x == c then fjern xs c
    else x : fjern xs c
--}


-- Listekomprehensjon
fjern1 :: String -> Char -> String
fjern1 xs c = [x | x <- xs, (x /= c)]


-- B

-- Rekursjon
tegnpos :: Char -> String -> [Int]
tegnpos c xs = pos c $ zip xs [0 ..]


pos :: Eq t => t -> [(t, a)] -> [a]
pos c [] = []
pos c (x : xs)
  | c == fst x = snd x : pos c xs
  | otherwise = pos c xs
  

-- Listekomprehensjon
tegnpos1 :: Char -> String -> [Int]
tegnpos1 c xs = [ x | (y, x) <- zip xs [0..], y == c ]


-- C
intToList :: Int -> [Int]
intToList 0 = []
intToList x = intToList (x `div` 10) ++ [x `mod` 10]


-- D(a)
settSammen :: [String] -> String
settSammen [] = []
settSammen ([x]) = x
settSammen (x:xs) = x ++ " " ++ settSammen xs


-- D(b)

{--delStrengen :: String -> [String]
delStrengen [] = []
delStrengen (x : xs)
    | [x] == " " = delStrengen xs
    | otherwise = --}



-- D(c)
