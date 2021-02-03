import System.IO

--1
al1 :: [Bool] -> Bool
al1 [] = True
al1 (x:xs)
    | x == False = False
    | otherwise = al1 xs

al1' :: [Bool] -> Bool
al1' [] = True
al1' (False: _) = False
al1' (_:ls) = al1' ls

al2 :: [Bool] -> Bool
al2 = all (== True)

al3 :: [Bool] -> Bool
al3 = foldl (&&) True

al4 :: [Bool] -> Bool
al4 = foldr (&&) True


--2
ala :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool
ala f d bs = foldr f d bs

ala' :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool
ala' _ b [] = b
ala' f b ls = foldl f (head ls) (tail ls)


--3
trekant :: Int -> IO ()
trekant n = putStrLn $ unlines $ map (unwords . flip replicate "*") [1..n]

trekant1 :: Int -> IO ()
trekant1 n = do
            putStr $ trekantHelper n

trekantHelper :: Int -> String
trekantHelper n
    | n < 1 = []
    | otherwise = trekantHelper (n-1) ++ (replicate n '*') ++ "\n"


--4
trekant' :: Int -> IO ()
trekant' n = putStrLn $ unlines $ map (flip center (n*2) . unwords . flip replicate "*") [1..n]

center :: String -> Int -> String
center s n = spaces ++ s ++ spaces
    where spaces = replicate ((n - length s) `div` 2) ' '


juletre :: Int -> IO ()
juletre n = do
            putStr $ juletreHjelper n 0

juletreHjelper :: Int -> Int -> String
juletreHjelper n m
    | n < 1 = []
    | otherwise = (juletreHjelper (n-1) (m+1)) ++ (replicate m ' ') ++ concat (replicate n "* ") ++ "\n"

