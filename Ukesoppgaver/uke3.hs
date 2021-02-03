--A. fra boken

-- 4.5
test :: Bool -> Bool -> Bool

test x  y = if x == False then False 
            else if y == False then False 
                else True
                
{-
longConj :: Bool -> Bool -> Bool
longConj x y = if x
                then if (y
                    then True
                    else False)
                else False
-}

-- 4.7
mult :: Int -> Int -> Int -> Int
mult = \x y z -> x*y*z

mult2 :: Int -> Int -> Int -> Int
mult2 = \x -> \y -> \z -> x*y*z


-- B. fra boken

-- 5.6
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], (sum (factors x)) -x == x]

perfects2 :: Int -> [Int]
perfects2 n = [x | x <- [1..n], sum ( init (factors x)) == x]


-- 5.7
con = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]


-- 5.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]


-- C
rem1 :: Eq a => [a] -> a -> [a]
rem1 [] a = []
rem1 (x:xs) a | a == x = xs
    | otherwise = x : rem1 xs a


-- Conditional expression
rem11 :: Eq a => [a] -> a -> [a]
rem11 [] _ = []
rem11 (x:xs) a = if (x == a)
                    then xs
                    else x : (rem11 xs a)
     
     
-- Guarded expression
rem12 :: Eq a => [a] -> a -> [a]
rem12 xs a 
        | xs == [] = []
        | head xs == a = tail xs
        | otherwise = (head xs) : (rem12 (tail xs) a)


-- List comprehnsion
rem13 :: Eq a => [a] -> a -> [a]
rem13 xs a = [x | (x,n) <- zip xs [0..], x /= a || n > length (takeWhile (/=a) xs)]


-- Using where-keyword
rem14 :: Eq a => [a] -> a -> [a]
rem14 xs a = beforeA ++ (if afterA == [] then [] else (tail afterA))
            where
                beforeA = takeWhile (/=a) xs
                afterA = dropWhile (/=a) xs
                

-- D
diff :: Eq a => [a] -> [a] -> [a]
diff xs [] = xs
diff xs (a:as) = diff (rem1 xs a) as


-- Nyttige funkjsoner: foldl and foldr
diff2 :: Eq a => [a] -> [a] -> [a]
diff2 xs as = foldl rem1 xs as
--foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b


diff3 :: Eq a => [a] -> [a] -> [a]
diff3 xs as = foldr (\x y -> rem1 y x) xs as
--foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b