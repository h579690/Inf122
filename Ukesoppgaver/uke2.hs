-- B 

second xs = head (tail xs)  
second :: [a] -> a

swap (x,y) = (y,x)
swap :: (a, b) -> (b, a)

pair x y = (x, y)
pair :: a -> b -> (a,b)

double x = x * 2
double :: Num a => a -> a

palindrome xs = reverse xs == xs
palindrome :: Eq a => [a] -> Bool

twice f x = f (f x)
twice :: (a -> a) -> a -> a




 -- C 

{-
False :: Bool

5 + 8 :: Num a => a

(+) 2 :: Num a => a -> a

(+2) :: Num a => a -> a

(2+) :: Num a => a -> a

(["foo", "bar"], 'a') :: ([[Char]], Char) --([String], Char)

[(True, []), (False, [['a']])] :: [(Bool, [[Char]])]

\x y -> y !! x :: Int -> [a] -> a

[take, drop,\x y -> (y !! x)]:: Har ikke type

[take, drop,\x y -> [y !! x]]:: [Int -> [a] -> [a]]

-}



-- D 

foo1 x y = (x, y)
foo1 :: a -> b -> (a, b)

foo2 x = \y -> (x, y) 
foo2 :: a -> b -> (a, b)

foo3 = \x y -> (x, y)
foo3 :: a -> b -> (a, b)

foo4 = \x -> \y -> (x, y)
foo4 :: a -> b -> (a, b)

foo5 = \x -> \y -> (y,x)
foo5 :: b -> a -> (a, b)

foo6 = \y -> \x -> (y,x)
foo6 :: a -> b -> (a, b)

--Alle utenom foo5 er ekvivalente



-- E

f1 :: a -> (a,a)
f1 x = (x, x)

f2 :: (a,b) -> a
f2 (x, y) = x

f3 :: (a,b) -> b
f3 (x, y) = y

f4 :: a -> b -> a 
f4 x y = x

f5 :: a -> b -> b
f5 x y = y




--F. 

f :: Num t => t -> t -> t
f x y = x+y

g :: Num t => (t, t) -> t
g (x, y) = x+y


--test x y = (f x y) == (g (x, y))
