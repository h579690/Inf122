-- A
-- 7.1
reExpr f p xs = map f (filter p xs)

-- 7.4
dec2int :: [Int] -> Int
dec2int = foldl(\x y -> 10*x + y) 0

dec2int' :: [Int] Int
dec2int' ns = read (foldl func "" ns)
    where func = \ls n -> ls ++ (show n)


-- 7.5
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x,y) 

uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = \(x,y) -> f x y


-- 7.9
altmap :: (a -> b) -> (a -> b) -> [a] -> [b]
altmap f g [] = []
altmap f g [x] = [f x]
altmap f g (x:y:zs) = f x : g y : altmap f g zs

altmap1 :: (a -> b) -> (a -> b) -> [a] -> [b]
altmap1 f1 f2 ls = map func (zip ls [0..])
    where func = \(el, idx) -> if idx `mod`2 == 0
                                    then f1 el
                                    else f2 el

altmap2 :: (a -> b) -> (a -> b) -> [a] -> [b]
altmap2 f1 f2 ls = map func (pairing ls)
    where
        func [x,y] = [f1 x, f2 y]
        func [x] = [f1 x]
        
        pairing (x:y:ls) = [x,y]:pairing ls
        pairing (x:ls) = [x]:[]
        pairing [] = []
-- B
-- 8.5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add a b) = g (folde f g a) (folde f g b)


-- 8.6
eval :: Expr -> Int
eval e = folde (\x -> x) (+) e

size :: Expr -> Int
size e = folde (\x -> 1) (+) e
--size e = folde (const 1) (+) e

-- B
infiks :: Expr -> String
infiks (Val x) = show x
infiks (Add x y) = "(" ++ infiks x ++ "+" ++ infiks y ++ ")"

prefiks :: Expr -> String
prefiks (Val x) = show x
prefiks (Add x y) = "+ " ++ prefiks x ++ " " ++ prefiks y

postfiks :: Expr -> String
postfiks (Val x) = show x
postfiks (Add x y) = postfiks x ++ " " ++ postfiks y ++ " +" 