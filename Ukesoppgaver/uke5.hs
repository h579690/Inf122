import Data.Char

-- 1
data Ast = V Int | S String | P Ast Ast | M Ast Ast deriving Show

eval :: Ast -> Int
eval (V x) = x
eval (P a b) = (eval a) + (eval b)
eval (M a b) = (eval a) * (eval b)

eval (S x) = error ("Kan ikke evalueres, inneholder strenger")


-- 2
inn :: Ast -> String
inn (V x) = show x
inn (P a b) = (inn' a) ++ " + " ++ (inn' b)
inn (M a b) = (inn' a) ++ " * " ++ (inn' b)


inn' :: Ast -> String
inn' (V x) = show x
inn' (P a b) = "(" ++ (inn' a) ++ " + " ++ (inn' b) ++ ")"
inn' (M a b) = "(" ++ (inn' a) ++ " * " ++ (inn' b) ++ ")"


-- 3.1
tokenize :: String -> [String]
tokenize [] = []
tokenize (x:xs) 
    | elem x t = [x] : tokenize xs
    | elem x s = tokenize xs
    | otherwise = (takeWhile (notin (t++s)) (x:xs)) : tokenize (dropWhile (notin (t++s)) (x:xs))


t = "*+()"
s = " "
notin xs = \x -> not(elem x xs)



-- 3.2
parseU :: [String] -> (Ast, [String])

parseU [] = error ("Uttrykket er ikke korrekt!")
parseU ("+":xs) = 
    let (e1, r1) = parseU xs 
        (e2, r2) = parseU r1 
    in (P e1 e2, r2)
parseU ("*":xs) = 
    let (e1, r1) = parseU xs
        (e2, r2) = parseU r1 
    in (M e1 e2, r2)
    
parseU (x:xs)
    | (onlyDigits x) = (V (read x), xs)
    | otherwise = (S x, xs)

onlyDigits xs = takeWhile isDigit xs == xs

parse :: String -> Ast
parse xs = fst(parseU (tokenize xs))


-- 3.3
ev :: String -> Int
ev s = eval (parse s)


-- 3.4
innfiks :: String -> String
innfiks s = inn (parse s)