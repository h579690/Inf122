--1.7.1
{- Give another possible calculation for the result of double (double 2).

double (double 2) 
= double (2+2) 
= 2* (2+2)
= (2+2) + (2+2) 
= 4 + 4 
= 8
-}


--1.7.2
{- Show that sum [x] = x for any number x.

sum[x] 
= sum x:[]
= x + sum[]
= x + 0
= x
-}


--1.7.3
{- Define a function product that produces the product of a list of numbers, and show using your definition that product [2,3,4]=24.

product [] = 1
product (n:ns) = n * product ns

product [2,3,4]
= 2 * (product[3,4])
= 2 * (3 * product [4])
= 2 * (3 * (4 * product [])))
= 2 * (3 * (4 * 1))
= 24

-}



--1.7.4
{-How should the definition of the function qsort be modified so that it produces a reverse sorted version of a list?

Må bytte om på smaller og larger for at listen skal sortere fra høy til lav.

-}

qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller 
        where 
            smaller = [a | a <- xs, a <= x]
            larger = [b | b <- xs, b > x]




--1.7.5
{-What would be the effect of replacing <= by < in the original definition of qsort? Hint: consider the example qsort [2,2,3,1,1].

Ved å endre fra <= til < vil ikke nye tall som er lik et tall som allerede er i listen smaller komme med i listen.



-}


--2.7.2
{- Parenthesise the following numeric expressions:
2^3*4
2*3+4*5 
2+3*4^5


(2 ^ 3) * 4
(2 * 3) + (4 * 5)
2 + (3 * (4 ^ 5))

-}

--2.7.3
{-The script below contains three syntactic errors. Correct these errors and then check that your script works properly using GHCi.


Endrer ´div´til `div`.
Variabel og funksjonsnavn skal starte med liten bokstav. Endret fra N til n.
xs må stå på linje med a.


-}

n = a `div` length xs 
    where
        a = 10
        xs = [1,2,3,4,5]
        

--2.7.4
{- The library function last selects the last element of a non-empty list; for example, last [1,2,3,4,5] = 5. Show how the function last could be defined in terms of the other library functions introduced in this chapter. Can you think of another possible definition?

-}

last1 xs = head (reverse xs) --Reverserer listen og plukker ut det første

last2 xs = xs !! ((length xs) -1) --Plukker ut tallet på plass nr lengde-1



--2.7.5
{-The library function init removes the last element from a non-empty list; for example, init [1,2,3,4,5] = [1,2,3,4]. Show how init could similarly be defined in two different ways.

-}

init1 xs = take ((length xs) -1) xs --fjerner elementet på plass lengde -1
init2 xs = reverse (drop 1 (reverse xs)) --reversever, fjerner første element og reverserer
init3 xs = reverse (tail (reverse xs)) --reverserer, plukker ut halen og reverserer



--C1

{-Input: en liste k med heltall og et heltall n
Output: listen der hvert element e fra listen k er erstattet med e+n (elementene står i samme rekkefølge som i k).

F.eks.:
plu [1,2,3] 5 = [6,7,8] 
plu [1,2,3] 0 = [1,2,3]
-}


plu:: [Int] -> Int -> [Int]
plu [] n = []
plu (k:ks) n = [k + n] ++ plu (ks) n


plu1 ns x = [n + x | n <- ns]


plu2 [] _ = []
plu2 (n:ns) x = (n+x) : (plu2 ns x)


--C2
{-
som gir True hvis inputlisten er en palindrome og False ellers.
(== er boolsk likhet, dvs. s == t gir True hvis s og t er like, og False ellers.) F.eks.:
pali “abba” = True
pali “abbac” = False 
pali [1,2,3,3,2,1] = True

-}

pali:: (Eq a) => [a] -> Bool
pali as = as == reverse as


