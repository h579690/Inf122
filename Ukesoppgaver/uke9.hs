import System.IO
import Data.Char

-- A
brett :: Int -> IO ()

brett nR = do 
    clr
    writeTop nR
    writeRow 1 nR
    mapM_ (\i -> writeRow i nR) [1..nR]

clr = putStr "\ESC[2J" -- tømmer skjermen

writeTop nR = writeAt (lft + 1, 0)
    ((concat[(show (mod i 10))++" " | i <- [1..nR]]) ++"\n")

lft = 3

writeAt :: (Int,Int) -> String -> IO ()
writeAt (x,y) xs = do 
    goto (x,y) 
    putStr xs
    
goto :: (Int,Int) -> IO ()
goto(x,y) = putStr("\ESC["++show y++";"++show x++"H")

writeRow i nR = do
    writeAt (if i > 9 then (lft-2) else lft-1, 1+i) (show i)
    mapM_ (\i -> putStr " .") [1..nR]
    putStrLn ""


-- B
brettet :: Int -> IO ()
brettet nR = do
    brett nR
    exx nR

exx nrR = do
    goto(1, nrR+3)
    c <- getLine
    let com = words c
    if (head com == "q") then return ()
    else if (head com == "n") then do
                                    let x = ((read (head (tail com)) :: Int) + 1) * 2
                                        y = (read (last com) :: Int) + 1
                                    goto (x, y)
                                    putStr "X"
                                    exx (nrR+1)
    else if (head com == "d") then do
                                    let x = ((read (head (tail com)) :: Int) + 1) * 2
                                        y = (read (last com) :: Int) + 1
                                    goto (x, y)
                                    putStr "."
                                    exx (nrR+1)
    else do 
            print "Ukjent kommando"
            exx (nrR+2)




-- Løsningsforslag
brett1 :: Int -> IO ()
brett1 n
    | n <= 0 = error "Must be bigger than 0" --check for valid n
    | n >= 100 = error "Must be smaller than 100"
    | otherwise = do
            putStr "\ESC[2J]" -- Clearing page
            goto (0,0) -- Moving cursor to top left
            putStr $ spaces 2 -- Space for row numbers
            mapM_ (putStr . (\nb -> spaces (3 - l nb) ++ show nb)) [1..n] -- Printing col-numbers
            putStrLn "" -- Going to next line to start printing row
            mapM_ (putStr . (brettRow n)) [1..n] -- Printing the rows
        -- Oppg B
            printInstructions n-- Print instructions
            xGame n-- Start game loop


spaces :: Int -> String
spaces n = replicate n ' '


l :: Int -> Int
l = length . show


brettRow :: Int -> Int -> String
brettRow cols row = spaces (2 - l row) -- Spaces beforw one-digit numbers
                    ++ (show row) -- The row number
                    ++ concat ["  ." | _ <- [1..cols]] -- The dots with two spaces in front
                    ++ "\n"


xGame :: Int -> IO () 
xGame n  = do -- Game loop
            goto (0, n+4) --Input line below instructions and error messages
            putStr "\ESC[0J" -- clear previous input
            command <- getLine -- Get commando
            let cs = words command -- Split on spaces
            case cs of -- Check commando
                ("q":[]) -> return () -- quit
                ("n":x:y:[]) -> do -- add x
                                change x y n "X"
                                xGame n
                ("d":x:y:[]) -> do -- remove x
                                change x y n "."
                                xGame n 
                _ -> do -- unknown command
                        printError "Ukjent kommando" n
                        xGame n
    
change :: String -> String -> Int -> String -> IO() -- Put X or . in grid
change x y n str
    | and (map (all isDigit) [x,y]) = let (nx, ny) = (read x, read y) in -- Check that coorinates are numbers
                                        if (nx > n || nx < 1 || ny > n || ny < 1) -- check bounds
                                            then printError "Out of bound" n
                                        else printInGrid str nx ny n
    | otherwise = printError "Coordinates must be numbers" n


printInGrid :: String -> Int -> Int -> Int -> IO () -- Print X or . in grid
printInGrid s x y n = do
                printError "" n -- Remove Error
                goto((x*3 + 2), (y +1)) -- Move cursor
                putStr s -- Put X or .

printError :: String -> Int -> IO () -- Funksjon for å skrive ut "error linjen"
printError s n = do
        goto(0, n+3) -- Move cursor
        putStr "\ESC[0J" -- Erase previous error message
        putStr s -- Put correct error message


printInstructions :: Int -> IO ()
printInstructions n = do 
                        goto(0, n+2)
                        putStr "Enter a command: n <x> < y> ..."