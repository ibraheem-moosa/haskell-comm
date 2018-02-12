import System.Environment
import System.IO

main :: IO ()
main = do
        args <- getArgs
        firstFileHandle <- openFile (args !! 0) ReadMode
        secondFileHandle <- openFile (args !! 1) ReadMode
        firstFileContents <- hGetContents firstFileHandle
        secondFileContents <- hGetContents secondFileHandle
        let (firstUnique, secondUnique, common) = extractCommonAndUnique (lines firstFileContents) (lines secondFileContents)
--        rs <- sequence $ map putStrLn $ reverse $ common
--        print rs
--        mapM_ putStrLn common
        printComm (firstUnique, secondUnique, common) 

printComm :: ([String], [String], [String]) -> IO ()

printComm ([], [], []) =
    return ()

printComm (a:as, [], []) =
    do
        putStrLn a
        printComm (as, [], [])

printComm ([], b:bs, []) =
    do
        putStrLn $ "\t\t" ++ b
        printComm ([], bs, [])

printComm ([], [], c:cs) =
    do
        putStrLn $ "\t\t\t\t" ++ c
        printComm ([], [], cs)

printComm (a:as, b:bs, [])
    | (a < b) =
        do
            putStrLn $ a
            printComm (as, b:bs, [])
    | otherwise =
        do
            putStrLn $ "\t\t" ++ b
            printComm (a:as, bs, [])
        

printComm (a:as, [], c:cs)
    | (a < c) =
        do
            putStrLn $ a
            printComm (as, [], c:cs)
    | otherwise =
        do
            putStrLn $ "\t\t\t\t" ++ c
            printComm (a:as, [], cs)

printComm ([], b:bs, c:cs)
    | (b < c) =
        do
            putStrLn $ "\t\t" ++ b
            printComm ([], bs, c:cs)
    | otherwise =
        do
            putStrLn $ "\t\t\t\t" ++ c
            printComm ([], b:bs, cs)

printComm (a:as, b:bs, c:cs)
    | (&&) (a < b) (b < c) = 
        do
            putStrLn $ a
            printComm (as, b:bs, c:cs)
    | (&&) (b < c) (c < a) =
        do
            putStrLn $ "\t\t" ++ b
            printComm (a:as, bs, c:cs)
    | otherwise =
        do
            putStrLn $ "\t\t\t\t" ++ c
            printComm (a:as, b:bs, cs)

extractCommonAndUnique a [] = (a, [], [])
extractCommonAndUnique [] b = ([], b, [])
extractCommonAndUnique (a:as) (b:bs)
    | a < b = ((a:aRest1), bRest1, cRest1)
    | a > b = (aRest2, (b:bRest2), cRest2)
    | otherwise = (aRest3, bRest3, (a:cRest3))
    where 
        (aRest1, bRest1, cRest1) = extractCommonAndUnique as (b:bs)
        (aRest2, bRest2, cRest2) = extractCommonAndUnique (a:as) bs
        (aRest3, bRest3, cRest3) = extractCommonAndUnique as bs
{-
extractCommonAndUnique list1 list2 = extractCommonAndUniqueHelper list1 list2 ([], [], [])

extractCommonAndUniqueHelper [] [] result = result
extractCommonAndUniqueHelper [] (b:bs) (aUnique, bUnique, common) = extractCommonAndUniqueHelper [] bs (aUnique, (b:bUnique), common)
extractCommonAndUniqueHelper (a:as) [] (aUnique, bUnique, common) = extractCommonAndUniqueHelper as [] ((a:aUnique), bUnique, common)
extractCommonAndUniqueHelper (a:as) (b:bs) (aUnique, bUnique, common)
    | (a < b) = extractCommonAndUniqueHelper as (b:bs) ((a:aUnique), bUnique, common)
    | (a > b) = extractCommonAndUniqueHelper (a:as) bs (aUnique, (b:bUnique), common)
    | otherwise = extractCommonAndUniqueHelper as bs (aUnique, bUnique, (a:common))
-}
