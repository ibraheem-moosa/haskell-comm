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
        mapM_ putStrLn common

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
