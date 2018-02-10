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
        mapM_ putStrLn $ reverse common

extractCommonAndUnique list1 list2 = extractCommonAndUniqueHelper list1 list2 ([], [], [])

extractCommonAndUniqueHelper [] [] (aUnique, bUnique, common) = (aUnique, bUnique, common)
extractCommonAndUniqueHelper [] (b:bs) (aUnique, bUnique, common) = extractCommonAndUniqueHelper [] bs (aUnique, (b:bUnique), common)
extractCommonAndUniqueHelper (a:as) [] (aUnique, bUnique, common) = extractCommonAndUniqueHelper as [] ((a:aUnique), bUnique, common)
extractCommonAndUniqueHelper (a:as) (b:bs) (aUnique, bUnique, common)
    | (a < b) = extractCommonAndUniqueHelper as (b:bs) ((a:aUnique), bUnique, common)
    | (a > b) = extractCommonAndUniqueHelper (a:as) bs (aUnique, (b:bUnique), common)
    | otherwise = extractCommonAndUniqueHelper as bs (aUnique, bUnique, (a:common))
