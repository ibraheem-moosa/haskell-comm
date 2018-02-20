import System.Environment
import System.IO
import Data.Maybe


main :: IO ()
main = do
        args <- getArgs
        let argLength = length args
        firstFileHandle <- openFile (last $ init args) ReadMode
        secondFileHandle <- openFile (last args) ReadMode
        firstFileContents <- hGetContents firstFileHandle
        secondFileContents <- hGetContents secondFileHandle
        let configuration = extractConfigurationFromArgs $ take (argLength - 2) args
        let a = elem '1' configuration 
        let b = elem '2' configuration
        let c = elem '3' configuration
        printComm '\t' (a, b, c) $ extractCommonAndUnique (lines firstFileContents) (lines secondFileContents)


extractConfigurationFromArgs :: [String] -> String
extractConfigurationFromArgs args = foldl (++) "" $ map tail $ filter (\x -> head x == '-') args


data CommonOrUnique = FirstUnique | SecondUnique | Common


printComm :: Char -> (Bool, Bool, Bool) -> [(String, CommonOrUnique)] -> IO ()

printComm _ _ [] = return ()
printComm delimiter omit@(omitFirstUnique, _, _) ((a, FirstUnique):as) = do
                                                                    if omitFirstUnique then return () else putStrLn $ (replicate 0 delimiter) ++ a
                                                                    printComm delimiter omit as
printComm delimiter omit@(_, omitSecondUnique, _) ((a, SecondUnique):as) = do
                                                                      if omitSecondUnique then return () else putStrLn $ (replicate 1 delimiter) ++ a
                                                                      printComm delimiter omit as
printComm delimiter omit@(_, _, omitCommon) ((a, Common):as) = do
                                                           if omitCommon then return () else putStrLn $ (replicate 2 delimiter) ++ a
                                                           printComm delimiter omit as


extractCommonAndUnique :: (Ord a) => [a] -> [a] -> [(a, CommonOrUnique)]

extractCommonAndUnique a [] = map (\x -> (x, FirstUnique)) a
extractCommonAndUnique [] b = map (\x -> (x, SecondUnique)) b
extractCommonAndUnique (a:as) (b:bs)
    | a < b = (a, FirstUnique):extractCommonAndUnique as (b:bs)
    | a > b = (b, SecondUnique):extractCommonAndUnique (a:as) bs
    | otherwise = (a, Common):extractCommonAndUnique as bs

