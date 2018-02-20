import System.Environment
import System.IO
import Data.Maybe


main :: IO ()
main = do
        args <- getArgs
        let argLength = length args
        let configuration = extractConfigurationFromArgs $ take (argLength - 2) args
        firstFileHandle <- openFile (last $ init args) ReadMode
        secondFileHandle <- openFile (last args) ReadMode
        firstFileContents <- hGetContents firstFileHandle
        secondFileContents <- hGetContents secondFileHandle
        let (firstUnique, secondUnique, common) = extractCommonAndUnique (lines firstFileContents) (lines secondFileContents)
        let a = if elem '1' configuration then [] else firstUnique 
        let b = if elem '2' configuration then [] else secondUnique 
        let c = if elem '3' configuration then [] else common
        printComm '\t' (a, b, c)


extractConfigurationFromArgs :: [String] -> String
extractConfigurationFromArgs args = foldl (++) "" $ map tail $ filter (\x -> head x == '-') args


-- safe version of head
getHead :: [a] -> Maybe a

getHead = listToMaybe


-- checks if first parameter is less than second
-- Nothing is considered greater than any Just value
lessthanWithNothing :: (Ord a) => (Maybe a) -> (Maybe a) -> (Maybe Bool)

lessthanWithNothing Nothing Nothing = Nothing
lessthanWithNothing (Just a) Nothing = Just True
lessthanWithNothing Nothing (Just b) = Just False
lessthanWithNothing (Just a) (Just b) = Just (a < b)


andWithNothing :: (Maybe Bool) -> (Maybe Bool) -> Bool

andWithNothing a b = (fromMaybe False a) && (fromMaybe False b)


minHeadOfThreeLists :: (Ord a) => ([a], [a], [a]) -> Maybe (a, Int, ([a], [a], [a]))

minHeadOfThreeLists (a, b, c)
    | andWithNothing (lessthanWithNothing ah bh) (lessthanWithNothing ah ch) = Just (head a, 0, (tail a, b, c))
    | andWithNothing (lessthanWithNothing bh ah) (lessthanWithNothing bh ch) = Just (head b, 1, (a, tail b, c))
    | andWithNothing (lessthanWithNothing ch bh) (lessthanWithNothing ch bh) = Just (head c, 2, (a, b, tail c))
    | otherwise = Nothing
    where (ah, bh, ch) = (getHead a, getHead b, getHead c)


printComm :: Char -> ([String], [String], [String]) -> IO ()

printComm delimiter lists =
        case minHeadOfThreeLists lists of Nothing -> do
                                                        return ()
                                          Just (h, i, next) -> do
                                                                  putStrLn $ (replicate i delimiter) ++ h
                                                                  printComm delimiter next


extractCommonAndUnique :: (Ord a) => [a] -> [a] -> ([a], [a], [a])

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
