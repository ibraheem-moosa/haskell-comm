import System.Environment
import System.IO
import Data.Maybe
import Data.List


main :: IO ()
main = do
    args <- getArgs
    let argLength = length args
    firstFileHandle <- openFile (args !! (argLength - 2)) ReadMode
    secondFileHandle <- openFile (last args) ReadMode
    firstFileContents <- hGetContents firstFileHandle
    secondFileContents <- hGetContents secondFileHandle
    let configuration = extractConfigurationFromArgs
                            $ take (argLength - 2) args
    let a = elem '1' configuration
    let b = elem '2' configuration
    let c = elem '3' configuration
    let d = case (elemIndex 'd' configuration)
                of Nothing -> '\t'
                   Just i -> head $ args !! (i + 1)
    printComm d
        $ filter (not . (shouldOmitCommonOrUnique (a, b, c)) . snd)
              $ extractCommonAndUnique
                    (lines firstFileContents)
                    (lines secondFileContents)


extractConfigurationFromArgs :: [String] -> String
extractConfigurationFromArgs args =
    foldl (++) ""
        $ map tail
        $ filter (\x -> head x == '-') args


data CommonOrUnique = FirstUnique | SecondUnique | Common


shouldOmitCommonOrUnique :: (Bool, Bool, Bool) -> CommonOrUnique -> Bool

shouldOmitCommonOrUnique (True, _, _) FirstUnique = True
shouldOmitCommonOrUnique (_, True, _) SecondUnique = True
shouldOmitCommonOrUnique (_, _, True) Common = True
shouldOmitCommonOrUnique _ _ = False


commonOrUniqueToColumn :: CommonOrUnique -> Int

commonOrUniqueToColumn FirstUnique = 0
commonOrUniqueToColumn SecondUnique = 1
commonOrUniqueToColumn Common = 2


printComm :: Char -> [(String, CommonOrUnique)] -> IO ()

printComm delimiter =
    mapM_ (putStrLn . renderLine)
    where
        renderLine (a, commonOrUnique) =
            replicate
                (commonOrUniqueToColumn commonOrUnique)
                delimiter ++ a


extractCommonAndUnique :: (Ord a) => [a] -> [a] -> [(a, CommonOrUnique)]

extractCommonAndUnique a [] = map (\x -> (x, FirstUnique)) a
extractCommonAndUnique [] b = map (\x -> (x, SecondUnique)) b
extractCommonAndUnique (a:as) (b:bs)
    | a < b     = (a, FirstUnique):extractCommonAndUnique as (b:bs)
    | a > b     = (b, SecondUnique):extractCommonAndUnique (a:as) bs
    | otherwise = (a, Common):extractCommonAndUnique as bs

