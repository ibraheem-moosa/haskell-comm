import System.Environment
import System.IO
import Data.Maybe
import Control.Monad (forM_)

main :: IO ()
main = do
    args <- getArgs
    (flags, firstFile, secondFile) <-
        case reverse args of
            secondFile : firstFile : flags ->
                return (flags, firstFile, secondFile)
            _ ->
                error "Usage: comm -[123] firstFile secondFile"
    firstFileHandle <- openFile firstFile ReadMode
    secondFileHandle <- openFile secondFile ReadMode
    firstFileContents <- hGetContents firstFileHandle
    secondFileContents <- hGetContents secondFileHandle
    let configuration = extractConfigurationFromArgs flags
    printComm '\t' configuration
        $ extractCommonAndUnique
            (lines firstFileContents)
            (lines secondFileContents)

data Config = Config
    { configOmissions :: [CommonOrUnique]
    }

shouldOmit :: CommonOrUnique -> Config -> Bool
shouldOmit cou cfg = cou `elem` configOmissions cfg

extractConfigurationFromArgs :: [String] -> Config
extractConfigurationFromArgs args =
    Config
        { configOmissions =
            foldr addIf []
                [ ('1', FirstUnique)
                , ('2', SecondUnique)
                , ('3', Common)
                ]
        }
  where
    addIf (char, val) rest
        | char `elem` argStr = val : rest
        | otherwise     = rest
    argStr =
        concat
            $ map tail
            $ filter (\x -> head x == '-')
            $ args

data CommonOrUnique = FirstUnique | SecondUnique | Common
    deriving Eq

commOrUniqToColumn :: CommonOrUnique -> Int
commOrUniqToColumn cou =
    case cou of
        FirstUnique -> 0
        SecondUnique -> 1
        Common ->  2

printComm :: Char -> Config -> [(String, CommonOrUnique)] -> IO ()
printComm delimiter cfg xs =
    mapM_ putStrLn (map renderLine (filter (not . omitted) xs))
  where
    omitted (_, cou) = shouldOmit cou cfg
    renderLine (a, cou) = replicate (commOrUniqToColumn cou) delimiter ++ a

extractCommonAndUnique :: (Ord a) => [a] -> [a] -> [(a, CommonOrUnique)]
extractCommonAndUnique a [] = map (\x -> (x, FirstUnique)) a
extractCommonAndUnique [] b = map (\x -> (x, SecondUnique)) b
extractCommonAndUnique (a:as) (b:bs)
    | a < b     = (a, FirstUnique)  : extractCommonAndUnique as (b:bs)
    | a > b     = (b, SecondUnique) : extractCommonAndUnique (a:as) bs
    | otherwise = (a, Common)       : extractCommonAndUnique as bs
