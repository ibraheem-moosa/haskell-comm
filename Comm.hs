import System.Environment
import System.IO

main :: IO ()
main = do
        fileName <- getLine
        handle <- openFile fileName ReadMode
        contents <- hGetContents handle
        putStr contents
