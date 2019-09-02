import System.IO
import System.Environment
import Apocalisp
import ApocaFiles

apocafiles :: [String] -> IO ()
apocafiles [] = putStrLn "Fatal error. No args were passed."
apocafiles tab = do
    interpretFiles tab
    return ()


main :: IO ()
main = do
  args <- getArgs
  if null args then apocalisp
    else apocafiles args