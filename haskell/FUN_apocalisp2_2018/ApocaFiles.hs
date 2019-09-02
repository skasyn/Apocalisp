module ApocaFiles where
import System.IO
import Apocalisp

readContent :: Handle -> [String] -> IO ()
readContent file tab = do
    isEof <- hIsEOF file
    if isEof == True then do
        putStrLn (last tab)
        return ()
        else do
            line <- hGetLine file
            readContent file (tab ++ [(show (eval (parseIt (split line ""))))])
            return ()

openOneFile :: String -> IO ()
openOneFile path = do
    file <- openFile path ReadMode
    readContent file []
    hClose file
    return ()

evalFiles :: [String] -> Bool -> IO ()
evalFiles [] r =
    if r == True then apocalisp
    else return ()
evalFiles (x:xs) r = if x == "-i" then evalFiles xs True
    else do
        openOneFile x
        evalFiles xs r

--ApocaFiles
interpretFiles :: [String] -> IO ()
interpretFiles tab = evalFiles tab False