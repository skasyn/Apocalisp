module Apocalisp where

import Text.Read
import System.IO
import System.Environment

data Expr = ApoInt Integer
          | ApoBool Bool
          | ApoSymbol String
          | ApoFn ([Expr]->Expr)
          | ApoList [Expr]
          | ApoCons (Expr, Expr)


instance Show Expr where
         show (ApoInt x) = show x
         show (ApoSymbol x) = x
         show (ApoFn x) = "<function>"
         show (ApoBool True) = show "True"
         show (ApoBool False) = show "False"
         show (ApoList x) = showApoList x
         show (ApoCons x) = showApoCons x

showApoPar :: Expr -> Expr
showApoPar (ApoList x) = ApoList [ApoSymbol "(", ApoList x, ApoSymbol ")"]
showApoPar (ApoCons x) = ApoList [ApoSymbol "(", ApoCons x, ApoSymbol ")"]
showApoPar x = x

showApoList :: [Expr] -> String
showApoList [] = ""
showApoList (ApoSymbol "(":xs) = "(" ++ showApoList xs
showApoList (ApoSymbol ")":xs) = ")"
showApoList (x:[]) = show x
showApoList (x:[ApoSymbol ")"]) = show x ++ ")"
showApoList (x:xs) = show x ++ " " ++ showApoList xs

showApoCons :: (Expr, Expr) -> String
showApoCons (x, ApoList []) = show x
showApoCons (x, ApoCons y) = show x ++ " " ++ showApoCons y
showApoCons (x, ApoList y) = show x ++ " " ++ showApoList y
showApoCons (x, y) = show x ++ " . " ++ show y

toInt :: Expr -> Integer
toInt (ApoInt x) = x

to_str :: Expr -> String
to_str (ApoSymbol x) = x
to_str x = []

exprToInt :: [Expr] -> [Integer]
exprToInt [] = []
exprToInt [(ApoList [])] = []
exprToInt ((ApoInt x):xs) = [x] ++ exprToInt xs

addInt :: [Expr] -> Expr
addInt list = (ApoInt (foldl (+) 0 (exprToInt list)))

minInt :: [Expr] -> Expr
minInt (x:[]) = (ApoInt(- (toInt x)))
minInt (x:xs) = (ApoInt (foldl (-) (toInt x) (exprToInt xs)))

multInt :: [Expr] -> Expr
multInt list = (ApoInt (foldl (*) 1 (exprToInt list)))

divInt :: [Expr] -> Expr
divInt (x:xs) = (ApoInt (foldl (div) (toInt x) (exprToInt xs)))

cons :: [Expr] -> Expr
cons [] = ApoList []
cons (x:[]) = x
cons (x:[ApoInt xs]) = ApoCons (x, ApoInt xs)
cons (x:xs) = ApoCons (x, cons xs)

car :: [Expr] -> Expr
car [] = ApoList []
car [ApoList (x:xs)] = x
car [ApoCons (x, xs)] = x

cdr :: [Expr] -> Expr
cdr [] = ApoList []
cdr [ApoList (x:xs)] = ApoList xs
cdr [ApoCons (x,xs)] = xs

quote :: [Expr] -> Expr
quote [] = ApoList []
quote (x:xs) = x

list :: [Expr] -> Expr
list [] = ApoList []
list (x:xs) = addInList x (list xs)

procs :: [(String, Expr)]
procs = [
  ("+", ApoFn addInt),
  ("-", ApoFn minInt),
  ("*", ApoFn multInt),
  ("/", ApoFn divInt),
  ("cons", ApoFn cons),
  ("car", ApoFn car),
  ("cdr", ApoFn cdr),
  ("quote", ApoFn quote),
  ("list", ApoFn list)
  ]

split :: String -> String -> [String]
split "" "" = []
split "" acc = [acc]
split (' ':cs) "" = split cs ""
split (' ':cs) acc = acc : split cs ""
split ('(':cs) "" = "(" : split cs ""
split ('(':cs) acc = acc : "(" : split cs ""
split (')':cs) "" = ")" : split cs ""
split (')':cs) acc = acc :  ")" : split cs ""
split ('\'':cs) "" = "'" : split cs ""
split ('\'':cs) acc = acc :  "'" : split cs ""
split (c:cs) "" = split cs [c]
split (c:cs) acc = split cs (acc ++ [c])

fromJust :: Maybe a -> a
fromJust (Just x) = x

addInList :: Expr -> Expr -> Expr
addInList a (ApoList list) = case list of
  [] -> (ApoList [a])
  otherwise -> (ApoList (a : list))

getSymInt :: String -> Expr
getSymInt [] = ApoList []
getSymInt x = case (readMaybe x :: Maybe Integer) of
    Nothing -> (ApoSymbol x)
    Just res -> (ApoInt res)

parseList :: [String] -> (Expr, [String])
parseList (")":xs) = (ApoList [], xs)
parseList list = (addInList expr expr2, res2)
  where (expr, res) = parser list
        (expr2, res2) = parseList res

handle_quote :: [String] -> Bool -> (Expr, [String])
handle_quote [] x = (ApoList [], [])
handle_quote (")":[]) x = (ApoSymbol ")", [])
handle_quote (")":xs) x = (ApoSymbol ")", xs)
handle_quote ("(":xs) x = (ApoSymbol ("(" ++ (to_str expr)), rest)
  where (expr, rest) = handle_quote xs True
handle_quote (x:xs) True = (ApoSymbol (x ++ " " ++ (to_str expr)), rest)
  where (expr, rest) = handle_quote xs True
handle_quote (x:xs) False = (ApoSymbol x, xs)

parser :: [String] ->   (Expr, [String])
parser [] = (ApoList [], [])
parser ("(":xs) = parseList xs
parser ("'":xs) = handle_quote xs False
--parser ("'":xs) = ((ApoList [(ApoSymbol "quote"), expr]), rest)
-- where (expr, rest) = parseList xs
parser ("#t":xs) = ((ApoBool True), xs)
parser ("#f":xs) = ((ApoBool False), xs)
parser (x:xs) = (getSymInt x, xs)

parseIt :: [String] -> Expr
parseIt [] = ApoList []
parseIt list = fst (parser list)

apply :: Expr -> [Expr] -> Expr
apply (ApoFn f) args = f args

eval :: Expr -> Expr
eval (ApoList []) = ApoList []
eval (ApoInt x) = ApoInt x
eval (ApoSymbol sym) = case lookup sym procs of
  Nothing -> ApoSymbol sym
  (Just result) -> result
eval (ApoList((ApoSymbol "'"):xs)) = ApoList xs
eval (ApoList (ApoList x:xs)) = eval (ApoList x)
eval (ApoList (x:xs)) = apply (eval x) (map eval xs)

-- Apocalisp
interpret :: String -> String
interpret x = (show (showApoPar (eval (parseIt (split x "")))))

apocalisp :: IO ()
apocalisp = do
  end <- isEOF
  if end then return ()
    else do
      input <- getLine
      putStrLn (interpret input)
      apocalisp
