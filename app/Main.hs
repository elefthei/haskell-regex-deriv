{-# LANGUAGE UnicodeSyntax, FlexibleInstances, FlexibleContexts #-}
module Main (main) where

import System.Environment
import System.Console.GetOpt

import qualified Data.Matrix as M
import qualified Data.List as L

import qualified Antimirov as A
import Brzowski

-- CMD arguments parser
data Flag = Alphabet String | Regx String | Doc String
  deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['a']     ["alphabet"] (ReqArg Alphabet "ab")  "alphabet to use"
    , Option ['r']     ["regex"]    (ReqArg Regx "regex")   "regular expression"
    , Option ['d']     ["doc"]      (ReqArg Doc "document") "document"
    ]

parseString :: String -> String -> String
parseString alphabet (x:xs)
    | x `elem` alphabet = x:parseString alphabet xs
    | otherwise = error $ "Character " ++ show x ++ " not in alphabet " ++ show alphabet
parseString _ [] = []

dot :: String -> Regex Char
dot (c:xs)
    | null xs = C c
    | otherwise = Alt (C c) (dot xs)
dot [] = Bot

lexer :: String -> [String]
lexer (x:xs)
    | x == '(' =
        case break (')' ==) xs of
            (_, []) -> error $ "Unmatched parenthesis " ++ show (x:xs)
            (inner, _:outer) -> inner : lexer outer
    | otherwise = [x] : lexer xs
lexer [] = []

parseRegexp :: String -> String -> Maybe (Regex Char) -> Regex Char
parseRegexp alphabet inp = go (lexer inp)
    where go (['|']:x:xs) (Just r) =
            let alt = Alt r (parseRegexp alphabet x Nothing) in
            go xs (Just alt)
          go ([x]:xs) Nothing
            | x `elem` alphabet = go xs . Just . C $ x
            | x == '.' = go xs . Just . dot $ alphabet
            | otherwise = error $ "Character " ++ show x ++ " not in alphabet " ++ show alphabet
          go ([x]:xs) (Just r)
            | x `elem` alphabet = App r . go xs . Just . C $ x
            | x == '.' = App r . go xs . Just . dot $ alphabet
            | x == '*' = go xs . Just . Star $ r
            | x == '+' = go xs . Just . App r . Star $ r
            | otherwise = error $ "Character " ++ show x ++ " not in alphabet " ++ show alphabet
          go (str:cs) (Just r) =
            let inner = parseRegexp alphabet str Nothing in
            App r . go cs . Just $ inner
          go (str:cs) Nothing =
            let inner = parseRegexp alphabet str Nothing in
            go cs . Just $ inner
          go [] (Just r) = r
          go [] Nothing = Nil

usage :: String
usage = "Usage: exe --alphabet \"abcd\" --regex \"ab*\" --doc \"abbbbbb\""

-- From flags get (regex * input)
extract :: [Flag] -> (String, Regex Char, String)
extract x = case (filterMap getAlphabet x, filterMap getRegex x, filterMap getDocument x) of
                (Just a, Just b, Just c) -> (a, parseRegexp a b Nothing, parseString a c)
                (Nothing,_,_) -> error $ "Error: Missing --alphabet\n\t" ++ usage ++ "\n"
                (_,Nothing,_) -> error $ "Error: Missing --regex\n\t" ++ usage ++ "\n"
                (_,_,Nothing) -> error $ "Error: Missing --doc\n\t" ++ usage ++ "\n"
    where
        filterMap f (y:xs) = case f y of
                        Just x -> Just x
                        Nothing -> filterMap f xs
        filterMap _ [] = Nothing
        getAlphabet (Alphabet s) = Just s
        getAlphabet _ = Nothing
        getRegex (Regx s) = Just s
        getRegex _ = Nothing
        getDocument (Doc s) = Just s
        getDocument _ = Nothing

main :: IO ()
main = do
    argv <- getArgs
    (ab, r, d) <-
        case getOpt Permute options argv of
            (o,_,[]) -> return $ extract o
            (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usage options))
    -- Antimirov
    -- putStrLn "Antimirov derivative"
    -- putStrLn $ if A.nullable (A.deriv (A.toLf r) d) then "MATCH" else "NON-MATCH"
    -- Brzowski
    putStrLn "Brzowski derivative"
    putStrLn $ if nullable (deriv r d) then "MATCH" else "NON-MATCH"
    putStrLn "Closure of derivative over alphabet"
    let (clo, adj) = adjmat r ab
    let nstates = L.length clo
    print clo
    putStrLn "Adjacency matrices for each letter (indices)"
    putStrLn . unlines . map (\(a,b) -> "M(" ++ show a ++ ") = " ++ "\n" ++ show b) $ adj

    -- Compute product over d
    putStrLn $ L.foldl (\acc a -> acc ++ " * M(" ++ show a ++ ")") ("M(" ++ show d ++ ") := I") d ++ " = "
    let adjProd = L.foldl (\acc a -> let (Just m) = L.lookup a adj in acc * m) (M.identity nstates) d
    print adjProd
    putStrLn $ "Match if M(1, " ++ show nstates ++ ") = 1"
    putStrLn $ "M(1, " ++ show nstates ++ ") = " ++ show (M.unsafeGet 1 nstates adjProd)
