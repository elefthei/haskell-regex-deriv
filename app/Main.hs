{-# LANGUAGE UnicodeSyntax, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main (main) where

import Debug.Trace
import System.Environment
import System.Console.GetOpt

-- CMD arguments parser
data Flag = Alphabet String | Regx String | Doc String
  deriving Show

options :: [OptDescr Flag]
options =
    [ Option ['a']     ["alphabet"] (ReqArg Alphabet "ab")  "alphabet to use"
    , Option ['r']     ["regex"]    (ReqArg Regx "regex")   "regular expression"
    , Option ['d']     ["doc"]      (ReqArg Doc "document") "document"
    ]

data Regex t =
    Nil
  | Bot
  | C t
  | App (Regex Char) (Regex Char)
  | Alt (Regex Char) (Regex Char)
  | Star (Regex Char) deriving Eq

instance Show (Regex Char) where
    show r = case r of
              Nil -> "ε"
              Bot -> "∅"
              C c -> [c]
              App a b -> show a ++ show b
              Alt a b -> paren a ++ "|" ++ paren b
              Star x -> paren x ++ "*"
        where paren x = case x of
                            Nil -> show x
                            Bot -> show x
                            C _ -> show x
                            App a b -> show a ++ show b
                            _ -> "(" ++ show x ++ ")"

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

parseString :: String -> String -> String
parseString alphabet (x:xs)
    | x `elem` alphabet = x:parseString alphabet xs
    | otherwise = error $ "Character " ++ show x ++ " not in alphabet " ++ show alphabet
parseString _ [] = []

nullable :: Regex Char -> Bool
nullable Nil = True
nullable (Star _) = True
nullable (App a b) = nullable a && nullable b
nullable (Alt a b) = nullable a || nullable b
nullable _ = False

deriv' :: Regex Char -> Char -> Regex Char
deriv' Nil _ = Bot
deriv' Bot _ = Bot
deriv' (C c) x
    | c == x = Nil
    | otherwise = Bot
deriv' (App a b) c
    | nullable a = Alt (App (deriv' a c) b) (deriv' b c)
    | otherwise = App (deriv' a c) b
deriv' (Star r) c = App (deriv' r c) (Star r)
deriv' (Alt a b) c = Alt (deriv' a c) (deriv' b c)

-- Normalization procedure is a bit costly, see last cases
simpl :: Regex Char -> Regex Char
simpl r | trace ("  simpl " ++ show r) False = undefined
simpl r =
    case r of
        Nil -> Nil
        Bot -> Bot
        (C c) -> C c
        -- Bot absorbs App
        (App Bot _) -> Bot
        (App _ Bot) -> Bot
        -- Nil identity App
        (App Nil a) -> simpl a
        (App a Nil) -> simpl a
        -- Bot identity Alt
        (Alt a Bot) -> simpl a
        (Alt Bot a) -> simpl a
        -- Nil, Bot identity Star
        (Star Nil) -> Nil
        (Star Bot) -> Nil
        -- distributivity_alt_star (a+b)* = a* + b*
        (Star (Alt a b)) -> simpl $ Alt (Star a) (Star b)
        (Star a) -> if normal a then
                        Star a
                    else
                        Star (simpl a)
        (App a b) -> if normal a && normal b then
                        App a b
                     else
                        simpl (App (simpl a) (simpl b))
        (Alt a b) -> if normal a && normal b then
                        if a == b then
                            a
                        else
                            Alt a b
                     else
                        simpl (Alt (simpl a) (simpl b))
    where normal x = simpl x == x

deriv :: Regex Char -> String -> Regex Char
deriv r s | trace ("∂_" ++ s ++ "(" ++ show r ++ ")") False = undefined
deriv r (c:xs) = deriv (simpl $ deriv' r c) xs
deriv r [] = r

usage :: String
usage = "Usage: exe --alphabet \"abcd\" --regex \"ab*\" --doc \"abbbbbb\""

-- From flags get (regex * input)
extract :: [Flag] -> (Regex Char, String)
extract x = case (filterMap getAlphabet x, filterMap getRegex x, filterMap getDocument x) of
                (Just a, Just b, Just c) -> (parseRegexp a b Nothing, parseString a c)
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
    (r, d) <-
        case getOpt Permute options argv of
            (o,_,[]) -> return $ extract o
            (_,_,errs) -> ioError (userError (concat errs ++ usageInfo usage options))
    putStrLn $ if nullable (deriv r d) then "MATCH" else "NON-MATCH"
