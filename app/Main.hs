{-# LANGUAGE UnicodeSyntax, FlexibleInstances, FlexibleContexts #-}
module Main (main) where

import Debug.Trace
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

parseString :: String -> String -> Maybe (Regex Char) -> Regex Char
parseString alphabet inp = go (lexer inp)
    where go (['|']:x:xs) (Just r) =
            let alt = Alt r (parseString alphabet x Nothing) in
            go xs (Just alt)
          go ([x]:xs) Nothing
            | x `elem` alphabet = go xs . Just . C $ x
            | x == '.' = go xs . Just . dot $ alphabet
            | otherwise = error $ "Cannot parse character " ++ show x
          go ([x]:xs) (Just r)
            | x `elem` alphabet = App r . go xs . Just . C $ x
            | x == '.' = App r . go xs . Just . dot $ alphabet
            | x == '*' = go xs . Just . Star $ r
            | otherwise = error $ "Cannot parse character " ++ show x
          go (str:cs) (Just r) =
            let inner = parseString alphabet str Nothing in
            App r . go cs . Just $ inner
          go (str:cs) Nothing =
            let inner = parseString alphabet str Nothing in
            go cs . Just $ inner
          go [] (Just r) = r
          go [] Nothing = Nil

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
simpl r | trace ("simpl " ++ show r) False = undefined
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
deriv r s | trace ("d_" ++ s ++ "(" ++ show r ++ ")") False = undefined
deriv r (c:xs) = deriv (simpl $ deriv' r c) xs
deriv r [] = r

main :: IO ()
main = do
    -- putStrLn "Give alphabet"
    let sigma = "ab"
    putStrLn "Give regex"
    r <- getLine
    putStrLn "Give document"
    d <- getLine
    print $ deriv (parseString sigma r Nothing) d
