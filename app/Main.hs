module Main (main) where

data Regex t =
    Nil
  | Bot
  | C t
  | App (Regex t) (Regex t)
  | Alt (Regex t) (Regex t)
  | Star (Regex t) deriving (Eq, Show)

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

nullable :: Regex t -> Bool
nullable Nil = True
nullable (Star _) = True
nullable (App a b) = nullable a && nullable b
nullable (Alt a b) = nullable a || nullable b
nullable _ = False

deriv' :: Eq t => Regex t -> t -> Regex t
deriv' Nil _ = Bot
deriv' Bot _ = Bot
deriv' (C c) x
    | c == x = Nil
    | otherwise = Bot
deriv' (App a b) c
    | nullable a = Alt (App (deriv' a c) b) (App a (deriv' b c))
    | otherwise = App (deriv' a c) b
deriv' (Star r) c = App (deriv' r c) (Star r)
deriv' (Alt a b) c = Alt (deriv' a c) (deriv' b c)

-- Normalization procedure is a bit costly, see last cases
simpl :: Eq t => Regex t -> Regex t
simpl r =
    case r of
        Nil -> Nil
        Bot -> Bot
        (C c) -> C c
        (App Bot _) -> Bot
        (App _ Bot) -> Bot
        (App Nil a) -> simpl a
        (App a Nil) -> simpl a
        (Alt a Bot) -> simpl a
        (Alt Bot a) -> simpl a
        (Star Nil) -> Nil
        (Star Bot) -> Nil
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
                            b
                     else
                        simpl (Alt (simpl a) (simpl b))
    where normal x = simpl x == x

deriv :: Eq t => Regex t -> [t] -> Regex t
deriv r (c:xs) = deriv (simpl $ deriv' r c) xs
deriv r [] = r

main :: IO ()
main = do
    putStrLn "Give alphabet"
    sigma <- getLine
    putStrLn "Give regex"
    r <- getLine
    putStrLn "Give document"
    d <- getLine
    print $ deriv (parseString sigma r Nothing) d
