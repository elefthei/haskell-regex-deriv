{-# LANGUAGE StandaloneDeriving, UnicodeSyntax, FlexibleInstances, FlexibleContexts #-}
module Brzowski (deriv, Regex(..), nullable, adjmat) where

import Debug.Trace

import Data.Set (Set)
import Data.Matrix (Matrix)

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Matrix as M

data Regex t =
    Nil
  | Bot
  | C t
  | App (Regex Char) (Regex Char)
  | Alt (Regex Char) (Regex Char)
  | Star (Regex Char) deriving Eq

deriving instance Ord t => Ord (Regex t)

instance Show (Regex Char) where
    show r = case r of
              Nil -> "ε"
              Bot -> "∅"
              C c -> [c]
              App a b -> paren a ++ paren b
              Alt a b -> paren a ++ "|" ++ paren b
              Star x -> paren x ++ "*"
        where paren x = case x of
                            Nil -> show x
                            Bot -> show x
                            C _ -> show x
                            App a b -> show a ++ show b
                            _ -> "(" ++ show x ++ ")"

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
-- simpl r | trace ("  simpl " ++ show r) False = undefined
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
                        sort App a b
                     else
                        simpl (sort App (simpl a) (simpl b))
        (Alt a b) -> if normal a && normal b then
                        if a == b then
                            a
                        else
                            sort Alt a b
                     else
                        simpl (sort Alt (simpl a) (simpl b))
    where normal x = simpl x == x
          sort f a b
            | a > b = f a b
            | otherwise = f b a

-- Find the transitive closure of derivatives over alphabet
dclosure :: Regex Char -> [Char] -> Set (Regex Char) -> Set (Regex Char)
dclosure r alphabet s = L.foldl (\acc c ->
    let dr = simpl (deriv' r c) in
    if S.member dr acc then
        acc
    else
        acc `S.union` dclosure dr alphabet (S.insert dr acc)
    ) (S.insert r s) alphabet

adjmat :: Regex Char -> [Char] -> ([Regex Char], [(Char, Matrix Int)])
adjmat r alphabet =
    -- Derivative closure associative list
    let dcas = L.reverse (S.toList (dclosure r alphabet S.empty)) in
    (dcas, do
        c <- alphabet
        return (c, M.matrix
            (L.length dcas)
            (L.length dcas)
            (\(i,j) -> let r = dcas !! (i-1) in
                       let r' = dcas !! (j-1) in
                       if simpl (deriv' r c) == r' then
                           1
                       else
                           0)))

deriv :: Regex Char -> String -> Regex Char
deriv r s | trace ("∂_" ++ s ++ "(" ++ show r ++ ")") False = undefined
deriv r (c:xs) = deriv (simpl $ deriv' r c) xs
deriv r [] = r
