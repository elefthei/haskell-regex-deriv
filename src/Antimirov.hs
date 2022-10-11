{-# LANGUAGE UnicodeSyntax, RecordWildCards, StandaloneDeriving, FlexibleInstances, FlexibleContexts #-}
module Antimirov (deriv, nullable, toLf) where

import Debug.Trace
import Data.Set (Set)
import Data.Bifunctor
import Control.Monad
import qualified Data.Set as S
import qualified Brzowski as B

-- Regex without Alt
data Regex t =
    Nil
  | Bot
  | C t
  | App (Regex t) (Regex t)
  | Star (Regex t) deriving Eq

deriving instance Ord t => Ord (Regex t)

-- Linear forms without nil <c, regex>
type LF t = Set (t, Regex t)

-- Regular expression non-determinism as set
data RegSet t = RegSet { is_nil :: Bool, lf :: LF t } deriving Eq

instance Show (Regex Char) where
    show r = case r of
              Nil -> "ε"
              Bot -> "∅"
              C c -> [c]
              App a b -> paren a ++ paren b
              Star x -> paren x ++ "*"
        where paren x = case x of
                            Nil -> show x
                            Bot -> show x
                            C c -> [c]
                            App a b -> show a ++ show b
                            _ -> "(" ++ show x ++ ")"

instance Show (RegSet Char) where
    show RegSet { .. }
        | is_nil = "ε + " ++ showLf lf
        | otherwise = showLf lf
        where showLf s = case S.toList s of
                [] -> ""
                ((c, r):xs) -> [c] ++ "·" ++ show r


nullable' :: Regex t -> Bool
nullable' Nil = True
nullable' Bot = False
nullable' (C _) = False
nullable' (Star _) = True
nullable' (App a b) = nullable' a && nullable' b

bot :: RegSet t
bot = RegSet { is_nil = False, lf = S.empty }

nil :: RegSet t
nil = RegSet { is_nil = True, lf = S.empty }

monomial :: Char -> Regex Char -> RegSet Char
monomial c r = RegSet False $ S.singleton (c, r)

monomial' :: Bool -> Char -> Regex Char -> RegSet Char
monomial' b c r = RegSet b $ S.singleton (c, r)


-- Smart app constructor that simplifies nils and bottoms
simpApp :: Regex Char -> Regex Char -> Regex Char
simpApp Bot c = Bot
simpApp c Bot = Bot
simpApp Nil c = c
simpApp c Nil = c
simpApp a b = App a b

-- Symbol ⊙ in Antimirov's paper
cons :: RegSet Char -> Regex Char -> RegSet Char
cons _ Bot = bot
cons RegSet { .. } r = RegSet (is_nil && nullable' r) $ lcons lf r
    where
       lcons _ Bot = S.empty
       lcons l Nil = l
       lcons l t
           | S.null l = S.empty
           | otherwise = S.map (second (`simpApp` t)) l

-- Extend `cons` to `append` for linear forms.
-- Elementwise join is sort of complex? Unless sets are sorted
app :: RegSet Char -> RegSet Char -> RegSet Char
app l r | trace ("\t app " ++ show l ++ " ++ " ++ show r) False = undefined
app (RegSet b l) (RegSet b' l')
    | b && b'   = RegSet True  $ join' l l' `S.union` l `S.union` l'
    | b         = RegSet False $ join' l l' `S.union` l'
    | b'        = RegSet False $ join' l l' `S.union` l
    | otherwise = RegSet False $ join' l l'
    where join' l r = traceShow "join'(l,r) = " $ S.fromList $ S.toList l `join` S.toList r
          join ls rs = do
            (l, lv) <- ls
            (r, rv) <- rs
            guard (r == l)
            return (l, simpApp lv rv)

union :: RegSet Char -> RegSet Char -> RegSet Char
union (RegSet b l) (RegSet b' l') = RegSet (b || b') (l `S.union` l')

-- Convert Brzowski derivatives (with Alt) to RegSets
toLf :: B.Regex Char -> RegSet Char
toLf r | trace ("\t Converting " ++ show r) False = undefined
toLf B.Bot = bot
toLf B.Nil = nil
toLf (B.C x) = monomial x Nil
toLf (B.Alt r t) = toLf r `union` toLf t
toLf (B.Star r) = traceShow "toLF (Star r) =" . RegSet True . S.map (second Star) . lf . toLf $ r
toLf (B.App r t) = traceShow "toLf (App r t) = " $ toLf r `app` toLf t

rderiv :: Regex Char -> Char -> RegSet Char
rderiv Nil _ = bot
rderiv Bot _ = bot
rderiv (C c) x
    | c == x = nil
    | otherwise = bot
rderiv (App a b) x
    | nullable' a = rderiv a x `cons` b `union` rderiv b x
    | otherwise = rderiv a x `cons` b
rderiv (Star r) x = rderiv r x `cons` Star r

-- Return the first char of this regex
firstChar :: Regex Char -> Maybe Char
firstChar Bot = Nothing
firstChar Nil = Nothing
firstChar (C c) = Just c
firstChar (App a b) = case firstChar a of
                        Nothing -> firstChar b
                        found -> found
firstChar (Star r) = firstChar r

-- Antimirov derivative of a single monomial
mderiv :: (Char, Regex Char) -> Char -> RegSet Char
mderiv (c, Nil)  _ = bot
mderiv (c, Bot)  _ = bot
mderiv (c, C c') x
    | c == x = monomial c' Nil
    | otherwise = bot
mderiv (c, r) x
    | c == x = case firstChar r of
                 Just c -> monomial' (nullable' r) c Nil `app` rderiv r c
                 Nothing -> RegSet (nullable' r) S.empty
    | otherwise = bot

nullable :: RegSet Char -> Bool
nullable = is_nil

deriv' :: RegSet Char -> Char -> RegSet Char
deriv' r c
    | is_nil r = S.foldl (\rs m -> rs `union` mderiv m c) nil . lf $ r
    | otherwise = S.foldl (\rs m -> rs `union` mderiv m c) bot . lf $ r

deriv :: RegSet Char -> String -> RegSet Char
deriv r s | trace ("∂_" ++ s ++ "(" ++ show r ++ ")") False = undefined
deriv r s = foldl deriv' r s
