module Poly where
-- ce module implemente un type generique representant des polynomes 
-- sur des corps arbitraires

import Data.Vector hiding ((++), all)
import Prelude hiding (replicate, length, map, const, foldr)

-- Un polynome sur un corps arbitraire
-- La structure est immuable
data Poly a = Poly {toVector :: !(Vector a)}

instance (Num a, Eq a) => Eq (Poly a) where
    (==) p q = let m = max (maxdegre p) (maxdegre q) in
               all (\i -> coeff i p == coeff i q) [0..m]

-- Les polynomes sont affiches sans leurs variables, petit-boutiste
instance Show a => Show (Poly a) where
        show (Poly v) = foldl' (\s n -> show n ++  " " ++ s) "" v

-- les Polynomes forment un anneau
instance (Num a, Eq a) => Num (Poly a) where
        (+) = somme
        (*) = multPoly 
        negate p = Poly $ map negate (toVector p)
        abs = undefined
        signum = undefined
        fromInteger = const . fromInteger


-- ********* Primitives non mathematiques *************

-- cree un polynome manuellement a partir d'une liste
-- l'element d'indice i devient le coefficient de degre i
polyAvecCoeffs :: [a] -> Poly a
polyAvecCoeffs l = Poly $ fromList l

-- genere un polynome en fonction des degres de chaque coefficient
genererPoly :: Int -> (Int -> a) -> Poly a
genererPoly n coefficients = Poly $ generate n coefficients

-- le degre maximal possible pour un polynome
maxdegre:: Poly a -> Int
maxdegre p = (length (toVector p)) -1

--  le degre reel d'un polynome
degre :: (Num a, Eq a) => Poly a -> Int
degre poly = aux poly (maxdegre poly)
    where aux _ (-1) = -1
          aux p n    = if (coeff n p) /= 0 then n else aux p (n-1)

-- le coefficient de degre n
coeff :: Num a => Int -> Poly a -> a
coeff n p = if n > maxdegre p then 0 else (toVector p) ! n

-- le polynome constant de valeur x 
const :: a -> Poly a
const x = polyAvecCoeffs [x]

-- tronquer un polynome pour ne garder que les n monomes de degres minimaux
reduire :: Int -> Poly a -> Poly a
reduire n (Poly v) = Poly $ force $ slice 0 n v




-- ********* Primitives algebriques *************

-- Somme de deux polynomes
somme :: Num a => Poly a -> Poly a -> Poly a
somme p q = Poly $ generate (d+1) (\i -> (coeff i p) + (coeff i q))
        where d = max (maxdegre p) (maxdegre q)

-- renvoie p*X^k + q
sommeDecale :: Num a => Int -> Poly a -> Poly a -> Poly a
sommeDecale n p q = 
    let m = max ((maxdegre p) + n) (maxdegre q)
        calculer_coeff i = if i < n then (coeff i q) else (coeff (i-n) p) + (coeff i q)
    in Poly $ generate (m + 1) calculer_coeff

-- renvoie p*X^k
shift :: (Eq a, Num a) => Int -> Poly a -> Poly a
shift k p = 
    let m = max ((degre p) + k) (maxdegre p) in
    Poly $ generate (m + 1) (\i -> if i < k then 0 else coeff (i-k) p)

-- multiplication par un scalaire
scale :: Num a => a -> Poly a -> Poly a
scale l (Poly v) = Poly $ map (* l) v

-- produit de deux polynomes de meme taille
multPoly :: (Num a, Eq a) => Poly a -> Poly a -> Poly a
multPoly p q = ifoldl step z0 (toVector p)
    where z0 = shift (2 * maxdegre p) (const 0)
          step z i coeff_x = 
                if coeff_x == 0 then z
                else sommeDecale i (scale coeff_x q) z

-- valeur d'un polynome en un point
evalP :: (Eq a, Fractional a) => Poly a -> a -> a
evalP p x = fst $ foldl' etape (0, 1) (toVector p)
        where etape (val, x_i) coeff_i= (val + coeff_i * x_i, x_i * x)





-- ********* Primitives arithmetiques *************

-- Algorithme classique pour la division euclidenne
-- d doit etre non nul 
divEucl :: (Fractional a, Eq a) => Poly a -> Poly a -> (Poly a, Poly a)
divEucl num d = step ((const 0), num) (degre num)
    where step (q, r) deg_r = 
            if deg_r >= degre d 
                then let t = (coeff deg_r r) / (coeff (degre d) d)
                         qq = (shift 1 q) + (const t)
                         rr = r - (shift (deg_r - degre d) (scale t d))
                    in step (qq, rr) (deg_r - 1)
                else (q, r)

-- modulo un certain polynome
modP :: (Fractional a, Eq a) => Poly a -> Poly a -> Poly a
modP poly base = snd $ divEucl poly base

-- Renvoie (r,u,v) tels que r = pgcd(a,b) et a*u + b*v = r
algoEuclide :: (Fractional a, Eq a) => Poly a -> Poly a 
                -> (Poly a, Poly a, Poly a)
algoEuclide a b = etape (a, const 1, const 0, b, const 0, const 1)
    where etape (r, u, v, rr, uu, vv) = 
            let q = fst (divEucl r rr)
            in if rr == const 0 
                then (r, u, v)
                else etape (rr, uu, vv, r - q*rr, u - q*uu, v - q*vv)
