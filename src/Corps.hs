module Corps where

import qualified Parametres
import qualified Data.Vector as V
import Poly
import CorpsPremier

degreeBase :: Int
degreeBase = Parametres.n

prime :: Poly CP
prime = Poly $ V.fromList $ map fromInteger $ Parametres.irreductible


newtype Galois = F (Poly CP)
        deriving (Eq)

instance Show Galois where
        show (F (Poly v)) = V.foldl' (\s n -> show n ++ s) "" v

-- le polynome des chiffres binaires d'un entier
-- au maximum s coefficients
chiffres :: Num a => Int -> Integer -> Poly a
chiffres s n = Poly $ V.generate s chiffre_en
    where chiffre_en i = fromInteger $ (n `quot` (2 ^ i)) `mod` 2

instance Num Galois where
    (+) (F x) (F y) = F (x + y)
    (*) (F x) (F y) = F $ reduce degreeBase $ modP (x * y) prime
    negate = id
    signum = undefined
    abs = undefined
    fromInteger n = F $ chiffres degreeBase n

instance Fractional Galois where
    recip (F a) = F u where (_,u,_) = algoEuclide a prime
    fromRational = undefined

instance Enum Galois where
    toEnum n = fromInteger (toEnum n :: Integer) 
    fromEnum (F (Poly v)) = V.ifoldr somme 0 v
        where somme _ (CP b) s = 2*s + (if b then 1 else 0)
