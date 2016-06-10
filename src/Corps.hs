module Corps where
-- Ce module implemente un corps fini

import qualified Parametres
import qualified Data.Vector as V
import Poly
import CorpsPremier

-- le type "corps fini", est implemente comme une classe d'équivalence sur les
-- polynomes a coefficients dans Z/2Z
newtype Galois = F (Poly CP)
        deriving (Eq)

-- Le degre du polynome sur lequel on base le corps
degreeBase :: Int
degreeBase = Parametres.n

-- Les membres du corps sont des classes modulo ce polynome
irred :: Poly CP
irred = Poly $ V.fromList $ map fromInteger $ Parametres.irreductible

instance Show Galois where
        show (F (Poly v)) = V.foldl' (\s n -> show n ++ s) "" v

-- le polynome des chiffres binaires d'un entier
-- au maximum s coefficients
chiffres :: Num a => Int -> Integer -> Poly a
chiffres s n = Poly $ V.generate s chiffre_en
    where chiffre_en i = fromInteger $ (n `quot` (2 ^ i)) `mod` 2

instance Num Galois where
    (+) (F x) (F y) = F (x + y)
    (*) (F x) (F y) = F $ reduire degreeBase $ modP (x * y) irred
    negate = id
    signum = undefined
    abs = undefined
    fromInteger n = F $ chiffres degreeBase n

instance Fractional Galois where
    recip (F a) = F u where (_,u,_) = algoEuclide a irred
    fromRational = undefined

instance Enum Galois where
    toEnum n = fromInteger (toEnum n :: Integer) 
    fromEnum (F (Poly v)) = V.ifoldr somme 0 v
        where somme _ (CP b) s = 2*s + (if b then 1 else 0)
