module Code where
import qualified Data.Vector as V
import Prelude hiding (const)
import qualified Parametres 
import Poly
import Corps

t :: Int
t = Parametres.t

alpha :: Galois
alpha = fromInteger Parametres.racine_generatrice

generateur :: Poly Galois
generateur = foldr (\i p -> p * scinde i) (const 1) [0..(2*t-1)]
        where scinde i = polyAvecCoeffs [- (alpha ^ i), 1]

encoder :: Poly Galois -> Poly Galois
encoder p = q - (q `modP` generateur)
        where q = shift (2*t) p

decoder :: Poly Galois -> Poly Galois
decoder (Poly v) = Poly $ V.drop (2*t) v  

syndromes :: Poly Galois -> Poly Galois
syndromes p = polyAvecCoeffs $ map (\i -> evalP p (alpha ^ i)) [0..(2*t-1)]

algoEuclideEtendu :: (Fractional a, Eq a) => Poly a -> Poly a 
                -> (Poly a, Poly a)
algoEuclideEtendu a b = etape (0, 1, a, b)
    where etape (u, uu, v, vv) = 
            let q = fst (divEucl v vv)
            in if degree vv < t
                then (uu, vv)
                else etape (uu, u - q*uu, vv, v - q*vv)

sugiyama :: Poly Galois -> (Poly Galois, Poly Galois)
sugiyama syndr = (localisateur, evaluateur)
    where (localisateur, evaluateur) = algoEuclideEtendu monome syndr
          monome = shift (2*t) (const 1)

positions :: Poly Galois -> Int -> [Int]
positions localisateur n  = etape 1 0
    where beta = recip alpha
          etape x i = if i > n
            then []
            else if evalP localisateur x == 0 
                then i : (etape (x * beta) (i+1))
                else etape (x * beta) (i+1)
                                    
derivee :: Poly Galois -> Poly Galois
derivee p = polyAvecCoeffs $ map derivee_monome [1..(2*t-1)]
    where derivee_monome i = sum $ replicate i (coeff i p)

erreurs :: Poly Galois -> Poly Galois -> [Int] -> [(Int, Galois)]
erreurs localisateur evaluateur pos = zip pos (map magnitude pos)
    where p = derivee localisateur
          x i = (recip alpha) ^ i
          magnitude i = (alpha ^ i) * evalP evaluateur (x i) / evalP p (x i)

polynomeErreurs :: [(Int,Galois)] -> Int ->  Poly Galois
polynomeErreurs errs n = Poly $ (V.replicate n 0) V.// errs

corriger :: Poly Galois -> Poly Galois
corriger p = if l == 0 then p else p - e
    where n = maxDegree p
          (l,o) = sugiyama (syndromes p)
          pos = positions l n
          errs = erreurs l o pos
          e = polynomeErreurs errs n
