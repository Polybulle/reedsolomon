module CorpsPremier where
import qualified Parametres

taille :: Int
taille = Parametres.p

recipCP :: CP -> CP
recipCP _ = CP True

-- Un type chiffre binaire, ie Z/2Z
newtype CP = CP Bool
    deriving (Eq)

instance Show CP where
       show (CP True)  = "1"
       show (CP False) = "0"

-- l'anneau Z/2Z
instance Num CP where
        (CP a) + (CP b) = CP (a /= b)
        (CP a) * (CP b) = CP (a && b)
        negate = id
        signum _ = CP True
        abs = id
        fromInteger n = CP (n /= 0)

instance Fractional CP where
        recip = recipCP
        fromRational r = CP (r /= 0)

instance Enum CP where
        toEnum n = CP (n /= 0)
        fromEnum (CP b) = if b then 1 else 0 
