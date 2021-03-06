module CorpsPremier where
-- Ce module implemente le corps premier Z/2Z

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

-- le corps Z/2Z
instance Fractional CP where
        recip = recipCP
        fromRational r = CP (r /= 0)

-- Conversion de et vers les entiers
instance Enum CP where
        toEnum n = CP (n /= 0)
        fromEnum (CP b) = if b then 1 else 0 
