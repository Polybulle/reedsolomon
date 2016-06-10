module Fichiers where
-- ce module traduit des sequences d'octets en polynomes
import qualified Data.ByteString as B
import qualified Parametres
import Data.List (foldl')
import Data.Word (Word8)
import Corps
import Poly

-- Le nombre d'octet dans un bloc de donnees, ie dans un membre du corps
taille_bloc :: Int
taille_bloc = quot Parametres.n 8

-- traduit une sequence de taille convenable en element du corps
versGalois :: B.ByteString -> Galois
versGalois = toEnum . B.foldr (\w n -> 256*n + fromEnum w) 0

-- l'operation reciproque de la precedente
versOctets :: Galois -> B.ByteString
versOctets x = B.pack $ map toEnum (octets taille_bloc (fromEnum x))
    where octets 0 _ = []
          octets i m = (toEnum $ m `mod` 256) : octets (i-1) (quot m 256)
          
-- determine quelle est la taille du dernier morceaux de la sequence, qui
-- est trop petit pour etre encode directement
alignement :: B.ByteString -> Int
alignement bs = B.length bs `mod` taille_bloc

-- nombre de bloc de donnees necessaires pour representer une sequence
n_blocs :: B.ByteString -> Int
n_blocs bs = if alignement bs == 0
                then quot (B.length bs) taille_bloc 
                else quot (B.length bs) taille_bloc + 1

-- le i-eme bloc de donnee de la sequence
bloc :: B.ByteString -> Int -> Galois
bloc bs i = versGalois segment 
    where segment = if i == n_blocs bs - 1 && alignement bs /= 0
                        then aligner $ B.drop (i*taille_bloc) bs
                        else B.take taille_bloc $ B.drop (i*taille_bloc) bs
          aligner bs = B.take taille_bloc $ B.concat [bs, B.replicate taille_bloc 0]

-- transcrit une sequence d'octet en polynome
-- la taille reelle de la fin de la sequence doit etre conservee pour le
-- decodage
ouvrir :: B.ByteString -> (Poly Galois, Int)
ouvrir bs = (p, tampon)
    where p = genererPoly (n_blocs bs) (bloc bs)
          tampon = B.length bs `mod` taille_bloc
        
-- transcrit un polynome en sequence d'octet
sauvegarder :: Poly Galois -> Int -> B.ByteString
sauvegarder p align = B.take taille_fichier bs
    where taille_fichier = if align == 0 then B.length bs
                                         else B.length bs - taille_bloc + align
          bs = (B.concat morceaux)
          morceaux = map (\i -> versOctets $ coeff i p) [0..(maxdegre p)]

