module Fichiers where
import qualified Data.ByteString as B
import qualified Parametres
import Data.List (foldl')
import Data.Word (Word8)
import Corps
import Poly

versGalois :: B.ByteString -> Galois
versGalois = toEnum . B.foldr (\w n -> 256*n + fromEnum w) 0

versOctets :: Galois -> B.ByteString
versOctets x = B.pack $ map toEnum (octets taille_bloc (fromEnum x))
    where octets 0 _ = []
          octets i m = (toEnum $ m `mod` 256) : octets (i-1) (quot m 256)
          
taille_bloc :: Int
taille_bloc = quot Parametres.n 8

alignement :: B.ByteString -> Int
alignement bs = B.length bs `mod` taille_bloc

n_blocs :: B.ByteString -> Int
n_blocs bs = if alignement bs == 0
                then quot (B.length bs) taille_bloc 
                else quot (B.length bs) taille_bloc + 1

bloc :: B.ByteString -> Int -> Galois
bloc bs i = versGalois segment 
    where segment = if i == n_blocs bs - 1 && alignement bs /= 0
                        then aligner $ B.drop (i*taille_bloc) bs
                        else B.take taille_bloc $ B.drop (i*taille_bloc) bs
          aligner bs = B.take taille_bloc $ B.concat [bs, B.replicate taille_bloc 0]

ouvrir :: B.ByteString -> (Poly Galois, Int)
ouvrir bs = (p, tampon)
    where p = genererPoly (n_blocs bs) (bloc bs)
          tampon = B.length bs `mod` taille_bloc
        
sauvegarder :: Poly Galois -> Int -> B.ByteString
sauvegarder p align = B.take taille_fichier bs
    where taille_fichier = if align == 0 then B.length bs
                                         else B.length bs - taille_bloc + align
          bs = (B.concat morceaux)
          morceaux = map (\i -> versOctets $ coeff i p) [0..(maxDegree p)]

