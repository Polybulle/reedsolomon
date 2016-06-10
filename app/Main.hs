module Main where
import System.Environment (getArgs)
import qualified Data.ByteString as B
import Fichiers
import Code

main :: IO ()
main = do 
        [action, fpin, fpout] <- take 3 <$> getArgs
        (p, align) <- ouvrir <$> B.readFile fpin
        let resultat = case action of "-e"  -> encoder p
                                      "-d"  -> decoder p
                                      "-c"  -> corriger p
                                      "-cd" -> decoder $ corriger p
        B.writeFile fpout (sauvegarder resultat align)
