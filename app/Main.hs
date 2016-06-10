module Main where
import System.Environment (getArgs)
import qualified Data.ByteString as B
import Fichiers
import Code

main :: IO ()
main = do 
        [action, fpin, fpout] <- take 3 <$> getArgs
        (p, align) <- ouvrir <$> B.readFile fpin
        let resultat = case action of "-e"  -> encode p
                                      "-d"  -> decode p
                                      "-c"  -> correction p
                                      "-cd" -> decode $ correction p
        B.writeFile fpout (sauvegarder resultat align)
