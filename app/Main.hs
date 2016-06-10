module Main where
--  le programme en lui-meme
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Control.Monad (when)
import qualified Data.ByteString as B
import Fichiers
import Code
import Poly
import Corps

afficher_usage :: IO ()
afficher_usage = do
        putStrLn "Usage: reedsol action entree sortie"
        putStrLn "Les actions sont:"
        putStrLn "      * -e encoder un fichier"
        putStrLn "      * -d décoder un fichier encodé"
        putStrLn "      * -c corriger un fichier encodé"
        putStrLn "      * -cd corriger puis décoder un fichier encodé"
        exitWith (ExitFailure 1)

enregistrer :: FilePath -> Int -> Poly Galois -> IO ()
enregistrer fp p align = B.writeFile fp (sauvegarder align p)

main :: IO ()
main = do 
        args <- getArgs
        when (length args < 3) afficher_usage
        [action, fpin, fpout] <- take 3 <$> getArgs
        (p, align) <- ouvrir <$> B.readFile fpin
        case action of "-e"  -> enregistrer fpout align (encoder  p)
                       "-d"  -> enregistrer fpout align (decoder  p)
                       "-c"  -> enregistrer fpout align (corriger p)
                       "-cd" -> enregistrer fpout align (decoder $ corriger p)
                       _     -> afficher_usage
