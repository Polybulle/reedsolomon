module Parametres where
-- Ce module regroupe tous les paramètres que l'utilisateur peut
-- modifier dans le programme

-- *** Parametres du corps fini GF(p^n)
-- p doit etre un nombre premier. L'implementation specialisee des corps
-- premiers utilisee dans cette version ne permet pas de changer p
p :: Int
p = 2

-- les valeurs de n testees sont  8, 16 et 24
n :: Int
n = 16

-- les coefficients d'un polynome irreductible de degre n a coefficients
-- dans Z/pZ. Ils doivent etre entres par degre du monome correspondant
-- croissant, et doivent etre les representants positifs de leurs classes les
-- plus petits.
irreductible :: [Integer]
irreductible = [1,1,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1]

-- *** Parametres du code correcteur
-- Pouvoir de correction souhaite
t :: Int
t = 2

-- La representation sous forme d'entier de la racine du polynome
-- generateur du code souhaite
-- il faut que son ordre mutliplicatif dans GF(p^n) soit plus grand que 2t
-- et n
racine_generatrice :: Integer
racine_generatrice = 3
