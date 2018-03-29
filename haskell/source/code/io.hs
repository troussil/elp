import System.Random -- for randomIO

data CouleurCarte = Trefle | Carreau | Coeur | Pique
  deriving (Eq, Ord, Enum, Read, Show)
data ValeurCarte = Deux | Trois | Quatre | Cinq | Six | Sept |
                   Huit | Neuf | Dix | Valet | Dame | Roi | As
  deriving (Eq, Ord, Enum, Read, Show)
data Carte = Carte ValeurCarte CouleurCarte 
  deriving (Read, Show)

----------------------------------------------------------------

estLaMeme :: Carte -> Carte -> Bool
estLaMeme (Carte v c) (Carte v' c')
  | ((v == v') && (c == c')) = True
  | otherwise = False

jeu :: Carte -> IO ()
jeu carteATrouver =
  do
    line <- getLine
    if (read line) `estLaMeme` carteATrouver
      then putStrLn "Vous avez gagne!"
      else do
        putStrLn "Non, reessayez!"
        jeu carteATrouver

main = do
    -- choix aleatoire d'une carte
    c <- randomIO :: IO Int -- import System.Random 
    v <- randomIO :: IO Int -- import System.Random
     let (c', v') = (c `mod` (fromEnum Pique), v `mod` (fromEnum As)) 
    let laCarte = Carte (toEnum v' :: ValeurCarte)
                        (toEnum c' :: CouleurCarte)
    putStrLn ("indice: " ++ (show laCarte))
    -- jeu de devinette
    jeu laCarte
    
