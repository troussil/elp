data CouleurCarte = Trefle | Carreau | Coeur | Pique
  deriving (Eq, Ord, Enum, Read, Show)
data ValeurCarte = Deux | Trois | Quatre | Cinq | Six | Sept |
                   Huit | Neuf | Dix | Valet | Dame | Roi | As
  deriving (Eq, Ord, Enum, Read, Show)
data Carte = Carte ValeurCarte CouleurCarte 
  deriving (Eq, Read, Show)

----------------------------------------------------------------

jeu :: Carte -> IO ()
jeu carteATrouver =
  do
    line <- getLine
    if (read line) == carteATrouver
      then putStrLn "Vous avez gagne!"
      else do
        putStrLn "Non, reessayez!"
        jeu carteATrouver

main = do
    let laCarte = Carte As Trefle
    putStr "indice: "
    print laCarte
    -- jeu de devinette
    jeu laCarte
