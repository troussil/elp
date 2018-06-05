data CouleurCarte = Trefle | Carreau | Coeur | Pique
  deriving (Eq, Ord, Enum, Read, Show)
data ValeurCarte = Deux | Trois | Quatre | Cinq | Six | Sept |
                   Huit | Neuf | Dix | Valet | Dame | Roi | As
  deriving (Eq, Ord, Enum, Read, Show)
data Carte = Carte ValeurCarte CouleurCarte 
  deriving (Eq, Ord, Read, Show)

type Tas = [Carte]
leTas = [ Carte v c | v <- [Deux ..], c <- [Trefle ..] ]
