data CouleurCarte = Trefle | Carreau | Coeur | Pique
  deriving (Eq, Ord, Enum, Read, Show)
data ValeurCarte = Deux | Trois | Quatre | Cinq | Six | Sept |
                   Huit | Neuf | Dix | Valet | Dame | Roi | As
  deriving (Eq, Ord, Enum, Read, Show)
data Carte = Carte ValeurCarte CouleurCarte 
  deriving (Read, Show)

instance Eq Carte where
  (Carte v1 _) == (Carte v2 _) = v1 == v2
instance Ord Carte where
  (Carte v1 _) `compare` (Carte v2 _) = v1 `compare` v2

type Tas = [Carte]
leTas = [ Carte v c | v <- [Deux ..], c <- [Trefle ..] ]
