data CouleurCarte = Trefle | Carreau | Coeur | Pique
data ValeurCarte = Deux | Trois | Quatre | Cinq | Six | Sept |
                   Huit | Neuf | Dix | Valet | Dame | Roi | As
data Carte = DonneCarte ValeurCarte CouleurCarte

instance Eq CouleurCarte where
  c1 == c2 = case (c1,c2) of
               (Trefle,Trefle) -> True
               (Carreau,Carreau) -> True
               (Coeur,Coeur) -> True
               (Pique,Pique) -> True
               (_,_) -> False

