module Test exposing (..)

-- carte
type CouleurCarte = Trefle | Carreau | Coeur | Pique
type ValeurCarte = Deux | Trois | Quatre | Cinq | Six | Sept |
                   Huit | Neuf | Dix | Valet | Dame | Roi | As
type Carte = Carte ValeurCarte CouleurCarte

asTrefle = Carte As Trefle
asTous = [ asTrefle
         , Carte As Carreau
         , Carte As Coeur
         , Carte As Pique
         ]
               
-- stack
type Stack a = Empty | Push a (Stack a)

aStack = Push 'b' (Push 'a' Empty)

stackSize s = case s of
   Empty -> 0
   Push x xs -> 1 + stackSize xs

{-
Tu dois obtenir: 
> stackSize Empty
0 : number
> stackSize aStack
2 : number
-}

-- tree
type Tree a = Void | Node (Tree a) a (Tree a)
{- Note: 
Tu peux faire aussi 
type Tree a = Void | Node a (Tree a) (Tree a)
type Tree a = Void | Node (Tree a) (Tree a) a 
Void crée un arbre vide
-}

leaf1 = (Node Void 1.0 Void)
leaf2 = (Node Void 2.0 Void)
aTree = Node leaf1 5.4 leaf2
    
treeHeight : Tree a -> Int
treeHeight tree = case tree of
   Void -> 0
   Node l _ r -> 1 +
       max (treeHeight l) (treeHeight r) 

{-
Tu dois obtenir: 
> treeHeight Void
0 : Int
> treeHeight leaf1
1 : Int
> treeHeight aTree
2 : Int
-}
