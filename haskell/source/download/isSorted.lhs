Ce programme lit les arguments, les affiche sur la sortie standard
et indique si ceux-ci sont triés ou non (ordre du dictionnaire).

Pour obtenir la liste d'arguments nous utilisons la fonction `getArgs`
définie dans le module `System.Environnement`.

> import System.Environment -- for getArgs

> main = do
>  args <- getArgs

Ensuite, nous procédons à l'affichage : 

>  putStrLn ( (show args)
>             ++ " is sorted ? "
>             ++ show (isSorted args) )

La fonction `isSorted` prend la liste d'arguments en entrée et
renvoie un booléen, indiquant si oui ou non cette liste est triée. 

> isSorted :: (Ord a) => [a] -> Bool

Elle est définie récursivement ainsi : 

> isSorted lst = case lst of
>                 [] -> True
>                 [x] -> True
>                 (x:xs@(y:ys)) -> (x <= y) && (isSorted xs)  
