-- import Slides exposing (md, mdFragments, slidesDefaultOptions)
-- import Slides.Styles
-- import Slides.SlideAnimation
-- import List

-- import Css exposing (..)
-- import Css.Global exposing (img)


-- main = Slides.app

--     { slidesDefaultOptions
--         | style = List.append
--              [ img [ maxWidth (px 500) ]
--              ]
--              <| Slides.Styles.elmMinimalist (hex "#fff") (hex "#ccc") (px 16) (hex "#000")
--         , slideAnimator = Slides.SlideAnimation.scroll
--     }

-- script ! src "/assets/highlight/highlight.pack.js" $ ""

module Main exposing (..)
import Slides exposing (..)
import Slides.SlideAnimation

main = Slides.app

    { slidesDefaultOptions
        | slideAnimator = Slides.SlideAnimation.scroll
    }

    -- , mdFragments
    --     [ "Another slide with three fragments"
    --     , "This appears later"
    --     , "And this even later"
    --     ]
    -- ]

    [ md
 """
  # (H).E.L.P: Haskell, ELM
  
  """
    , md
 """ 
  # Pourquoi des langages fonctionnels ?

  - La programmation fonctionnelle, ancienne, explose de nouveau dans le web. 
  - Les idées mises en oeuvre font leur chemin aussi dans des langages généralistes.

  """
    , md
 """
  # Pourquoi Haskell et ELM ?

  - Haskell a des caractéristiques peu communes. 
  - Les connaitre permet de mieux saisir les autres langages.
  - ELM est inspiré de Haskell, mais plus simple. 
  - On fait une appli web monopage très rapidemment.  
  - Tous deux sont formateurs, car on ne peut faire autrement que coder dans un style fonctionnel. 
  
  """
    , md
 """ 
 # Plan
  
  1. Haskell
  2. ELM
  3. Travail à faire
  
  """
    , md
 """
  ![Logo Haskell](../static/images/haskell-logo.svg)
  
  - Haskell a été créé en 1990 par un ensemble d'universitaires.
  - Il est défini dans un document appelé *Haskell Report*, dont la dernière révision date de 2010.
  - C'est un langage turing-complet, en général compilé, et *fonctionnel*. 

  """
    , md

 """
  # Haskell, un langage fonctionnel

  """
    , md
  """
   ## Valeur

   - Une valeur est une entité abstraite comme 
   `5, 'a', [1,2,3], ('b',4), x, inc, (+)` 
   - Une fonction est une valeur. Elle est définie
     - par une fonction anonyme : 
   `inc = \\n -> n+1`
     - ou sous forme équationnelle : 
   `inc n = n+1` 
   - Un appel est l'appariemment du nom d'une fonction et de ses arguments : `inc 3`
   """
   , md
 """
  ## Expression et évaluation

  - Une expression est une combinaison de valeurs et d'appels comme
  `inc (inc (3+1))` 
  - Toutes les actions sont faites par l'évaluation d'expressions menant à des valeurs.
  - Le point d'entrée d'un programme est l'évaluation de `main` : 

  ```haskell
  inc n = n + 1
  main = print (inc (inc (3+1)))
  ```

  """
    , md
 """ 
  # Les caractéristiques d'Haskell

  - pureté (ou encore transparence)
  - évaluation paresseuse
  - typage statique

  """
    , md
 """
  ## Pureté

  - Une expression est toujours évaluée en la même valeur.
  - Permet de raisonner sur les programmes comme en algèbre : 
  ex. `inc 3` sera toujours évalué en `3+1`. 

  - Les *effets de bord* (modification d'une variable globale, entrées-sorties, etc.) 
  sont explicitement encapsulés dans les arguments et/ou la valeur de retour.  
  

  """
    , md
 """
  ## Evaluation paresseuse

  Les expressions ne sont pas évaluées tant que leur valeur n'est pas requise : 

  `inc n = n+1`: 
  `inc (3 + 1)` donne `(3 + 1) + 1` (puis `5`), mais pas `inc 4`.

  `f x y = x`:
  `f 2 35^532` et `f 2 (1/0)` sont toutes deux évaluées à `2` sans erreur.  

  """
    , md
 """
  ### Conséquence 1

  Il est possible de définir ses propres structures de contrôle. 

  Définition : 
  ```haskell
  si True valSiVrai valSiFaux = valSiVrai
  si False valSiVrai valSiFaux = valSiFaux
  ```

  Appel : 
  ```haskell
  si (x >= 18) (print "yes") (print "no")
  ```
  
  Un seul affichage sera effectué (au lieu de deux dans le cas d'une évaluation stricte)

  """
    , md
 """
  ### Conséquence 2

  Il est possible de manipuler des structures de données infinies. 

  Definition : 

  ```haskell
  numbersFrom x = x : (numbersFrom (x+1)) 
  ```

  - L'appel `numbersFrom 1` ne termine pas, 
  - mais l'appel `take 5 (numbersFrom 1)` termine et renvoie `[1,2,3,4,5]`, 
  car `take` extraie les 5 premiers éléments de la liste infinie créée
  par `numbersFrom 1`.
  """
    , md
 """
  ## Typage statique fort

  - Un type représente un ensemble de valeurs, sur lesquelles on peut faire les mêmes opérations. 
  - Chaque valeur a un type (donné explicitement ou déduit automatiquement).
  - Aucune conversion implicite. 
  - Les types sont vérifiés à la compilation, ce qui révèle de nombreuses erreurs.

  """
    -- types prédéfinis (atomiques, function mapping, structurés), types algébriques ?
    -- point fort, point faible, dev web, entreprises
    , md
 """
  # Point fort

  Haskell est particulièrement adapté aux tâches de compilation
  nécessitant une structure d'arbre fixe, mais
  une diversité croissante d'opérations.  

  Exemple : [Pandoc](<https://github.com/jgm/pandoc>), 
  un convertisseur entre formats de fichier à balise.
  """
    , md
 """
  # Point faible

  - Il peut être difficile de manipuler des structures non récursives comme les matrices ou les graphes. 
  - Il peut être difficile, voire impossible, d'atteindre la complexité optimale pour certaines tâches. 
  - La complexité en temps est particulièrement difficile à déterminer à cause de l'évaluation paresseuse. 

  """
    , md
 """
  # En entreprise

  Haskell est utilisé en production dans des grandes entreprises :
 
  - Microsoft : [Bond](https://github.com/Microsoft/bond>)
  - Facebook : [Haxl](https://github.com/facebook/Haxl>) 

  Consultez par exemple cette [liste](<https://github.com/erkmos/haskell-companies>)
  qui répertorie des entreprises qui utilisent Haskell. 

  """
    , md
 """
  # Développement Web

  - backend
  
    - [Yesod](https://www.yesodweb.com/)
    - [Happstack](http://happstack.com/)
    - [Snap](http://snapframework.com/)
    - ...
  
  - frontend
  
    - compilateurs Haskell vers Javascript : 
  [GHCJS](https://github.com/ghcjs/ghcjs>), [Haste](https://haste-lang.org/)
    - langages dérivés pour le web : 
  [Purescript](http://www.purescript.org/), [Elm](https://elm-lang.org/)
  """
   ]         
