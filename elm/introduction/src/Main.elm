module Main exposing (..)
import Slides exposing (..)
import Slides.SlideAnimation

main = Slides.app

    { slidesDefaultOptions
        | slideAnimator = Slides.SlideAnimation.scroll
    }

    [
     md
 """
  # (H).E.L.P: Haskell, elm
  
  """
    , md
 """ 
  # Pourquoi des langages fonctionnels ?

  - La programmation fonctionnelle, ancienne, explose de nouveau dans le web. 
  - Les idées mises en oeuvre font leur chemin aussi dans des langages généralistes.

  """
    , md
 """
  # Pourquoi Haskell et elm ?

  - Haskell a des caractéristiques peu communes. 
  - Les connaitre permet de mieux saisir les autres langages.
  - elm est inspiré de Haskell, mais plus simple. 
  - On fait une appli web monopage très rapidemment.  
  - Tous deux sont formateurs, car on ne peut faire autrement que coder dans un style fonctionnel. 
  
  """
    , md
 """ 
 # Plan
  
  1. Haskell
  2. elm
  3. Travail à faire
  
  """
    , md
 """
  ![Logo Haskell](static/images/haskell-logo.svg)
  
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
  - typage statique fort

  """
    , md
 """
  ## Pureté

  - Une expression est toujours évaluée en la même valeur.
  - Permet de raisonner sur les programmes comme en algèbre : 
  ex. `inc 3` sera toujours évalué en `3+1`. 
  - Les *effets de bord* (modification d'une variable globale, entrées-sorties, etc.) 
  sont absents (ou si nécessaires, explicitement encapsulés).  
  

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

  Exemple : 
  ```haskell
  si True valSiVrai valSiFaux = valSiVrai
  si False valSiVrai valSiFaux = valSiFaux
  ```

  ```haskell
  si (x >= 18) (print "yes") (print "no")
  ```
  
  Un seul affichage sera effectué (au lieu de deux dans le cas d'une évaluation stricte)

  """
    , md
 """
  ### Conséquence 2

  Il est possible de manipuler des structures de données infinies. 

  Exemple : 

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
    , md

 """
  # Haskell dans la vraie vie

  """
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

  Haskell est utilisé en production par des entreprises. 
 
  Exemple : [Haxl](https://github.com/facebook/Haxl>) de Facebook. 

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
  [Purescript](http://www.purescript.org/), [elm](https://elm-lang.org/)
  """
     , md
 """
  ![Logo elm](static/images/elm-logo-bande.svg)
  
  - elm a été créé par Evan Czaplicki en 2012.
  - Il est officiellement documenté : 
  [guide](https://guide.elm-lang.org/), 
  [syntaxe](https://elm-lang.org/docs/syntax), 
  [packages](https://package.elm-lang.org/). 
  - C'est un langage fonctionnel, inspiré de Haskell.  
  """
    , md
 """ 
    # Les caractéristiques d'elm
  
  - pureté,
  - évaluation *stricte*, 
  - typage statique fort, 
  - dédié à la création d'applications web, 
  - interopérable avec javascript. 

  """
    , md
 """
  ## Pureté

  - Une expression est toujours évaluée en la même valeur.
  - Aucune valeur ne peut changer après avoir été créée (immutabilité). 
  - Si on veut, par exemple, ne garder que certains éléments d'une liste,
  on va créer une *nouvelle* liste contenant les éléments à conserver. 
  """
    , md
 """
  ## Evaluation stricte 

  Les expressions sont évaluées tout de suite; en particulier, 
  les arguments sont évalués au moment de l'appel. 

  ```elm
  numbersFrom x = x :: (numbersFrom (x+1))
  ```

  Les deux appels suivant provoquent une erreur : 

  ```elm
  numbersFrom 1
  ``` 

  ```elm
  List.head (numbersFrom 1)
  ``` 

  """
    , md
 """
  ## Typage statique fort

  - Un type représente un ensemble de valeurs. 
  - Chaque valeur a un type.

  ```elm
  x = 5 -- déduit automatiquement
  ```

  ```elm
  x : Int -- donné explicitement
  x = 5
  ```

  - Il existe des types prédéfinis, mais on peut aussi définir ses propres types.  
  """
    , md
 """
  ### Types prédéfinis

  - types atomiques
    - `Bool`, `Int`, `Float`, `Char`, `String`...
    - `Int -> Bool`, `Int -> Int -> Int`...
  - types structurés 
    - tuples comme `(Int, Bool)`...
    - records comme `{age:Int, name:String}`... 
    - listes comme `List Int`, `List (List Int)`... 
  """
    , md
 """
  ### Types personnalisés 

  On peut aussi créer des types *algébriques* :

  ```elm
  type IntTree = 
    Empty | Node Int IntTree IntTree
  ```

  On peut également créer des synonymes de types existants
  en introduisant le mot-clé `alias` : 

  ```elm
  type alias PhoneBook =
    List {name:String, phone:String}
  ```

  """
  , md
 """
  ## Appli web élémentaire
"""
 , md
 """
  ### Aspects structurels
  
  La structure de l'application repose sur deux types : 

    - `Model` *modélise* l'état de l'application,
    - `Msg` représente le *message* envoyé par l'interface 
  suite à un événement pour mettre à jour le modèle.  

  => il faut savoir définir des types!
  """
 , md
 """
  ### Aspects fonctionnels
  
  Le modèle est déterminé par deux fonctions : 
  - `init : Model` 
  - `update : Msg -> Model -> Model`
  
  Une troisième fonction traduit le modèle en HTML : 
  - `view : Model -> Html Msg`   

  => il faut savoir définir des fonctions!
  """
 , md
 """
  ### Point d'entrée
  
  C'est la fonction `main` 
  à laquelle on *attache* les trois autres fonctions
  (qu'on peut appeler comme on veut, seul leur signature compte). 
      
  ```elm
  main = Browser.sandbox 
    { init = init
    , update = update
    , view = view
    }
  ```
  """
 , md
 """
  ### Schéma de l'architecture

  ![elm architecture](static/images/elm-archi.svg)
  """
 , md
 """
  ### Runtime system et boucle d'événement
 
  - Attend un événement, 
  - Envoie un message, 
  - Produit un nouveau modèle (update), 
  - Traduit le modèle en HTML (view), 
  - Affiche le nouveau document HTML, 
  - Recommence. 

 """
 , md
 """
  ### Pour aller plus loin

  Il est possible non seulement de manipuler le DOM, 
  mais aussi d'interagir avec l'extérieur via 
  - les *commandes*, pour demander l'exécution de tâches, 
  - les *souscriptions*, pour écouter des événements.
  
  Voir `browser.element`, `browser.document`. 
  """
 , md
 """ 
  ## Interopérabilité avec javascript

  Un programme elm est [compilé en javascript](https://guide.elm-lang.org/interop/)
  et peut communiquer avec javascript de deux manières : 
    - par les [flags](https://guide.elm-lang.org/interop/flags.html) au lancement du programme, 
    - par les [ports](https://guide.elm-lang.org/interop/ports.html) en cours d'exécution.
  """
 , md
 """ 
  ## elm vs javascript

  Puisque elm est compilé en javascript, 
pourquoi ne pas écrire directement en javascript ?

Essentiellement pour écrire avec un langage fonctionnel pur, 
fortement typé et compilé. Un grand nombre d'erreurs seront
révélées et donc corrigées à la compilation, ce qui augmente
la fiabilité de l'application finale.   
  """
 , md
 """
  ### Compilation 
      
  ```
  elm make src/Main.elm --output=Main.js
  ```
  Crée un fichier javascript `Main.js` qui expose une fonction `Elm.Main.init()` 
  qui prend en entrée un dictionnaire dont le champs `node` pourra être associé à un noeud du DOM. 

  """
 , md
 """
  ### Intégration (1/2)
      
  Dans l'en-tête du document HTML, il faut intégrer le fichier `Main.js` : 

  ```html
<html>
<head>
  <meta charset="UTF-8">
  <title>Main</title>
  <script src="Main.js"></script>
</head>
...
```
"""
 , md
 """
  ### Intégration (2/2)

  Dans le corps du document HTML, il faut créer un noeud et initialiser le programme elm à partir de ce noeud : 
 
 ```html
...
<body>
  <div id="myapp"></div>
  <script>
  var app = Elm.Main.init({
   node:document.getElementById('myapp')
  });
  </script>
</body>
</html>
```
"""
    , md
  """ 
  # Eco-système elm 

  - compilateur : Haskell, 
  - runtime system : HTML, CSS, javascript, 
  - site web officiel : 
    - serveur : Haskell, 
    - pages : elm (sauf éditeur de code). 

  [source](https://elm-lang.org/assets/papers/concurrent-frp.pdf)

  """
    , md
  """ 
  # Travail à faire

  """
    , md
  """ 
  ## Organisation

  - 1 CM d'introduction (aujourd'hui)
  - prise en main en autonomie 3x2h
    - [TD1](https://github.com/troussil/elp/blob/master/elm/TD1/README.md) : types existants et fonctions,
    - [TD2](https://github.com/troussil/elp/blob/master/elm/TD2/README.md) : types personnalisés et architecture elm,
    - finir les TDs ou parfaire ses connaissances et préparer le mini-projet à suivre.
  - réalisation d'un [mini-projet](https://github.com/troussil/elp/blob/master/elm/projet/README.md), 4h.

  """
    , md
  """ 
  ## Mini-projet

  - A réaliser par deux (inscription sur un document partagé).  
  - Donnez vos noms et l'adresse d'un repository github public. 
  - Date limite 05/02/2024, 8h. 
  - Critères d'évaluation sur [Moodle](https://moodle.insa-lyon.fr/course/view.php?id=7725). 
  - [Enoncé](https://github.com/troussil/elp/blob/master/elm/projet/README.md) et [exemple](https://perso.liris.cnrs.fr/tristan.roussillon/GuessIt/). 

  """
     , md
  """
      Ce document a été écrit en elm avec le package [elm-slides](https://package.elm-lang.org/packages/xarvh/elm-slides/latest/).
  """
   ]         
