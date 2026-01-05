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
  # (H).E.L.P: elm
  
  """
    , md
 """
  ![Logo elm](static/images/elm-logo-bande.svg)
  
  - elm a été créé par Evan Czaplicki en 2012.
  - Il est officiellement documenté : 
  [guide](https://guide.elm-lang.org/), 
  [syntaxe](https://elm-lang.org/docs/syntax), 
  [packages](https://package.elm-lang.org/). 
  - C'est un langage *fonctionnel*, inspiré de Haskell.  
  """
      , md
 """
  # elm, un langage fonctionnel

  """
    , md
  """
   ## Valeur

   - Une valeur est une entité abstraite comme 
   `5, 'a', "abc", [1,2,3], ('b',4), x, inc` 
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
  - l'évaluation est stricte : les arguments sont évalués avant le passage de paramètre. 
  """
    , md
 """
  ## Pureté (transparence)

  - Une expression est toujours évaluée en la même valeur.
  - Permet de raisonner sur les programmes comme en algèbre : 
  ex. `inc 3` sera toujours évalué en `3+1`. 
  - Les *effets de bord* (mise à jour d'une variable, entrées-sorties, etc.) 
  sont absents (ou si nécessaires, explicitement encapsulés).   
"""  
    , md
 """ 
 # Plan
  
  1. Intermède historique
  2. Typage statique fort
  3. Appli web monopage
  3. Travail à faire
  
  """
    , md
 """
# Brève histoire des langages fonctionnels

- origine mathématique (>1930)
- émergence (1950-2000)
- popularité croissante (2000-2020)
- aujourd'hui
"""
    , md
 """ 
## Origine : λ-calcul

- λ-calcul (>1930), créé par Alonzo Church
- modèle équivalent à la machine de Turing
- base théorique pour la programmation fonctionnelle.
"""
    , md
 """ 
## Emergence (1950-2000)

- LISP (1958) : récursivité et listes
- ML (1973) : typage statique et polymorphe
- Haskell (1990) : évaluation paresseuse et pureté 
"""
    , md
 """ 
## Popularité croissante (2000-2020)

- adoption d'éléments fonctionnels dans des langages généralistes comme Python, Java (8), C++ (11)
- augmentation des systèmes parallèles et distribués, pour lesquels la pureté est idéale
"""
    , md
 """ 
## Aujourd'hui

- Systèmes où la fiabilité est cruciale (Haskell, OCaml, F#),
- Big data (Spark), 
- Développement web :
  - Javascript (nombreux frameworks et langages dérivés).
  - compilateurs Haskell vers Javascript : 
  [GHC](https://www.haskell.org/ghc/) (9.6, js backend)
  - langages dérivés d'Haskell : 
  [Purescript](http://www.purescript.org/), [elm](https://elm-lang.org/)
"""
    , md
 """    
  ## Point fort des langages fonctionnels 

  - tâches de compilation/conversion
    nécessitant une structure d'arbre fixe, 
    une diversité croissante d'opérations.  
  - calcul en parallèle grâce à la pureté. 
  """
    , md
 """
  ## Points faibles des langages fonctionnels 

  - Difficile de manipuler des structures non récursives comme les matrices ou les graphes. 
  - Difficile, voire impossible, d'atteindre la complexité optimale pour certaines tâches. 
  """
    , md
 """
  # Retour sur elm

  - Typage statique fort
  - Appli web monopage 
"""
   , md
 """
  ## Typage statique fort
"""
   , md
 """
  ### Typage statique fort

  - Un type représente un ensemble de valeurs, sur lesquelles on peut faire les mêmes opérations. 
  - (fort) Chaque valeur a un type (donné explicitement ou déduit automatiquement). Aucune conversion implicite. 
  - (statique) Les types sont vérifiés à la compilation, ce qui révèle de nombreuses erreurs.

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
  ### Typage fort, mais implicite 

  Les types sont donnés explicitement 
  ```elm
  x : Int -- x est de type Int
  x = 5
  ```
  ou déduit automatiquement
  ```elm
  x = 5
  ```
  """
     , md
     """
 ### Classes 
         
 Il existe 4 classes de types :
 - `number` (`Int` and `Float`)
 - `appendable` (`String` et `List a`)
 - `comparable` (`Int`, `Float`, `Char`, `String`, listes/tuples de valeurs comparables)
 - `compappend` (`String` et `List comparable`)
    
 utilisées pour la déduction automatique
 des types et la définition de fonctions 
 génériques, c-à-d. paramétrées par un type. 
 """
    , md
 """
  ### Types personnalisés 

  On peut définir tous types *algébriques* 

  ```elm
  type ITree = 
   Empty | Node Int ITree ITree
  ```

  et les paramétrer
  ```elm
  type T a = 
   Empty | Node a (T a) (T a)
  ```

  """
   , md
     """
### Exemple

Que fait cette petite fonction ?

```elm
ht tree = case tree of
   Empty -> 0
   Node _ l r -> 1 + 
       max (ht l) (ht r)
```
   """
    , md
 """
  ### Synonymes de types
 
  On peut également créer des synonymes de types existants
  en introduisant le mot-clé `alias` : 

  ```elm
  type alias PhoneBook =
    List {name:String, phone:String}
  ```
  """
  , md
 """
  ## Appli web monopage
"""
    , md
 """
 ### Prérequis : le DOM

 - Le Document Object Model (DOM) est la représentation arborescente 
 d'une page web chargée en mémoire.  
 - On peut le manipuler pour modifier le contenu/style d'une page web. 
 - Un programme elm est compilé en un programme javascript qui 
 manipule le DOM pour rendre une appli web monopage. 
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
 ### Exemple

     
 [compteur](https://guide.elm-lang.org/architecture/buttons.html)

     
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
  ### Interopérabilité avec javascript

  Un programme elm est [compilé en javascript](https://guide.elm-lang.org/interop/)
  et peut communiquer avec javascript de deux manières : 
    - par les [flags](https://guide.elm-lang.org/interop/flags.html) au lancement du programme, 
    - par les [ports](https://guide.elm-lang.org/interop/ports.html) en cours d'exécution.
  """
 , md
 """ 
  ### elm vs javascript

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
    # Conclusion
"""
   , md
 """ 
    ## Caractéristiques
  
  - elm est un langage fonctionnel pur,
  - à typage statique fort, 
  - dédié à la création d'applis web, 
  - interopérable avec javascript. 
  """
    , md
 """
  ## Pourquoi elm ?

  - Très formateur, car on ne peut faire autrement que coder dans un style fonctionnel. 
  - Moins puissant, mais plus simple, par rapport à d'autres langages fonctionnels. 
  - On fait des applis web monopages (notamment des jeux) très rapidemment.  
  - C'est utilisé dans la vraie vie : voir cette [liste](<https://github.com/jah2488/elm-companies>)
  qui répertorie des entreprises qui utilisent elm.   
  """
    , md
 """
  ## Faut-il continuer à apprendre à programmer en 2026 ?

  """
    , md
 """
  ### 1. Les programmes restent la réalité numérique 

Même si l’IA génère du code, **quelqu’un doit comprendre ce que fait le programme**, poser les contraintes, juger la correction, la sécurité, la performance.

Sans savoir coder, tu dépends entièrement d’outils que tu ne maîtrises pas.
    
  """     
    , md
  """ 
  ### 2. L’IA augmente la valeur des gens qui comprennent le code

* Une personne **qui ne code pas** utilise l’IA comme une boîte noire
* Une personne **qui code** utilise l’IA comme un amplificateur. Elle
    peut détecter une hallucination, un bug, refactoriser, auditer,
    optimiser ce qui est généré, relier plusieurs systèmes réels, ...

  """
    , md
  """ 
 ### 3. Programmer est une compétence en mutation

Ce qui est en train de mourir :

* le dev purement répétitif
* le copier-coller de tutos

Ce qui devient plus précieux :

* architecture
* modélisation
* fiabilité
* sécurité
* compréhension des systèmes complexes, ...

  """
    , md
  """ 
### 4. Programmer est une compétence transférable

Apprendre à programmer c'est aussi apprendre à :

* décomposer un problème
* raisonner sur des invariants
* comprendre les effets de bord
* distinguer *spécification* et *implémentation*, ...

Cette compétence se **transfère** à plein d’autres domaines.
  """
    , md
  """ 
### 5. Qu'est-ce qui vaut encore la peine d’être appris ?

* les fondamentaux (expressions, effets, types)
* les paradigmes (impératif, fonctionnel, objet)
* la lecture et la compréhension d'un programme
* la capacité à raisonner sur un programme, ...
  """
    , md
  """ 
  ### En résumé

* Oui, apprendre à coder reste pertinent
* C’est même plus stratégique qu’avant
* À condition de viser la compréhension, pas la routine
* Et d’utiliser l’IA comme un outil, pas comme un substitut

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

  - Deux sujets au choix. 
  - A réaliser par deux/trois (inscription document partagé Moodle).  
  - Date limite 06/02/2025, 8h. 
  - Critères d'évaluation sur [Moodle](https://moodle.insa-lyon.fr/course/view.php?id=7725). 

  """
     , md
  """
      Ce document a été écrit en elm avec le package [elm-slides](https://package.elm-lang.org/packages/xarvh/elm-slides/latest/).
  """
   ]         
