========================================
Introduction
========================================

4 caractéristiques majeures
========================================

Pourquoi Haskell ?
------------------------

- Haskell est très formateur car purement fonctionnel : on ne peut faire autrement que coder dans un style fonctionnel. 
- La programmation fonctionnelle, ancienne, explose de nouveau dans le web (python, js). 
- Les idées mises en oeuvre en Haskell font leur chemin dans d'autres langages plus populaires
  (list comprehension de python, generics de java, etc.).

Carte d'identité
------------------------

- Haskell a été créé en 1990 par un ensemble d'universitaires.
- Il est défini dans un document appelé *Haskell Report*, dont la dernière révision date de 2010.
- C'est un langage de programmation
    
  - fonctionnel
  - pur
  - paresseux
  - statiquement typé
    
Langage fonctionnel
------------------------

- Toutes les actions sont faites par l'évaluation d'*expressions* menant à des *valeurs*.
  
  Par exemple ``inc (inc 3)`` donne ``5``. 
- Les valeurs sont des entités abstraites comme ``5, 'a', [1,2,3], ('b',4), inc``. 
- Les fonctions sont définies sous forme équationnelle

  ``inc n = n + 1``.     
- Toutes les valeurs (y compris les fonctions) sont *first-class*. 
  Elles peuvent être passés comme arguments à des fonctions,
  retournées comme résultats, placées dans des structures de données, etc.     

Pureté
------------------------

- Les structures de données sont toujours figées (*immutable*).  
- Les expressions n'ont jamais d'effets de bord (comme modifier des variables globales ou effectuer des entrées-sorties).
  Leur évaluation mènera toujours au même résultat. 

- Le parallélisme est plus direct quand l'évaluation d'une expression est indépendante des autres.
- La forme équationnelle permet de mieux raisonner sur les programmes.
  Comme en algèbre, on peut toujours remplacer la partie gauche d'une equation par
  sa partie droite dans une seconde équation :

  ``inc (inc 3) = inc (3 + 1) = (3 + 1) + 1 = 5``. 

Paresse
--------------------------

En Haskell, les expressions ne sont pas évaluées tant que leur valeur n'est pas requise: 

``inc (3 + 1)`` est évalué en ``(3 + 1) + 1`` et  non ``inc 4``! 

Par exemple, ``v = 1/0`` ne produit aucune erreur, car cela signifie `v est défini comme étant 1/0`,
plutôt que `calculer 1/0 et stocker le résultat dans v`. Ce n'est que lorsque ``v`` sera utilisé
(test d'égalité, affichage, etc.) que l'erreur surviendra.

Par conséquent, il est possible :

- de définir ses propres structures de contrôle (`if` par exemple)
- de manipuler des structures de données infinies ou de taille exponentielle.
    
Typage statique fort
---------------------------      
      
- Chaque valeur à un type associé.
- Un type est intuitivement un ensemble de valeurs. 
- Les types sont vérifiés à la compilation.
    
- De nombreuses erreurs seront détectées à la compilation.
- Le code généré par le compilateur sera optimisé. 
- C'est aussi une aide pour raisonner sur les programmes.    

  
A quoi sert Haskell ?
==================================

Compilation
-------------------------

Comme tous les langages fonctionnels, Haskell
est particulièrement adapté aux tâches de compilation
nécessitant une structure d'arbre fixe, mais
une diversité croissante d'opérations pour ajouter
de nouvelles transformations.  

- `Pandoc <https://github.com/jgm/pandoc>`_, un convertisseur entre formats de fichier à balise, 
- compilateur de `Bond <https://github.com/Microsoft/bond>`_, framework de manipulation de données structurées utilisé par Microsoft,
- `Pugs <https://github.com/perl6/Pugs.hs>`_, premier compilateur Perl 6 (maintenant supplanté par `Rakudo Perl <https://rakudo.org/>`_).
  

Langages dédiés
----------------------------

Haskell est particulièrement adapté pour accueillir des
langages dédiés à des tâches précises, sous la forme
d'un ensemble de types et de fonctions à combiner pour
résoudre un problème spécifique.

Par exemple `diagrams <https://hackage.haskell.org/package/diagrams>`_
un `langage déclaratif <http://ozark.hendrix.edu/~yorgey/pub/diagrams-FARM.pdf>`_
en Haskell pour les graphiques vectoriels.

Le :download:`projet <download/enonce-projet.pdf>` de l'an passé est un autre exemple. 

Développement Web
-----------------------------------

- backend
  
  - `Yesod <https://www.yesodweb.com/>`_,
  - `Happstack <http://happstack.com/>`_,
  - `Snap <http://snapframework.com/>`_,
  - ...

- frontend
  
  - compilateurs Haskell vers Javascript : `GHCJS <https://github.com/ghcjs/ghcjs>`_, `Haste <https://haste-lang.org/>`_,
  - langages dérivés d'Haskell pour le web : `Elm <https://elm-lang.org/>`_, `Purescript <http://www.purescript.org/>`_.
   
Qui utilise Haskell ?
------------------------------------

Qui utilise un langage dans lequel les variables ne varient pas,
dans lequel on ne peut rajouter une instruction d'affichage, sans
changer le type de la fonction ? 

- Facebook (`Haxl <https://github.com/facebook/Haxl>`_, `Sigma <http://multicore.doc.ic.ac.uk/iPr0gram/slides/2015-2016/Marlow-fighting-spam.pdf>`_),
- Microsoft (`Bond <https://github.com/Microsoft/bond>`_),

Consultez par exemple cette `liste <https://github.com/erkmos/haskell-companies>`_
qui répertorie des entreprises qui utilisent Haskell. 

   
Compilation
=================================

GHC
---------------------------------

- GHC (Glasgow Haskell Compiler) est bien connu pour compiler
  un programme Haskell (fichier d'extension ``.hs``).

- La commande ``ghc fichier.hs -o executable`` produit
  directement un exécutable : ``./executable``.

- D'autres options sont utiles : ``-c`` pour compiler sans édition de lien et   
  ``-outputdir build`` pour mettre les fichiers intermédiaires dans un répertoire séparé ``build``
  (voir ``man ghc`` ou ``ghc --help``). 
 
Mode interactif
--------------------------

GHCi est un REPL (Read-Eval-Print-Loop) qui accompage GHC.

Vous pouvez évaluer des expressions,
demander le type d'une expression avec ``:type`` (``:t``),
charger des fichiers Haskell avec ``:load`` (``:l``) et ``:reload`` (``:r``),
et bien plus encore (``:?`` pour une liste des commandes disponibles).

Comme les entrées-sorties ne sont pas évidentes en Haskell
(à cause de la paresse et de la pureté du langage),
nous allons d'abord surtout utiliser ce mode interactif en tapant ``ghci``. 

Pratique
--------------------------

- Téléchargez ce :download:`programme <download/logo.hs>`.
- Compilez avec GHC, lancez l'exécutable en redirigeant la sortie vers `fig.svg`,
  puis tapez l'instruction: `[Forward 50]`.
- Chargez le fichier avec GHCi, puis tapez
  
  - `printSvgTail`, `printSvgHead`
  - `printSvgLine (Cursor 0 0 0) (Cursor 50 50 0)`
  - `changeCursor (Cursor 50 50 0) (Forward 100)`
  - `changeCursor (Cursor 50 50 0) (Left 90)`

- Est-ce que vous comprenez ce que fait le programme ?
    
Dessin
--------------------------

Relancez plusieurs fois le programme précédent avec les instrutions suivantes:

- `[Repeat 4 [Forward 50, Left 90]]`
- `[Repeat 360 [Forward 1, Left 1]]`
- `[Repeat 36 [Right 10, Repeat 8 [Forward 25, Left 45]]]`
- `[Repeat 8 [Left 45, Repeat 6 [Repeat 90 [Forward 1, Left 2], Left 90]]]`

Vous avez en main le compilateur d'un langage dédié pour réaliser des traces
en SVG. 


Conclusion
===================

Capacités/Connaissances
--------------------------

- Classer Haskell parmi les langages fonctionnels purs.
- Classer Haskell parmi les langages compilés à typage statique.
- Compiler un programme Haskell ou l'exécuter en mode interactif. 
