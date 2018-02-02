===============================================================================================
Ecosystème des langages de programmation (ELP): quelques paradigmes
===============================================================================================

:auteur: Tristan Roussillon
:date: 30/01/2018

      
Plan
==========================

Il y existe beaucoup de langages de programmation. Certains se ressemblent, d'autres sont radicalement différents. 
Un *paradigme de programmation* fournit et détermine la vue qu'a le développeur de l'exécution de son programme
(source: Wikipédia).

- Programmation procédurale 
- Programmation orientée objet (OOP)
- Entracte  
- Programmation fonctionnelle (FP)
- Organisation de l'enseignement 

============================
Programmation procédurale
============================

Machine de Turing
============================

Alan Turing (1912-1954)

C'est un modèle minimal de *machine universelle*
capable d'exécuter des *algorithmes*.

.. figure:: figs/turing-machine.*
   :scale: 100%
   :alt: machine de Turing
   :align: center

(image: `texample.net <http://www.texample.net/tikz/examples/turing-machine/>`_)
	   
..
   - machine composee d'une bande infinie
   - avec des cases dans lesquelles on peut lire/écrire 
   - qu'on peut deplacer d'un cran vers la gauche ou la droite
   - guidé par un programme simple

Programmation procédurale
============================

Les langages procéduraux, comme le *C*, sont très proches
du modèle de Turing:

- concept de *variable*: boîte dans laquelle on peut lire, écrire (affectation) une *valeur*.
- concept d'exécution:
    
  - on exécute des *instructions*,
  - organisées par des *structures de contrôle* (branchement, boucle),
  - et des *fonctions* (= sous-programmes qui retourne généralement une valeur). 

=============================
Programmation orientée-objet
=============================

L'exemple de Fred le fleuriste s'inspire de
`*An Introduction to Object-Oriented Programming*. 3rd Ed. Timothy A. Budd <http://web.engr.oregonstate.edu/~budd/Books/oopintro3e/info/chap01.pdf>`_.

Message, objet et méthode
============================

La programmation orientée-objet étend et abstrait
les concepts de valeur et fonction par ceux d'objet et de méthode. 

Toute action est déclenchée par 

- une *requête* envoyée à un *objet*.
- l'objet y répond en exécutant les instructions d'une *méthode*

.. literalinclude :: code/ExempleObjet.py
    :language: python
    :lines: 52
  
Les objets sont représentés en mémoire :

- bouquetStandard: @0x7f7b64de9690(Bouquet, 3, "rose")


Messages vs appels de fonctions
================================

1. Un message est adressé à un objet,
   mais on peut le considérer comme un appel de
   fonction à laquelle on a ajouté l'objet en paramètre.

.. code-block :: python

    fred.preparerBouquet(bouquetStandard)
    Fleuriste.preparerBouquet(fred,bouquetStandard)
   
2. L'interprétation du message dépend de la catégorie de l'objet
   auquel il est adressé. En réponse au même message, deux objets
   différents peuvent agir différemment (*polymorphisme*).

.. literalinclude :: code/ExempleObjet.py
    :language: python
    :lines: 52,56

    
Classe
======================

Un objet est l'instance d'une classe qui décrit son comportement.
La méthode utilisée par un objet en réponse à une requête dépend de sa classe
(ou de ses classes parentes). 

.. literalinclude :: code/ExempleObjet.py
    :language: python
    :lines: 13-21


Hiérarchie de classe
======================

Les classes sont organisées dans une hiérarchie.
Une classe partage son comportement avec sa(ses) classe(s) fille(s).
Une classe fille ajoute de nouveaux comportements (ou en redéfinit certains).  

.. literalinclude :: code/ExempleObjet.py
    :language: python
    :lines: 3-11

Liaison dynamique
=======================

La recherche d'une méthode en réponse à une requête débute par la classe du récepteur.
Si aucune méthode appropriée n'est trouvée, la recherche se poursuit dans la classe parent.
La recherche se poursuit ainsi de parent en parent jusqu'à ce qu'une méthode soit trouvée
ou que la hiérarchie soit épuisée.
Dans le premier cas, la méthode est exécutée, dans le second, une erreur se produit. 


Exemple
=========================

.. literalinclude :: code/ExempleObjet.py
    :language: python
    :lines: 50-56

.. code-block :: none

    bonjour
    cela nous fait 4.5 euros, svp
    bonjour
    Traceback (most recent call last):
      File "ExempleObjet.py", line 56, in <module>
        kenneth.preparerBouquet(bouquetStandard) 
    AttributeError: 'Dentiste' object has no attribute 'preparerBouquet'


Résumé des concepts principaux
===============================

Alan Key (1940-)

1. Un programme est une communauté d'objets communiquant des messages ;
   un message est une demande d'action accompagnée des arguments nécessaires à sa réalisation.
2. L'état de chaque objet est stocké en mémoire. 
3. Chaque objet est une instance de classe. 
4. La classe contient le comportement associé à un objet ; tous les objets de la même classe ont le même comportement.
5. Toutes les classes sont organisées dans une hiérarchie. Une classe partage son comportement à toute classe descendante.

Langages orientés objet
=========================

- SmallTalk, 1971
- La plupart des langages orientés objets sont multi-paradigmes.
  On peut citer notamment
  - C++
  - *java*
  - *python*, ...
- Les langages procéduraux comme *C* peuvent être utilisés comme des langages orientés objet 
  (`*Object-Oriented Programming With ANSI-C*, Axel Schreiner, 2011 <https://www.cs.rit.edu/~ats/books/ooc.pdf>`_). 

============================
Entracte
============================

Connaissez-vous `robozzle <http://www.robozzle.com/>`_ ? 
==========================================================

.. figure:: figs/robozzle.*
   :alt: robozzle
   :align: center


Règles du jeu maison
============================

On veut déplacer un robot en lui envoyant un message.
Ce message est formé par la concaténation des lettres suivantes:

- ``L`` (left) : fait pivoter le robot à gauche
- ``R`` (right) : fait pivoter le robot à droite
- ``F`` (forward) : fait avancer le robot d'une case
- ``Pn`` (procedure #n) : sous-message

Par exemple, si on envoie le message ``P1`` où ``P1 = LFRF``,
le robot se déplacera d'une case sur la gauche ``LFR``, puis d'une case en avant ``F``

Jeu 1
============================

.. figure:: figs/jeu1.*
   :width: 35%
   :alt: jeu1
   :align: center

Comme déplacer le robot selon le trajet pointillé ?
	   
- ``P1=_ _ _ _``
- ``P2=_ _ _ _``
- message: ``_ _ _``

Jeu 2
============================

.. figure:: figs/jeu2.*
   :width: 35%
   :alt: jeu2
   :align: center

Comment déplacer le robot en diagonal sans interruption ?
	   
- ``P=_ _ _ _ _``
- message: ``_``

Nouvelle règle
============================

On ajoute des couleurs de fond : sans couleur de fond l'instruction ``x``
sera toujours exécutée; dans les autres cas, elle ne sera exécutée
que si le robot se trouve sur une case de couleur identique à celle du fond
de l'instruction. 

Les messages suivants, par exemple, ne feront avancer le robot que d'une case,
car ni la case actuelle du robot, ni la suivante ne sont rouges. 

.. figure:: figs/jeu3.*
   :width: 35%
   :alt: configuration
   :align: center

.. figure:: figs/message.*
   :width: 10%
   :alt: message
   :align: center

  
Jeu 3
============================

.. figure:: figs/jeu3.*
   :width: 35%
   :alt: jeu3
   :align: center

Comment déplacer le robot jusqu'à la case rouge et le faire revenir puis s'arrêter sur sa case de départ ?
	   
- ``P=_ _ _ _ _``
- message: ``_``

Pourquoi ce n'est pas si facile ?
===================================

- On ne décrit pas le chemin en extension, mais on le factorise
  en fonction des contraintes sur la taille du message et des procédures. 
  La trace est obtenue par *substitution*.

- De plus, toutes les instructions ne sont pas exécutées selon la position
  du robot. Il y a des *effets de bord* (side effects). 

Substitution
==============

.. figure:: figs/jeu3.*
   :width: 35%
   :alt: jeu3
   :align: center

.. figure:: figs/substitution.*
   :width: 85%
   :alt: substitution
   :align: center

Effets de bord
================

- Il y a des instructions dont l'exécution dépend de la
  couleur de la case dans laquelle se trouve le robot.

- On peut avoir :
  //couleur == bleu ; F ; //couleur == rouge

- C'est un effet de bord : la position du robot (et donc
  éventuellement la couleur de la case dans laquelle
  il se trouve) a changé, mais ça ne se voit pas sur le
  message.

Effets en python et cpp 
================================

.. code-block :: python

   def blork(a, b):
       # - n'importe quoi sur a et b, qui sont de n'importe quel type
       # - Renvoie n'importe quoi
       # Des IOs (variables globales, réseau, fichiers, ...)
       ...

.. code-block :: cpp

   float blork(float a, float b)
   {
       // - n'importe quoi sur a et b, qui sont de type float
       // - Renvoie un float
       // Des IOs (variables globales, réseau, fichiers, ...)
   }

(source: `G. Bouchard <http://aramis.resinfo.org/ateliers/slides-haskell/presentation.html#1>`_)
   

Effets en python et cpp
================================

Les codes C++, python suivants sont-ils équivalents ?

.. code-block :: cpp

   // version 1
   float a = blork(5, 2);
   float b = blork(5, 2);
   return a + b;

   // version 2
   float a = blork(5, 2);
   return a + a;

.. code-block :: python

   # version 1
   print("hello")
   notUsedVariable = doSomething(otherVariable)
   print("world")

   # version 2
   print("hello")
   print("world")

Pureté en Haskell
==================

Les langages fonctionnels sont généralement purs :
ils n'ont pas d'effet de bord. 

.. code-block :: haskell

   blork :: Float -> Float -> Float
   blork a b = ... -- N'importe quoi sur a et b, des Float
               ... -- Renvoie un Float
               ... -- Pas d'IO !

.. code-block :: haskell

   -- version 1
   let a = blork 5 2
       b = blork 5 2
   in a + b

   -- version 2
   let a = blork 5 2
   in a + a

   
============================
Programmation fonctionnelle
============================


Lambda-calcul
============================

Alonzo Church (1903-1995)

C'est un modèle équivalent à celui de Turing!

Sa grammaire est la suivante : 

- <expression>:= <var> | <func> | <call>
- <func>:= λ <expression> . <expression>
- <call>:= <expression> <expression> 

Les mots réservés sont λ, ., éventuellement des parenthèses () pour clarifier.

Interprétation
================

- λx.y est une fonction qui prend x et retourne y, mais x et y sont des expressions
  (et peuvent donc être des variables, des fonctions, des appels ou une combinaison de tout ça).

- λx.x est la fonction identité
    
- λx.x y est un exemple d'appel : la fonction identité prend en argument y. On remplace donc x par y (substitution) dans l'expression de retour, c'est-à-dire ici x. Ce qui s'écrit λx.x y = x[x->y] = y 

Rien que des lambda-expressions
=================================

Il n'y a pas de chaînes de caractères, ni d'entiers, ni de structures de contrôle.
Il n'y a que des expressions, des fonctions, des appels.

Les valeurs de vérité False, True sont des expressions spécifiques :  

- False : λ x . ( λ y . y )
  fonction qui retourne l'identité
- True : λ x . ( λ y . x ) 
  fonction qui retourne une fonction constante

Closure (Fermeture)
========================

Reprenons True: λ x . ( λ y . x ).
La première fonction retourne une fonction λ y . x qui a mémorisé le paramètre x et le retourne : c'est une fermeture.

Cette dernière n'est pas une fonction mathématique, mais comme elle n'est pas nommée, elle ne peut être utilisée seule ; uniquement avec celle qui la retourne, comme dans l'appel suivant :

( λ x . ( λ y . x ) ) i j
= ( λ y . x ) j [x -> i]
= ( λ y . i ) j
= i [y -> j]
= i

Il y a une écriture qui autorise les fonctions à plusieurs paramètres et explicite ce lien entre fonctions. 

Curryfication
========================

La *curryfication* et *décurryfication*, d'après Haskell Curry (1900-1982)
sont les transformations entre ces deux écritures équivalentes : 
λ x1 . (λ x2 . (λ xn . E) et  λ x1x2xn . E

Voici donc une autre écriture pour les valeurs de vérités :

- False : λ xy . y 
  fonction qui retourne son second paramètre
- True : λ xy . x  
  fonction qui retourne son premier paramètre

Branchement
========================

On veut mimer la structure conditionnelle suivante : a ? b : c.
Mais comment s'y prendre sans notion de branchement ?

Voilà le If : λa.(λb.(λc. (a b c) ))

Ou encore : λabc.abc, où a est une fonction qui retourne
soit son premier paramètre dans le cas True (ici b),
soit son second paramètre dans le cas False (ici c).


Proof of concept en Python
==========================

.. code-block :: python
	
    >>> myTrue = lambda x : (lambda y : x)
    >>> myFalse = lambda x : (lambda y : y)
    >>> myIf = lambda a : (lambda b : (lambda c: a(b)(c)))
    >>> print( myIf(myTrue)("b")("c") )
    b
    >>> print( myIf(myFalse)("b")("c") )
    c

.. code-block :: python

    >>> myTrue = lambda x,y : x
    >>> myFalse = lambda x,y : y
    >>> myIf = lambda a,b,c : a(b,c)
    >>> print( myIf(myTrue,"b","c") )
    b
    >>> print( myIf(myFalse,"b","c") )
    c

Terminons la boucle !
=======================

- Toute boucle peut s'écrire sous forme récursive
  (à faire à la maison si ça ne vous paraît pas évident).

- Mais comment une lambda-fonction, anonyme,
  peut s'appeler elle-même ?

- Le `Y-combinator`, formellement défini comme Y = λf.(λx.f (xx)) (λx.f (xx)),
  fait ce tour de force.  

Proof of concept en Python
============================

.. literalinclude :: code/Ycombinator.py
    :language: python
    :lines: 1-3

L'idée c'est d'ajouter la fonction elle-même en paramètre. 

.. literalinclude :: code/Ycombinator.py
    :language: python
    :lines: 6,7

Tout écrire en une ligne évite les affectations.  

.. literalinclude :: code/Ycombinator.py
    :language: python
    :lines: 10,11

Enfin, la *currification* permet de n'avoir que des fonctions à un paramètre.

..
  Boucle
  ===========================
  Une boucle peut s'écrire en toute généralité :
  
  .. code-block :: C 

      X=A; while(test(X)) X=body(X)

  Ce qu'on peut développer ainsi : 

  .. code-block :: C 

      X=A; if(test(X)) X=body(X); while(test(X)) X=body(X)

  D'où la définition récursive suivante : 

  .. code-block :: C  

      data myWhile(data test(data), 
                   data body(data), 
                   data x) {
        return test(X) ? myWhile(test, body, body(X)) : x; 
      }


   
Programmation fonctionnelle
============================

Les langages fonctionnels viennent directement du lambda-calcul. 

- notion de *variable*: symbole (qui peut réprésenter une expression plus complexe, y compris des fonctions).
- notion d'*évaluation*: les expressions sont évaluées par substitution
  (à la demande dans un cas "paresseux").

On peut citer :
    
- Lisp (1958), Scheme, ML, *Haskell*, Ocaml, Erlang, Scala, Clojure...
- D'autres langages inclus des mécanismes facilitant l'utilisation d'un style fonctionnel : Perl 6, python 3, C++ 11, Java 8...

FP vs OOP
=========================  

- OOP:
  
  - facile d'ajouter un nouvel objet
  - plus difficile d'ajouter une nouvelle méthode,
    car cela nécessite souvent de modifier la définition de plusieurs classes
    reliées par des dépendances d'héritage. 
    
- FP:
      
  - facile d'ajouter une nouvelle fonction,
  - plus difficile de modifier ou ajouter une struture de données,
    car cela nécessite souvent de modifier plusieurs fonctions pour rajouter des cas. 

FP et OOP : usage favori
==========================  

- OOP : interfaces où il y a un petit nombre d'opérations fixes
  (`paint`, `open`, `close`, `resize`, etc.), mais une diversité croissante d'objets graphiques.  
    
- FP : compilation,
  car il a une structure d'arbre récursive fixe, mais une diversité croissante d'opérations
  pour optimiser le code ou ajouter de nouvelles transformations.   

================================
Organisation de l'enseignement
================================

Objectifs
=====================

Avoir un panorama des concepts les plus fréquents
dans les langages de programmation afin de développer
la capacité de s'adapter à un nouveau langage.

- python (scripting, multi-paradigme, typage dynamique)
- javascript (fonctionnel pour le web)
- haskell (typage statique fort, fonctionnel)
- go (concurrence)
- java (objet, reflexivité)

  
Déroulement
================

5 langages, 4 TDs face à face pour chacun:

- *python* (R. Kéchichian)
- TD plateforme de code: `codingame <https://www.codingame.com/>`_
- *javascript* (S. Frénot)
- *haskell* (T. Roussillon)
- *go* (S. Gillani)
- *java* (S. Dufromentel, R. Kéchichian, T. Roussillon)
- examen final

Java
=================

Ce langage sera traité de manière spécifique car

- beaucoup d'entre vous en maîtrisent les bases, 
- c'est un prérequis pour d'autres matières en 4TC, 
- il est très utilisé en entreprise.

La première partie (clairement identifiée dans le
`support <http://liris.cnrs.fr/~troussil/ens/java/2018/html/index.html>`_)
devra être travaillée en **autonomie**.

La première heure du premier TD sera une séance de
questions-réponses pour vous aidez à maîtriser les
bases du langage, indispensables pour suivre la suite
(collections, flux, threads).
Les questions sont à préparer et poster sur le forum du cours.    


Evaluation
===================

Contrôle continu (40 %)

a) codingame pour moitié
b) medium pour moitié

Examen final 1h sur machine - 1h sur papier (60 %)

a) `codingame <https://www.codingame.com/>`_
=============================================

Codingame est une plateforme de code. Dans la zone d'entraînement,
il y a des *puzzles*, classés par niveau de difficulté :
facile, moyen et difficile et très difficile.

Pour résoudre les puzzles, on écrit du code dans le langage
de son choix. Ce code sera automatiquement testé pour vérifier
sa validité par rapport aux objectifs du scénario. 

a) contrat codingame
=======================

On évaluera deux aspects : la difficulté des puzzles résolus et
la diversité des langages utilisés.

Date limite : avant le dernier TD.

Difficulté
=============

- 2 points pour "Première victoire" (réussir 1 puzzle facile)
- 4 points pour "C'est du gâteau" (3 faciles)
- *6 points pour "On est bien" (3 moyens)*
- 8 points pour "Je vous attends" (3 difficiles)
- 10 points pour "Respect" (3 très difficiles)

Diversité 
=================

- 2 points pour "Habitué(e)" dans 1 langages différents
- 4 points pour "Habitué(e)" dans 2 langages différents
- *6 points pour "Habitué(e)" dans 4 langages différents*
- 8 points pour "Habitué(e)" dans 7 langages différents
- 10 points pour "Habitué(e)" dans 12 langages différents

  
b) `medium <https://medium.com/>`_
====================================

Vous devez rédiger sur medium une entrée de blog
qui doit être lue en 3 à 4 minutes. Le sujet qui vous
a été attribué arbitrairement est disponible sur
Moodle. Vous pourrez créer votre compte lors du
TD "codingame". Vous serez alors ajouté comme
"writer" à une publication. Vous pourrez soumettre
votre texte, qu'un enseignant relira.
Date limite : on arrête tous les retours au moment
de l'examen final, il faut donc *s'y prendre le plus tôt
possible*.

b) contrat medium
=====================

- > 16 : excellent, l'enseignant est bluffé tant sur le
  fond que sur la forme
- 16 : très bien, il n'y a aucune correction à apporter
- 12 : bien, mais doit être révisé pour tenir
  compte du retour de l'enseignant
- 10 : bien après plusieurs révisions.
- 6 : mauvais après plusieurs révisions.
- 0 : pas fait.
