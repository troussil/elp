
===========================================
Introduction
===========================================

Mon premier programme
==========================


Ex.1. Hello World (5 min)
-------------------------------

.. literalinclude:: code/TD1/HelloWorldApp.java
   :language: java

- Copiez ce bloc de code dans un fichier appelé ``HelloWordApp.java`` (le nom sans extension est le nom de la classe). 
- Compilez en byte code : **javac HelloWordApp.java** (un fichier ``HelloWordApp.class`` a été créé).  
- Exécutez : **java HelloWordApp**. Que se passe-t-il ?

Fonctionnement
---------------------------

Compilé en un langage intermédiaire unique (bytecode) puis interprété: "write once, run everywhere". 

.. figure:: figs/machineVirtuelle.*
   :scale: 50 %
   :alt: compilation et interprétation
   :align: center


En pratique...
-------------------------

- un répertoire par séance
- un fichier source par classe : *NomClasse*.java


- un éditeur pour écrire les fichiers sources

- un shell pour compiler (javac *NomClasse*.java)...
- (on obtient un fichier compilé par classe : *NomClasse*.class) 
- ...et pour exécuter (java NomClasse)


Compiler proprement
--------------------------

- On peut diriger le  bytecode (c'est-à-dire les fichiers .class) vers
  un répertoire donné: 
 
  - ``mkdir build`` 
  - ``javac *.java -d build/`` 

- On peut exécuter une classe qui se trouve dans un répertoire différent de celui
  dans lequel on se trouve en modifiant le *classpath* (option ``-cp``): 
 
  - ``java -cp build HelloWorldApp``

- Comme on a séparé fichiers sources et bytecode, on peut nettoyer le projet
  en supprimant le répertoire ``build``.  



Les bases du langage
==========================

Adresses utiles
----------------------------

Pensez à consulter régulièrement  

- des `tutoriaux <http://docs.oracle.com/javase/tutorial/java/>`_
- des `foires aux questions <http://java.developpez.com/faq/java/>`_
- `l'API standard <http://docs.oracle.com/javase/7/docs/api/>`_

Utilisez des marques-pages. 

Types primitifs 
-----------------------------

- void 
- boolean
- char (16-bit, Unicode)
- byte (8-bit)
- short (16-bit)
- int (32-bit)
- long (64-bit)
- float (32-bit)
- double (64-bit)

Opérateurs
---------------------------

- arithmétiques: ``+`` ``-`` ``*`` ``/`` ``%``
- relationnels: ``==`` (égalité) ``<=`` ``>=`` ``<`` ``>`` ``!=`` 
- logiques: ``!`` (non) ``&&`` (et) ``||`` (ou)
- incréments: ``++`` ``--``
- opérateurs sur les bits: ``&`` ``|`` ``^`` ``~`` ``>>`` ``<<``  
- affectation: ``=`` ``-=`` ``+=`` ``*=`` ``/=`` 
- conditionnel: ``?:`` 

Instructions et blocs
---------------------------------

- une expression est un assemblage de variables, d'opérateurs (et d'appels de méthodes), 
  évaluée en une valeur.  

- une instruction peut être: 
  
  - une déclaration: ``int x;``  
  - une expression: ``x = 2;`` 
  - une structure de contrôle: if, switch (sur types primitifs), for, while, do while

- un bloc d'instructions est délimité par des accolades. La portée d'une variable est 
  celle du bloc dans lequel elle est déclarée. 

Tableaux
-------------------

- Déclaration

.. literalinclude:: code/TD1/Tableau.java
   :language: java
   :lines: 3-4

- Création/Initialisation

.. literalinclude:: code/TD1/Tableau.java
   :language: java
   :lines: 6-9

- Utilisation

.. literalinclude:: code/TD1/Tableau.java
   :language: java
   :lines: 11-12

Ex.2. Arguments (10 min) 
--------------------------

- Ecrivez une classe ``ArgumentsApp`` qui affiche à l'écran les
  arguments passés à l'exécutable. Si aucun argument n'est passé, 
  un message avertit l'utilisateur. 
  
  Bien sûr, vous utiliserez le paramètre obligatoire ``args`` de ``main``,
  qui est de type ``String[]`` (tableau de chaînes de caractères).

.. literalinclude:: code/TD1/ArgumentsApp.java
   :language: java
   :lines: 2, 12


Le jeu des différences
-------------------------------

Tout ça ressemble au **C**. Mais en y regardant de plus près, certains
détails laissent penser que **Java** génère un monde un peu différent,  
peuplé d'objets: 

- un tableau est un objet (avec notamment un champs ``length``),
- les chaines de caractères de type ``String`` sont des objets (qui savent s'afficher sur la sortie standard),
- le flux de sortie standard ``out`` est un objet, lui-même champs de ``System``, à qui on peut demander de réaliser un affichage (par ``println``). 

Programmation orienté-objet
=============================

Introduction
--------------------------

Dans ce `petit exemple introductif <http://www.sebsauvage.net/comprendre/objet/index.html>`_, 
on comprend qu'on va décrire les tâches de la machine plus seulement comme une liste d'instructions,
mais comme un système dynamique d'objets qui inter-agissent. 

A l'exécution, sont crées en mémoire un ensemble d'objets. Chaque objet possède un état (qui peut changer) et des opérations
qu'il sait réaliser. Les objets inter-agissent en s'adressant des requêtes les uns aux autres sur le mode "je te demande de faire 
telle opération". 


Classe
---------------------------

Le programmeur va définir les caractéristiques d'une famille d'objets en écrivant une **classe**. 

La classe a

- un **nom**
- des **membres**

  - les **champs** (= attributs) décrivent la structure de l'état des objets 
  - les **méthodes** décrivent les opérations que savent réaliser les objets 


.. _Interrupteur-label:

Ex.3. Interrupteur/Classe (5 min)
--------------------------------------

Copiez et compilez.  

.. literalinclude:: code/TD1/Interrupteur.java
   :language: java
   :lines: 1-9

Attention, cette classe n'est pas exécutable, car 
elle ne contient pas de ``main``, point d'entrée 
obligatoire de toute classe exécutable.  


Instanciation
-------------------------

Un objet est manipulé via une variable dont le **type** porte le nom de sa classe. Cette variable contient une **référence** vers la zone mémoire allouée pour l'objet. 

.. code-block:: java

        //déclaration d'une référence (aucun objet n'est créé)
	Interrupteur unInterrupteur; 

Pour créer en mémoire un nouvel objet (= **instance**), on utilise l'opérateur ``new``, suivi de l'appel à une méthode portant le nom de la classe et appelée **constructeur**. 
Si aucun constructeur n'est écrit par le programmeur, celui-ci est automatiquement créé à la compilation. 

.. code-block:: java

	//création de l'objet, référencé par la variable unInterrupteur
	unInterrupteur = new Interrupteur();  


Valeurs par défauts
---------------------------------

Les *champs* d'une classe ont tous une valeur par défaut:

- boolean 	false
- char 	`\\u0000` (null)
- byte 	(byte)0
- short 	(short)0
- int 	0
- long 	0L
- float 	0.0f
- double 	0.0d
- tout objet 	null

NB: Seulement les champs, pas les variables locales. 


Initialisation des champs
--------------------------------

La première fois que la classe ``Interrupteur`` est impliquée dans l'éxécution du programme (ex. un objet de type ``Interrupteur`` est créé), 
la machine virtuelle charge ``Interrupteur.class`` en mémoire. 

Lorsque l'on crée un nouvel objet de type ``Interrupteur``, suffisamment d'espace mémoire est alloué et mis à zéro (``estEnMarche`` contient sa valeur par défaut: ``false``). 

Enfin, les champs sont initalisés dans l'ordre de déclaration (on affecte ``false`` à ``estEnMarche``, ce qui est inutile, mais rend le code plus lisible), 
avant que le constructeur (ici, par défaut) soit appelé. 


Interaction entre objets 
--------------------------

Une fois qu'un objet est créé, un objet tiers (= **client**) peut lui envoyer des **requêtes** (= message), 
c'est-à-dire
 
- soit lire/modifier un champs 

.. code-block:: java

        unInterrupteur.estEnMarche = true; 
	System.out.println( "est sur ON ?" + unInterrupteur.estEnMarche ); 

- soit lui demander de réaliser une opération qu'il sait faire. 

.. code-block:: java

        unInterrupteur.basculer(); //je lui demande de changer d'état 
	System.out.println( "est sur ON ?" + unInterrupteur.estEnMarche ); 


Ex.4. Interrupteur/Test (10 min)
-------------------------------------

- Ecrivez le code client dans un fichier ``InterrupteurTest.java`` qui, 
  en utilisant le champs ``estEnMarche`` et la méthode ``basculer()``, teste: 
 
  - que l'interrupteur est à l'état d'arrêt à sa création. 
  - qu'il est à l'état de marche après la bascule. 

.. literalinclude:: code/TD1/InterrupteurTest.java
   :language: java
   :lines: 1-5, 17-18


Ce qu'il faut retenir
-------------------------

- Dans le code client: 

  - on peut créer un objet en mémoire (par le constructeur précédé de l'opérateur ``new``),
  - mais on ne les détruit pas: la machine virtuelle possède un **garbage collector** qui s'en charge.  
  - l'objet est référencé par une variable dont le type est le nom de la classe,  
  - on peut adresser des requêtes à un objet pour lire/modifier son état ou activer un de ses comportements,

 
Copie d'objets
==========================

Ex.5. Affectation (5 min)
-----------------------------

Comparez ce que font ces deux blocs (dans un fichier ``DemoAffectation.java``). 

.. code-block:: java

        boolean b1 = false; 
	boolean b2 = b1; 
	b2 = !b2; 
	System.out.println( b1 ); 

.. code-block:: java

        Interrupteur i1 = new Interrupteur(); 
	Interrupteur i2 = i1; 
	i2.basculer(); 
	System.out.println( i1.estEnMarche ); 

Ex.6. Passage de paramètres (5 min)
------------------------------------------
 
Comparez ce que font ces deux fonctions (dans un fichier ``DemoPassageParametres.java``). 

.. code-block:: java

        static void faireBasculerBooleen(boolean unBool) {
	   unBool = !unBool;  
        }


.. code-block:: java

        static void faireBasculerInterrupteur(Interrupteur unInterrupteur) {
	   unInterrupteur.basculer(); 
        }


Ce qu'il faut retenir
-------------------------------

- L'affectation **copie** le contenu d'une variable dans une autre. 

- Les passages de paramètres se font aussi par **copie**. 

- Copie des **valeurs** pour les variables de type primitif.
  
- Mais copie des **références** (et pas des objets eux-même) pour les variables de type personnalisé. 

Ce que vous devez savoir faire
---------------------------------

- Classer java parmi les langages pseudo-compilé avec portabilité du bytecode.  
- Compiler un projet java et lancer une classe exécutable en modifiant si besoin le classpath. 
- Résoudre un problème simple à l'aide de tableaux et de structures de contrôle.
- Comparer les variables de types primitifs et les objets dans une affectation ou un passage de paramètres. 
