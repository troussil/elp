===========================
Réflexivité 
===========================

Inspection des classes 
===========================

Java est un langage réflexif
----------------------------------------

Java est un langage *réflexif* : il donne la possibilité pour un 
programme en cours d'exécution d'examiner son propre état et d'agir
sur ce dernier. 
 

Cette caractéristique de Java est très utilisée notamment

- pour inspecter du code
  (dans les environnements de développement ou pour sérialiser un
  objet en JSON), 

- pour instancier des classes ou appeler des méthodes dynamiquement
  (ce que font les frameworks comme Spring ou Hibernate).

Un exemple du langage
---------------------------

Toutes les classes dérivent par défaut de ``Object``, qui possède une méthode ``toString``.
Par conséquent, tous les objets sont transformables en chaîne de caractère et affichables. 

Le message affiché par défaut contient le nom de la classe, car la méthode ``toString`` de
``Object`` retourne une chaîne de caractère donnée par:  

.. code-block:: java 

	getClass().getName() + '@' + Integer.toHexString(hashCode()); 



La classe ``Class``
----------------------------------------

Le package ``java.lang`` comporte une classe appelée ``Class``. Au chargement d'une classe, la machine virtuelle 
crée automatiquement une instance de ``Class`` permettant d'obtenir tous les renseignements possibles concernant la 
classe chargée.  

On obtient une instance de ``Class`` de trois manières différentes : 

.. code-block:: java 

        Class<String> classe
	  = (Class<String>) Class.forName("java.lang.String"); //by name

.. code-block:: java 

        String aString = "azerty"; //from an object
	Class<String> classe = (Class<String>) aString.getClass();
   
.. code-block:: java 
	
	Class<String> classe = String.class; //from a class literal


Généricité
-----------------------------------

Comme vous le voyez, depuis java 5, la classe ``Class`` est paramétrée par un type.
Par exemple, ``Class<String>`` est le type de l'objet créé
par la machine virtuelle au chargement de la classe ``String``. 

.. code-block:: java 

        Class<String> classe
	  = (Class<String>) Class.forName("java.lang.String");

	  
Mais parfois ce type ne peut être connu lors de l'écriture du programme. Dans ce cas,
on peut utiliser le joker ``?`` :


.. code-block:: java 

        Class<?> yourClass = Class.forName(args[0]);  


Les méthodes de ``Class``
----------------------------------------

La classe ``Class`` contient des méthodes qui retournent des instances de
classes/interfaces définies dans le package ``java.lang.reflect``.

- ``Field[] getDeclaredFields()`` : renvoyer un tableau de tous les champs de la classe
- ``Method[] getDeclaredMethods()`` : renvoyer un tableau de toutes les méthodes
- ``Constructor[] getDeclaredConstructors()`` : renvoyer tous les constructeurs
- ``int getModifiers()`` : renvoyer un entier qu'il faut décoder pour connaître les modificateurs de la classe

Liste non exhaustive, consultez la `documentation <https://docs.oracle.com/javase/7/docs/api/java/lang/Class.html>`_.


Ex.1. Inspecteur (5 min)
----------------------------

- Téléchargez la classe :download:`XRayClass <download/XRayClass.java>`. 

- Lisez le code de la classe. Compilez et exécutez-la en passant divers paramètres
  (par exemple, ``java.lang.String methods`` ou ``java.lang.Integer constructors``). 

- Note 1 : ``declared`` renvoie à tout ce qui est déclaré dans la classe, quelle que soit la visibilité. 
  Sinon, ce sont seulement les membres publics qui sont retournés.

- Note 2 : depuis java 5, vous pouvez utiliser une construction *for each* pour parcourir un tableau : 

.. code-block:: java 

	for (int elt: tab) { //tab est de type int[]
	    System.out.println( elt ); 
	}

	  
Code dynamique
=================================


Instancier une classe
--------------------------

.. code-block:: java 

        Class<MaClasse> classe = (Class<MaClasse>) Class.forName(nomClasse);
        MaClasse instance = classe.newInstance();

La méthode ``newInstance()`` de la classe ``Class`` présente plusieurs contraintes :

- seul le constructeur sans paramètre est appelé,
- ce constructeur doit donc être déclaré comme *public*,
- et toutes les exceptions (*checked* et *unchecked*) levées lors de l'appel au constructeur sont propagées. 

Ex.2. Framework / Prise en main (5 min)
--------------------------------------------

- Téléchargez ce :download:`framework <download/reflexivite.tar.gz>`
  dans lequel on peut injecter son propre code.
  
- Ce framework est constitué d'une classe comportant un ``main``,
  qui ne doit pas changer, et d'une interface ``JeuCombinatoire``, qu'il
  s'agit d'implémenter pour injecter ses propres jeux dans le framework.
  Ces deux classes appartiennent au package
  ``fr.insalyon.tc.framework``. En plus, le framework est distribué
  avec le jeu de Wythoff. Regardez, compilez, testez. 

Ex.3. Framework / Jeu de Nim (10 min)
--------------------------------------------

- Sans nécessairement fermer l'application, ajoutez dans le répertoire ``jeux``
  une classe ``Nim`` implémentant ``JeuCombinatoire``.

- NB. Le jeu de Nim est un jeu combinatoire à deux joueurs qui enlèvent
  alternativement 1, 2 ou 3 éléments d'un tas de 15 éléments au depart.
  Le joueur qui ne peut plus jouer car le tas est vide a perdu. 

Ex.4. Framework / Ensemble de jeux (30 min)
--------------------------------------------

- Sans nécessairement fermer l'application, ajoutez dans le répertoire ``jeux``
  une classe ``EnsembleDeJeux`` implémentant ``JeuCombinatoire``.

- Il s'agit de construire un jeu combinatoire composé de tous les jeux  
  combinatoires se trouvant dans le répertoire ``jeux``.   
  Les deux joueurs jouent alternativement en choisissant un jeu 
  et un coup pour ce jeu. Quand il n'y a plus la possibilité
  de jouer dans un jeu, la partie se poursuit sur les autres
  jeux. Le joueur qui ne peut plus jouer dans le dernier jeu
  non fini a perdu.

- Vous cherchez quelque chose ? Quelques trucs suivent. 
  
Quelques trucs pour aller plus vite
-------------------------------------------

Obtenir le répertoire du classpath :  

.. code-block:: java 

	String nomRepertoire = System.getProperty("java.class.path");

Obtenir la liste des fichiers d'un répertoire : 	

.. code-block:: java 

	File repertoire = new File(nomRepertoire); 
        for (String nom : repertoire.list()) {}

Traitement des chaines de caractère :
	
.. code-block:: java 

	...
	String[] decomposition = chaine.split(separateur);
	boolean flag = chaine.endsWith(extension); 
	String sousChaine = chaine.substring(pos1,pos2);
	int n = Integer.parseInt(chaine);
	...

Note sur les paramètres
--------------------------------

Il est possible d'instancier une classe par un constructeur possédant
des paramètres. Dans ce cas, il s'agit de récupérer le constructeur
qu'on souhaite utiliser à partir de sa signature et de sa classe, puis
d'instancier la classe en fournissant la liste des arguments.

Selon le même procédé, il est possible de récupérer une méthode, puis
de l'appeler.

Consultez la `documentation <https://docs.oracle.com/javase/7/docs/api/java/lang/Class.html>`_.

Conclusion
==========================

Injection de dépendances
--------------------------

Dans un projet, il y a des *dépendances statiques*, c'est-à-dire des classes (ou interfaces)
qui apparaissent dans le code d'une classe :

- par héritage (``extends NomClasse``), implémentation d'interface (``implements NomInterface``)
- déclaration (``NomClasse nomObjet``), 
- et surtout par création d'objets avec ``new NomClasse(...)``.

L'injection de dépendances consiste principalement à supprimer ce dernier type de dépendance statique,
très forte, en injectant à l'exécution les classes apropriées à l'aide de la réflexivité.   
Bref, **on supprime les ``new``**.

Le coût
--------------------------

La réflexivité de Java contribue à sa flexibilité et à réduire les dépendances, 
mais elle a un coût à prendre en compte pour envisager son usage, car

- l'appel aux méthodes réflexives (``getMethod``, ``newInstance``, etc.) ont un surcoût,
- et le code exploitant la réflexivité est moins lisible et plus complexe à comprendre que lorsque les appels sont directs.

Bref, la réflexivité ne devrait être utilisé que lorsqu'aucune autre forme de programmation n'est appropriée. 

Capacités/connaissances
---------------------------------

- Expliquez le mécanisme de réflexivité en Java.
- Citer des exemples d'utilisation de la réflexivité en Java.
- Savoir récupérer le nom et la structure (champs, méthodes) de la classe d'un objet donné.
- Savoir instancier une classe dont le nom est donné à l'exécution. 
