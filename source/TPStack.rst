===========================================
TP "structures de données"
===========================================


Pour commencer (30 min)
==========================


Maven
--------------------------

- C'est un outil de gestion de production de projets Java. 
- Le projet est décrit dans un fichier appelé ``pom.xml`` (*Project Object Model*).
- La commande est **mvn**. Il existe un grand nombre de *buts* prédéfinis: 
  compile, test, package, clean, site, etc. 
- Les dépendances sont gérées automatiquement, avec téléchargements depuis 
  le site Maven.  

Consultez `http://maven.apache.org/ <http://maven.apache.org/>`_. 
 
Sur les machines du département
---------------------------------

Maven télécharge automatiquement tous plugins nécessaires pour son propre 
fonctionnement ou pour le fonctionnement de votre code. 

Les fichiers sont stockés par convention sur ``$HOME/.m2``. Pour éviter de 
surcharger votre répertoire personnel, avant tout, faites un lien symbolique
vers ``/tmp``: 

- ``mkdir /tmp/.m2``
- ``ln -s /tmp/.m2 $HOME/.m2``
- Vérifiez avec ``ls -a $HOME``. 

Création d'un projet
-----------------------------

- Pour créer un nouveau projet, tapez 
  ``mvn archetype:generate`` suivi des options suivantes: 

  - ``-DgroupId=tc.elp.java.datastructures`` (nom de l'organisation, à partir duquel le nom du package est déduit)
  - ``-DartifactId=Hanoi`` (nom du projet) 
  - ``-DarchetypeArtifactId=maven-archetype-quickstart`` (crée un ``pom.xml`` par défaut)
  - ``-DinteractiveMode=false`` 

- Observez la hiérarchie de répertoires avec ``ls -R``, ainsi que le ``pom.xml``. 


Compilation
-------------------------------

- La classe ``App.java`` a été créée par défaut. Que fait-elle ?

- Depuis le répertoire ``Hanoi``, compilez avec ``mvn compile``. 
  Observez la hiérarchie de répertoires avec ``ls -R`` ou ``tree``. 
  Où est ``App.class`` ?

- Expliquez pourquoi on peut exécuter cette classe en tapant 
  ``java -cp target/classes/ tc.elp.java.datastructures.App``.

- Que fait ``mvn clean`` ?

Tests
---------------------------------

- Pour tester nos classes nous allons utiliser le framework JUnit. 

- Depuis le répertoire ``Hanoi``, compilez avec ``mvn test-compile``. 
  Observez la hiérarchie de répertoires avec ``ls -R`` ou ``tree``. 
  Où est ``AppTest.class`` ?

- Exécutez avec ``mvn test``. 

Configurer les versions de Java, JUnit
-----------------------------------------

Pour utiliser Java 5 (annotations), dans ``pom.xml``, ajoutez ceci entre 
les balises ``project``:  

.. code-block:: xml

        <build>
          <plugins>
            <plugin>
              <groupId>org.apache.maven.plugins</groupId>
              <artifactId>maven-compiler-plugin</artifactId>
              <version>3.2</version>
              <configuration>
                <source>5</source>
                <target>5</target>
              </configuration>
            </plugin>
          </plugins>
        </build> 

De plus, pour les tests, corrigez la version de JUnit en 4.2. 

Organisation des fichiers
------------------------------------

A partir de maintenant, sauf mention contraire, 
les classes de l'application appartiendront au package ``tc.elp.java.datastructures``. 
Leur fichier source d'extension ``.java`` sera donc ajouté au répertoire 
``src/main/java/tc/elp/java/datastructures``. 


Un tableau dynamique (45 min)
==============================

.. _DynamicArray-label:


Objectif
-------------------------------------

- Dans cette partie, l'objectif est d'écrire une classe ``DynamicArray`` qui 
  implémente un tableau d'entiers dynamique auquel on peut ajouter autant 
  d'entiers qu'on veut à droite (dans les cases de plus grands indices).

- Pour tester, téléchargez :download:`TestDynamicArray.java <download/TestDynamicArray.java>`
  et ajoutez-le au répertoire 
  ``src/test/java/tc/elp/java/datastructures``. 


Classe ``DynamicArray``
----------------------------------

- La classe ``DynamicArray`` repose sur un champs de type tableau. L'idée est d'instancier 
  un tableau dont la longueur est une puissance de 2, en commençant par 1 à la construction. 
  A chaque fois que la taille de ce tableau est dépassée lors de l'ajout d'un nouvel élément, 
  un nouveau tableau de taille deux fois plus grande est créé puis utilisé.  

.. figure:: figs/DynamicArray.*
   :alt: diagramme UML de la classe DynamicArray
   :align: center
   :scale: 50%

Détails d'implémentation
-----------------------------------

- Remarquez que ``size`` (stocké dans ``mySize``) désigne le nombre d'éléments mémorisés, 
  tandis que ``capacity`` désigne la taille du tableau sous-jacent (= ``myArray.length``): 
  c'est toujours une puissance de 2.


.. figure:: figs/DynamicArrayFig.*
   :alt: illustration de l'implémentation de la classe DynamicArray
   :align: center
   :scale: 50%

- La méthode ``get`` retourne l'élément se trouvant à l'indice donné. 
- La méthode ``push`` ajoute un nouvel élément à l'indice ``mySize``
  quand ce dernier n'est pas donné.  



Implémentation de piles (45 min)
========================================


.. _Stack-label:

Interface ``Stack``
----------------------------------

- Créez une interface appelée ``Stack`` composée des méthodes suivantes: 
 
  -  ``boolean empty()`` (indique si la pile est vide) 
  -  ``int top()`` (renvoie l'élément du dessus) 
  -  ``void push(int aValue)`` (ajoute un élément au-dessus) 
  -  ``void pop()`` (supprime l'élément du dessus) 

Objectif 
-----------------------------------

- L'objectif de cette partie est d'implémenter deux structures de piles. 
  L'une de type liste chaînée, l'autre basée sur la classe ``DynamicArray``
  précédente. 

- Pour tester, téléchargez le :download:`TestStack.java <download/TestStack.java>` et ajoutez-le au répertoire 
  ``src/test/java/tc/elp/java/datastructures``. 

Exception
----------------------------------

- Vous avez certainement vu que les méthodes ``top`` et ``pop`` sont mal définies
  dans le cas d'une pile vide. 

- Créez une classe ``EmptyStackException`` dérivant de ``java.lang.Exception``, 
  de façon à ce qu'une exception de ce type soit levée au moment opportun. 

.. _StackByLinkedList-label:

Classe ``StackByLinkedList``
-----------------------------------------

- Implémentez une pile d'entiers au moyen d'une liste chaînée dans une classe ``StackByLinkedList``. 
  Bien sûr, elle doit satisfaire l'interface ``Stack``. 

- Pour aboutir plus rapidemment, vous utiliserez la :download:`classe LinkedListNode <download/LinkedListNode.java>` 
  représentant un noeud de liste.  


.. _StackByArray-label:

Classe ``StackByArray``
----------------------------------

- Dérivez une nouvelle classe appelée ``StackByArray`` de ``DynamicArray``,  
  en veillant à ce que ``StackByArray`` satisfasse l'interface ``Stack``.



Code client (20 min)
===========================

Hanoi
------------------------

L'objectif est maintenant d'implémenter l'algorithme qui 
déplace les tours de Hanoi; chaque tour étant modélisée 
par une pile d'entiers. 

Bien sûr, vous devez veiller à ce que toute structure de données 
de pile (respectant l'interface ``Stack``) puisse être utilisé dans
votre code.  

Ce code sera écrit dans la classe ``Hanoi``, ajoutée au répertoire
``src/main/java`` (elle n'appartient pas au package ``tc.elp.java.datastructures``). 


Jar (20 min)
========================

Qu'est-ce que c'est ?
----------------------------

Un fichier ``.jar`` est une archive compressée de classes compilées (fichiers ``.class``)

- l'archive peut être téléchargée d'un coup

- l'archive peut être signée par l'auteur

- l'archive peut être exécutable

Cela facilite donc le déploiement et la diffusion d'un projet. 

.. _Hanoi-label:

Création d'une archive jar
------------------------------------

- Depuis le répertoire ``target/classes``, créez une archive
  en tapant ``jar -cvf monArchive.jar tc/elp/java/datastructures/*`` 

- Vérifez le contenu de l'archive générée avec la commande ``jar -tvf monArchive.jar``.  

- Créez un répertoire ``tmp``. Depuis ce répertoire, désarchivez avec ``jar -xvf ../monArchive.jar``. 


Utilisation d'un jar
------------------------------

- Copiez ``Hanoi.java`` et ``monArchive.jar`` dans le répertoire racine ``Hanoi``,  

- Compilez avec ``javac -cp monArchive.jar Hanoi.java``. 

- Exécutez avec ``java -cp monArchive.jar:. Hanoi``. Attention au ``:.``. 

- NB: L'option **-classpath** (= **-cp**) des commandes **javac** et **java** accepte 
  non seulement des chemins, mais aussi des archives jar. Le ``:`` permet de séparer deux 
  chemins ou archives jar. Attention l'ordre importe: 
  l'option *-cp* doit se trouver avant le fichier à compiler ou la classe à exécuter.   


Jar exécutable 
-----------------------------------

- Dans le répertoire racine ``Hanoi``, ajoutez à votre archive le fichier 
  ``Hanoi.class`` et spécifiez un point d'entrée pour la rendre exécutable, 
  en tapant ``jar -uvfe monArchive.jar Hanoi Hanoi.class``.

NB. L'option ``e`` permet de spécifier la classe à exécuter (= *entrypoint*). 

- Tapez ``java -jar monArchive.jar`` pour l'exécuter.

Maven et Jar
--------------------------------------


- Invoquez la commande ``mvn package`` qui génère un jar du projet.
- Listez les fichiers du jar, testez-le.
- Rendez-le exécutable en spécifiant un point d’entrée avec ``jar -uvfe``.

Rendu du travail
---------------------------------------

- Copiez votre archive jar exécutable dans le répertoire racine ``Hanoi``. 

- Nettoyez le reste du projet avec ``mvn clean``. 

- Copiez le répertoire ``Hanoi`` (et tout ce qu'il contient) dans un répertoire
  à vos noms (de la forme ``NomEtudiant1-NomEtudiant2``)

- Archivez et compressez ce répertoire en un fichier d'extension ``.tar.gz`` 
  que vous chargerez sur Moodle. 


Pour aller plus loin (45 min)
==============================

Généricité
-------------------------

Nous avons manipulé jusqu'à maintenant des tableaux et piles d'entiers. 
Mais aucune partie du code n'est spécifique au type ``int``; nous voudrions
donc avoir des structures de données dont on peut choisir le type des éléments. 

Depuis Java 5, les `generics <https://docs.oracle.com/javase/tutorial/java/generics/>`_
fournissent un mécanisme pour que les classes, interfaces, méthodes puissent 
être paramétrées par un type; les incohérences étant détectées à la compilation. 

Syntaxe
------------------------

On fait suivre le nom de la classe (ou interface) par le 
paramètre de type entre chevrons: 

.. code-block:: java

        class LinkedListNode<E> { ... }

Dans le code, ``E`` représente le type d'un élément: 

.. code-block:: java

        private E myValue; /// valeur d'un élément

Pour instancier, on donne en argument un type existant: 

.. code-block:: java

        LinkedListNode<Integer> node = new LinkedListNode<Integer>(); 

Le mécanisme de substitution est celui du passage de paramètres.  

Restriction
--------------------------

Il n'est pas possible d'avoir un type primitif comme argument: 

.. code-block:: java

        LinkedListNode<int> node = new LinkedListNode<int>(); //compile pas

En remplacement, on utilise les *wrappers* ``Boolean``, ``Character``, 
``Integer``, ``Long``, ``Float``, and ``Double`` 
qui héritent tous de ``Object`` (comme tout type non primitif), 
et ajoutent des services aux types primitifs (conversion, etc.).

.. code-block:: java

        LinkedListNode<Integer> node = new LinkedListNode<Integer>(); //ok 


Objectif
--------------------------

L'objectif de cette partie est de rendre générique l'interface ``Stack``
et les classes ``LinkedListNode``, ``StackByLinkedList`` (afin d'avoir
des piles dont le type d'élément est arbitraire). 

En revanche, comme tableaux et *generics* cohabitent mal, on exige simplement
de la classe ``StackByArray`` (qui dérive de ``DynamicArray``) qu'elle satisfasse 
l'interface ``Stack<Integer>``. 

Dans les tests et dans ``Hanoi``, on n'utilisera que des instances de classes 
respectant l'interface ``Stack<Integer>``.

NB. Pour éviter de tout casser, travaillez sur une copie de votre projet. 

Rendu du travail
---------------------------------------

- Si vous avez réussi, archivez et compressez votre projet (avec le jar exécutable)
  et chargez-le sur Moodle. 
