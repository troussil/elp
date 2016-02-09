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

- La classe ``App.java`` a été créée par défaut. Que fait elle ?

- Depuis le répertoire ``Hanoi``, compilez avec ``mvn compile``. 
  Observez la hiérarchie de répertoires avec ``ls -R`` ou ``tree``. 
  Où est ``App.class`` ?

- Expliquez pourquoi on peut exécuter cette classe en tapant 
  ``java -cp target/classes/ tc.elp.java.datastructures.App``.

- Que fait ``mvn clean`` ?

Configurer la version du compilateur
-------------------------------------

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

Organisation des fichiers
------------------------------------

A partir de maintenant, sauf mention contraire, 
les classes de l'application appartiendront au package ``tc.elp.java.datastructures``. 
Leur fichiers sources ``.java`` seront donc situés dans le répertoire 
``src/main/java/tc/elp/java/datastructures``. 


Un tableau dynamique (45 min)
==============================

.. _DynamicArray-label:


Classe ``DynamicArray``
----------------------------------

- Ecrivez une classe ``DynamicArray``dans le répertoire ``src/main/java. qui implémente un tableau d'entiers
  dynamique auquel on peut ajouter des valeurs autant qu'on veut. Par composition, 
  il contiendra un champs de type tableau. L'idée est d'allouer un tableau dont 
  la longueur est une puissance de 2, en commençant par 1 à la construction. 
  A chaque fois que la taille du tableau est dépassée lors de l'ajout d'un nouvel élément, 
  un nouveau tableau de taille deux fois plus grande est utilisé.  

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
- La méthode ``push`` ajoute un nouvel élément à l'indice ``mySize``.  

``TestDynamicClass``
-------------------------------------

- Créez du code client pour garantir le bon fonctionnement des instances de ``DynamicArray``. 
  Le fichier source ``.java`` pourra avantageusement appartenir au répertoire 
  ``src/test/java/tc/elp/java/datastructures``. 

- Dans une instance de ``DynamicArray``, ajoutez avec ``push`` les 256 premiers entiers
  et assurez-vous que la capacité maximale est atteinte à chaque puissance de 2 avec 
  ``capacity`` et ``size``. Vérifiez le contenu du tableau avec ``get``. 


Deux implémentations de piles (45 min)
========================================


.. _Stack-label:

Interface ``Stack``
----------------------------------

- Créez une interface appelée ``Stack`` composée des méthodes suivantes: 
 
  -  ``boolean empty()`` (indique si la pile est vide) 
  -  ``int top()`` (renvoie l'élément du dessus) 
  -  ``void push(int aValue)`` (ajoute une élément au-dessus) 
  -  ``void pop()`` (supprime l'élément du dessus) 

Test
-----------------------------------

Téléchargez le test 


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




Code client
===========================

Hanoi (15 min)
------------------------




Jar (15 min)
========================

Qu'est-ce que c'est ?
----------------------------

Un fichier ``.jar`` est une archive compressée de classes compilées (fichiers ``.class``)

- l'archive peut être téléchargée d'un coup

- l'archive peut être signée par l'auteur

- l'archive peut être exécutable

Cela facilite donc le déploiement et la diffusion d'un projet. 

.. _Hanoi-label:

Ex.1. Jar non exécutable (15 min)
------------------------------------

- Créez un répertoire ``ExHanoiJar``, contenant un répertoire ``build`` (vide) et 
  ``src``, contenant :download:`Hanoi.java <download/Hanoi.java>` et les cinq 
  fichiers relatifs aux piles ``StackByArray.java``, 
  ``DynamicArray.java``, ``StackByLinkedList.java``, ``LinkedListNode.java``, 
  ``Stack.java``. 

- Depuis ``ExHanoiJar``, compilez en tapant ``javac src/*.java -d build/``.

- Dans le répertoire ``build``, créez une archive
  en tapant ``jar -cvf monArchive.jar *.class`` 

- Vérifez le contenu de l'archive générée avec la commande ``jar -tvf``.  
  Essayez ``jar -xvf`` pour désarchiver. 


Ex.5. Librairie/Hanoi (5 min) 
------------------------------

- Depuis ``ExPackage``, compilez ``Hanoi.java`` avec ``javac -cp monArchive.jar Hanoi.java``. 

- Exécutez avec ``java -cp build/monArchive.jar:. Hanoi``. Attention au ``:.``. 

- NB: L'option **-classpath** (= **-cp**) des commandes **javac** et **java** accepte 
  non seulement des chemins, mais aussi des archives jar. Le ``:`` permet de séparer deux 
  chemins ou archives jar. Attention l'ordre importe: 
  l'option *-cp* doit se trouver avant le fichier à compiler ou la classe à exécuter.   


Ex.2. Jar exécutable (5 min)
-----------------------------------

- Dans le répertoire ``build``, créez une archive exécutable 
  en tapant ``jar -cvfe monArchive.jar Hanoi *.class`` 

NB. L'option ``e`` permet de spécifier la classe à exécuter (= *entrypoint*). 

- Tapez ``java -jar monArchive.jar`` pour l'exécuter.


Pour aller plus loin
========================

Pile générique
-------------------------

