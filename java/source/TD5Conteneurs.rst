===========================================
Conteneurs
===========================================

Conteneurs
==========================

Résumé du package
---------------------------------------

Le package ``java.util`` regroupe des classes relatives à la gestion du temps, 
à la gestion des événements, ainsi qu'aux conteneurs (collections et tableaux associatifs). 

Un **conteneur** est un ensemble générique d’objets 
(jeu de cartes, répertoire de fichiers, sac de perles, annuaire téléphonique, dictionnaire, etc.). 

L'efficacité du codage particulier d'une collection dépend de ses caractéristiques (ordre, doublons, etc.) 
et des opérations envisagées (parcours, ajout, suppression, concaténation, etc.). 


Conteneurs
---------------------------------------

Le package ``java.util`` propose les conteneurs suivants:
 
- ``ArrayList``, ``ArrayDeque``
- ``LinkedList``
- ``HashMap``, ``HashSet``
- ``TreeMap``, ``TreeSet``. 

(Cette liste n'est pas exhaustive).

On utilise le plus souvent ``ArrayList``, ``HashSet``, ``HashMap``. 

Ex.1. Conteneurs (10 min)
--------------------------------

- Parcourez la `documentation <http://docs.oracle.com/javase/7/docs/api/>`_ relative aux conteneurs citées. 

- Quelle est la différence entre ``Collection``, ``List``, ``Set``, ``Map``?

- Quelle est la différence entre ``ArrayList`` et ``LinkedList``?

- Quelle est la différence entre ``HashMap`` et ``TreeMap``?

.. acces aléatoire sur un tableau de taille variable, liste doublement chainée sans acces aléatoire

.. collection ordonnée - ensemble sans doublons - ensemble de paires clé-valeurs, les clés étant uniques -

.. table de hachage : acces constant en moyenne, pas d'ordre fixe - arbre rouge et noir : acces en O(log n) + ordre sur les clefs

L'interface ``Collection``
-----------------------------------

Les interfaces ``List`` et ``Set`` dérivent de ``Collection``, 
dérivant elle-même de ``Iterable``. 

Elle exige la présence, entre autres, des méthodes suivantes:  

- ``contains(Object o)``
- ``isEmpty()``
- ``toArray()``
- ``iterator()``

La méthode ``iterator()``, requise par ``Iterable``, renvoie un objet issu d'une classe implémentant 
l'interface ``Iterator``, avec lequel on peut parcourir la collection. 

L'interface ``Iterator``
-----------------------------------
  
Un objet de type ``Iterator`` offre un moyen de parcourir la collection à laquelle il réfère:  

.. code-block:: java 

	Collection<Integer> c = new ArrayList<Integer>(); 
	...
	for (Iterator<Integer> it = c.iterator(); it.hasNext(); ) {
	    System.out.println( it.next() ); 
	}

Depuis java 5, vous pouvez utiliser ce raccourci: 

.. code-block:: java 

	for (Integer i: c) {
	    System.out.println( i ); 
	}


Généricité
-----------------------------------

Comme vous le voyez, depuis java 5, les classes et les interfaces peuvent être paramétrées par un type. 

.. code-block:: java 

	public class StackByLinkedList<E> { 
	    ...
	}

Lors de la déclaration, le type formel est remplacé par le type effectif (non primitif). 

.. code-block:: java 

	StackByLinkedList<Integer> s = new StackByLinkedList<Integer>(); 

Le fonctionnement est le même que celui du passage de paramètres pour une méthode. 

L'interface ``Map``
-----------------------------------

L'interface ``Map`` décrit des objets qui mettent en correspondance des clés et des valeurs 
(à une clé étant associé au plus une valeur). 

En plus de la méthode ``get()`` renvoyant la valeur associée à une clé donnée, elle offre 
trois vues de type ``Collection``: 

- l'ensemble de clés est renvoyé par la méthode ``keySet()``, 
- la collection de valeurs est renvoyé par la méthode ``values()``, 
- l'ensemble de paires clé-valeur est renvoyé par la méthode ``entrySet()``.

L'interface ``Map.Entry``
-----------------------------------

.. code-block:: java 

	Map<Integer,String> annuaire = new HashMap<Integer,String>(); 

L'interface ``Map.Entry`` représente une paire clé-valeur: 
``getKey()`` retourne la clé, tandis que ``getValue()`` retourne la valeur. 

.. code-block:: java 

        Iterator<Map.Entry<Integer,String> > it;  
	for (it = annuaire.entrySet().iterator(); it.hasNext(); ) {
	    Map.Entry<Integer,String> e = it.next();
            System.out.println(e.getKey() + ": " + e.getValue());  
	}

.. code-block:: java 

	for (Map.Entry<Integer,String> e: annuaire.entrySet()) {
            System.out.println(e.getKey() + ": " + e.getValue());  
	}

Ex.2. Etudiants (15 min)
-----------------------------------

- Téléchargez la classe :download:`Etudiant <download/Etudiant.java>`.  

- Créez une classe ``GroupeEtudiant`` qui possède la méthode ``void ajout(Etudiant e)``. 
  Les étudiants sont stockés dans un ``ArrayList``. 

- Dans le ``main`` d'une classe ``ClientEtudiant``, ajoutez des objets de la
  classe ``Etudiant`` à la classe ``GroupeEtudiant``. 
  Au moins un de ces objets aura comme nom `Toto`. 

- Dans la classe ``ClientEtudiant``, rechercher `Toto`, en appelant la méthode
  ``List<Etudiant> recherche(String nom)`` codée dans ``GroupeEtudiant``. 
  Quelle est la complexité de la méthode de recherche en temps et en espace ?

Ex.3. MultiMap (15 min)
--------------------------

- Créez une classe ``MultiMap<K,V>`` qui associe à une clée de type ``K`` plusieurs valeurs
  de type ``V`` stockées dans une liste. Elle devra implémenter l'interface ``Map<K,List<V> >``
  et posséder une méthode ``void putOneValue(K,V)`` qui s'occupe de l'insertion d'une nouvelle valeur
  et ``boolean containsOneValue(V)`` qui retourne vraie si la valeur donnée se trouve dans le conteneur,
  faux sinon. 
- Testez votre classe dans du code client. 
  

Ce qu'il faut retenir
------------------------------------

- Un conteneur est un ensemble générique d'objets.  

- On distingue deux types: 

  - ``Collection`` dont dérivent:

    - ``List`` (ensemble ordonné)
    - ``Set`` (ensemble sans doublon)

  - ``Map`` (ensemble de paires clés-valeurs) 

- ``Collection``, ``List``, ``Map``, ``Set`` sont des interfaces, seules les classes concrètes
  qui les implémentent sont instanciables. 

- Les objets de type ``Collection`` sont parcourus d'une manière uniforme
  (iterator ou boucle for étendue). Les objets de type ``Map`` offrent trois vues
  de type ``Collection``.    


Ce que vous devez savoir faire
---------------------------------

- Connaître la complexité théorique des opérations principales pour un conteneur donné. 
- Choisir un conteneur pour résoudre un problème donné. 
- Stocker une collection d'objets dans un conteneur et la parcourir.  
