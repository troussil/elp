===========================================
Threads
===========================================

Introduction aux threads
==========================

Qu'est-ce que c'est ?
----------------------------------------

- Java propose un mécanisme pour exécuter concurremment (et simultanément pour une machine multiprocesseurs) plusieurs séquences d'instructions.  

- Chaque séquence est un **thread**. Les threads sont des processus léger qui s'exécutent dans le même espace d'adressage.  

- Toute application s'exécute d'abord (méthode ``main``) dans le thread initial. Pour en créer de nouveaux, elle instancie ``java.lang.Thread``, 
  puis lance le thread au moyen de la méthode ``start``.  

- Consultez la `documentation <http://docs.oracle.com/javase/7/docs/api/>`_  de ``java.lang.Thread`` (et ``java.lang.Object``)
  ainsi que le `tutoriel <http://docs.oracle.com/javase/tutorial/essential/concurrency/index.html>`_ sur la programmation concurrente. 


Comment créer un thread ?
----------------------------------------

Première façon de faire: dériver ``java.lang.Thread``. 

.. code-block:: java

        public classe MonPremierThread extends Thread {
	  public void run() { ... }
	} 

L'appel à la méthode ``start`` (héritée) lance l'exécution de la méthode ``run``. 
L'exécution du thread se termine au retour de la méthode ``run``. 

.. code-block:: java

        MonPremierThread t = new MonPremierThread(); 
	monPremierThread.start(); 


Comment éviter l'héritage ?
----------------------------------------

L'héritage de ``java.lang.Thread`` est contraignant car il empêche tout autre héritage. 
Il existe une seconde façon de faire: implémenter l'interface ``java.lang.Runnable``. 

.. code-block:: java

        public classe MaClasseRunnable implements Runnable {
	  public void run() { ... }
	} 

On passe une instance de la classe implémentant ``Runnable`` au constructeur de ``Thread``. 

.. code-block:: java

        MaClasseRunnable r = new MaClasseRunnable(); 
	Thread t = new Thread( r ); 
	t.start();

Cycle de vie d'un Thread
---------------------------------------

- Un thread est activé et prêt après l'appel de sa méthode ``start``. 
- Quand le *scheduler* lui donne la main, il exécute la méthode ``run``. 
- Il peut décider lui-même de rendre la main par la méthode ``yield``. 
- Sinon, il s'exécute tant que le *scheduler* ne le préempte pas (le suspend provisoirement). 
- Un thread est bloqué par une opération d'entrée-sortie, par les méthodes ``sleep`` ou ``wait``.
- Dans ces cas, il redevient prêt quand l'opération s'achève ou qu'il est réveillé (méthode ``notify``).   
 

Synchronisation temporelle
============================

Controle des threads
-----------------------------------------

- Méthode de ``java.lang.Thread``: 

  - ``start`` : active ce thread.
  - ``run`` : exécute ce thread.
  - ``interrupt`` : interromp ce thread. 
  - ``join`` : attend que ce thread se termine.   
  - ``sleep`` : endort ce thread durant un certain temps (en millisecondes).
  - ``yield`` : ce thread rends la main. 

Controle des threads
-----------------------------------------

- Méthodes de ``java.lang.Object``: 

  - ``wait``: met en attente le thread courant sur l'objet auquel la requête est adressée, jusqu'à ce qu'il soit 
    réveillé ou interrompu par un autre thread (ou jusqu'à ce qu'une durée donnée se soit écoulée). 
  - ``notify``: réveille le thread qui est en attente sur l'objet dont on appelle la méthode ``notify`` (s'il y en a
    plusieurs, l'un d'eux est choisi arbitrairement).   
  - ``notifyAll``: réveille tous les threads en attente sur l'objet.  



Problème producteur/consommateur
---------------------------------------

- Imaginons un producteur; il produit des objets et les entreprose. Mais il n'y a qu'une seule place. 

- Imaginons un consommateur; il retire l'objet entreposé.  

- Comment synchroniser leurs actions afin que le producteur 
  n'essaie d'entreproser un nouvel objet que lorsque la place est libre et que le consommateur 
  n'essaie de retirer un nouvel objet que lorsqu'un objet est disponible ? 


Ex.1. Wait/Notify (20 min)
---------------------------------------

- Téléchargez cette :download:`archive <download/ProducerConsumer.tar.gz>`. 

- Que fait la classe ``ProducerConsumerTest`` ? Compilez et exécutez. Que se passe-t-il ?

- Ecrivez une classe ``SyncCubbyHole``, qui étend ``CubbyHole`` et qui redéfinit les méthodes 
  ``get`` et ``put`` en les marquant ``synchronized`` et en appelant les méthodes ``wait`` et ``notify``. 


Ce qu'il faut retenir
----------------------------------------

- Tous les objets peuvent mettre en attente le thread courant avec ``wait``. 

- Tous les objets peuvent réveiller le(s) thread(s) bloqué(s) par eux, avec ``notify`` et ``notifyAll``. 


Synchronisation physique
==========================

Partage de la mémoire et verrou
--------------------------------------

Tous les threads ont accès au même espace mémoire. Quand les threads manipulent
une référence vers un même objet, il se peut que cela génère des erreurs. 
 
Pour les éviter, on verrouille un objet ou une méthode avec le mot-clef ``synchronized``. 
L'objet accédé via ce verrou implique que tout autre thread doit attendre la fin de cet 
accès pour y avoir accès à son tour.  

Ex.2. Compteur (10 min)
---------------------------------------

- Téléchargez la classe :download:`EvenCounter <download/EvenCounter.java>`. Que fait-elle ?

- Ecrivez la classe ``EvenCounterTest`` dans laquelle vous instanciez un seul objet de la classe 
  ``EvenCounter``, que vous exécutez dans deux threads.

- Compilez et exécutez plusieurs fois. Que se passe-t-il ? Pourquoi ? 

- Ajoutez le mot-clef ``synchronized`` à la méthode ``toNextEven``. Que se passe-t-il ?

.. code-block:: java

        private synchronized void toNextEven() {


Ex.3. Tableaux de threads (10 min)
---------------------------------------

- Téléchargez la classe :download:`Piscine <download/Piscine.java>`. Que fait-elle ?

- Téléchargez la classe :download:`Baigneur <download/Baigneur.java>`. Que fait-elle ?

- Ecrivez une classe ``BaigneursTest`` qui lance des threads opérant sur 150 instances de 
  la classe ``Baigneur``, chacune connaissant un seul objet de type ``Piscine``: 

.. code-block:: java

	Piscine piscine = new Piscine();    //la piscine
	int n = 150;
	Thread[] baigneurs = new Thread[n]; 
	for (int i = 0; i < n; i++)         //les baigneurs
	    baigneurs[i] = new Thread( new Baigneur(piscine, 5) ); 
 
- Compilez, puis exécutez plusieurs fois. Est-ce que ça fonctionne ?

Ex.4. Accès concurrents (10 min)
---------------------------------------

- Dans la classe ``Piscine``, ajoutez des sections critiques avec la construction suivante: 

.. code-block:: java

	  synchronized (this) {
             ...
          }

L'objet entre parenthèse est utilisé de manière exclusive par le thread courant. 
L'exécution des autres threads est bloquée jusqu'à ce que le thread courant exécute
la dernière instruction du bloc.


Ce qu'il faut retenir
---------------------------------------

Quand plusieurs threads partagent des données, il peut y avoir *interférence* 
(deux exécutions d'une même méthode sont entrelacées) ou *incohérence* 
(les appels de différentes méthodes d'un même objet sont entrelacés). 

Pour éviter ces problèmes, on peut définir des **sections critiques** avec 
le mot-clef ``synchronized``. 

- L'objet dont une méthode qualifiée ``synchronized`` est exécutée par un thread n'est plus disponible pour les autres threads.  

- Le bloc ``synchronized`` permet d'utiliser de manière exclusive un objet par le thread courant. 

Dans les deux cas, la synchronisation porte sur *un objet particulier*. 



Pour aller plus loin
============================


Fabrique de threads
-----------------------------

Le package ``java.util.concurrent`` contient une classe  ``Executors`` fabriquant: 

- un thread avec ``newSingleThreadExecutor()``

- un pool de threads en appelant ``newFixedThreadPool()``  

Ces méthodes renvoient en fait un objet de type ``ExecutorService``, sous-type de ``Executor``.
Autrement dit, un objet issu d'une classe implémentant l'interface ``ExecutorService``, 
dérivant l'interface ``Executor``.  

Executor 
-----------------------------

Les objets de type ``Executor`` possèdent une méthode ``execute()`` qui crée, 
puis démarre un thread. 

Si ``e`` est un objet de type ``Executor`` et si ``r`` est un objet de type ``Runnable``, 
alors ces codes sont équivalents: 

.. code-block:: java

        Thread t = new Thread(r); 
	t.start(); 

.. code-block:: java

        e.execute(r); 

A la maison. Pool de threads (10 min) 
--------------------------------------

- Ecrivez une classe ``BaigneursTest2`` qui, au lieu de manipuler un tableau de threads
  comme dans ``BaigneursTest``, utilise le pool de threads renvoyé par la méthode 
  ``newFixedThreadPool()`` de ``Executors``. 

- Appelez la méthode ``shutdown()`` pour finir l'exécution des threads et ne plus attendre
  de nouvelles tâches. 

- Testez avec un nombre de threads égal à 150, puis 50, puis 3. 

