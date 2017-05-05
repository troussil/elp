===========================================
Threads
===========================================

Introduction aux threads
==========================

Qu'est-ce que c'est ?
----------------------------------------

- Java propose un mécanisme pour exécuter en concurrence (et simultanément pour une machine multiprocesseurs) plusieurs séquences d'instructions.  

- Chaque séquence est un **thread**. Les threads sont des processus légers qui s'exécutent dans le même espace d'adressage.  

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
	t.start(); 


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


Synchronisation physique
==========================

Partage de la mémoire et verrou
--------------------------------------

Tous les threads ont accès au même espace mémoire. Quand les threads manipulent
une référence vers un même objet, les accès concurrents au même objet sont 
suceptibles de génèrer des erreurs. 
 
Pour les éviter, chaque objet a un **verrou** (*intrinsic lock*, *monitor lock*, *monitor*).
Un thread qui veut un accès exclusif à un objet acquière ce verrou, puis le libère 
quand il a finit. Entre temps, il possède le verrou. Aucun autre thread ne peut 
alors acquérir le verrou de cet objet.

Le mot-clé ``synchronized``
--------------------------------------

Quand un thread appelle une méthode ``synchronized`` d'un objet, il acquière son verrou
et le relâche à la fin de l'exécution de la méthode. 

.. code-block:: java

        synchronized typeRetour nomMethode(listeParametres) { ... }  

On peut aussi écrire des blocs ``synchronized`` pour avoir un niveau plus fin 
et éviter un blocage non nécessaire.   

.. code-block:: java

	  synchronized (this) {
             ...
          }

.. NB. un thread peut acquérir un verrou qu'il possède déjà. 
.. NB: on ne doit pas appeler une méthode non "synchronized" dans une méthode "synchronized" du même objet ou d'un autre, sinon on brise l'exclusivité.  

Ex.1. Compteur (10 min)
---------------------------------------

- Téléchargez la classe :download:`EvenCounter <download/EvenCounter.java>`. Que fait-elle ?

- Ecrivez la classe ``EvenCounterTest`` dans laquelle vous instanciez un seul objet de la classe 
  ``EvenCounter``, que vous exécutez dans deux threads.

- Compilez et exécutez plusieurs fois. Que se passe-t-il ? Pourquoi ? 

- Ajoutez le mot-clef ``synchronized`` à la méthode ``toNextEven``. Que se passe-t-il ?

Ex.2. Arrêt du compteur (10 min) 
--------------------------------------

Modifiez les classes ``EvenCounter`` et ``EvenCounterTest`` de façon à ce que 
la valeur du compteur ne s'affiche que tant qu'elle est inférieure à 50. 
Utilisez le mot-clé ``synchronized`` a bon escient pour éviter les accès concurrents, 
tout en permettant aux deux threads de travailler.  

Astuce: Préfixez les affichages par ``Thread.currentThread().getName()``. 




Ce qu'il faut retenir
---------------------------------------

Quand plusieurs threads partagent des données, il peut y avoir *interférence* 
(deux exécutions d'une même méthode sont entrelacées) ou *incohérence* 
(les appels de différentes méthodes d'un même objet sont entrelacés). 

Pour éviter ces problèmes, on utilise le mot-clé ``synchronized``. 

Quand un thread appelle une méthode ``synchronized`` d'un objet ou exécute 
un bloc ``synchronized(this)`` dans une de ses méthodes, il acquière son verrou,
et le relâche à la fin de l'exécution.


Synchronisation temporelle
============================

Cycle de vie d'un Thread
---------------------------------------

- Un thread est activé et prêt après l'appel de sa méthode ``start``. 
- Quand le *scheduler* lui donne la main, il exécute la méthode ``run``. 
- Il peut décider lui-même de rendre la main par la méthode ``yield``. 
- Sinon, il s'exécute tant que le *scheduler* ne le préempte pas (le suspend provisoirement). 
- Un thread est bloqué par une opération d'entrée-sortie ou par l'appel de certaines méthodes. 
- Dans ces cas, il redevient prêt quand l'opération s'achève ou qu'il est réveillé.   
 
Controle des threads via ``java.lang.Thread``
------------------------------------------------

  - ``start`` : active ce thread.
  - ``run`` : exécute ce thread.
  - ``interrupt`` : interromp ce thread. 
  - ``join`` : attend que ce thread se termine.   
  - ``sleep`` : endort ce thread durant un certain temps (en millisecondes).
  - ``yield`` : ce thread rends la main. 

Ex.3. Fin (5 min) 
----------------------------

Modifiez la classe ``EvenCounterTest`` de façon à faire afficher par le thread principal 
un message de fin sur la sortie standard. 


Coordination
---------------------

Il peut y avoir plusieurs problèmes de concurrence: 

  - deadblock : chaque thread laisse passer l'autre (image de deux personnes qui n'avancent pas tant que l'une n'a pas fait le premier pas). 
  - liveblock : chaque thread réagit par rapport à l'autre (image de deux personnes qui ne parviennent pas à se croiser en faisant toutes deux un pas de même côté)
  - starvation : un thread lent empêche les autres de faire leur travail. 

Pour coordonner les threads, on implémente des **commandes bloquantes** avec les méthodes ``wait`` et ``notifiy(All)`` de ``java.lang.Object``. 

Controle des threads via ``java.lang.Object``
------------------------------------------------

  - ``wait``: le thread courant doit posséder le verrou de l'objet (c'est pourquoi la méthode 
    dans laquelle ``wait`` est appelée doit être déclarée ``synchronized``). 
    Il relâche le verrou et attend qu'un autre thread le réveille par ``notify(All)`` 
    (ou qu'une durée donnée soit écoulée). Il attend ensuite d'obtenir le verrou pour poursuivre l'exécution.  
  - ``notifyAll``: réveille tous les threads en attente sur l'objet.  
  - ``notify``: réveille un seul thread, choisi arbitrairement. 


Ex.4. Wait/NotifyAll (20 min)
---------------------------------------

- Téléchargez cette :download:`archive <download/ProducerConsumer.tar.gz>`. 

- Que fait la classe ``ProducerConsumerTest`` ? Compilez et exécutez. Que se passe-t-il ?

- Ecrivez une classe ``SyncCubbyHole``, qui étend ``CubbyHole`` et qui redéfinit les méthodes 
  ``get`` et ``put`` en les marquant ``synchronized`` et en appelant les méthodes ``wait`` et ``notifyAll``. 

NB: Une bonne pratique est d'appeler ``wait`` dans une boucle testant la condition attendue 
(``myProduct == null`` ou ``myProduct != null``), car le thread qui attend peut être réveillé par un 
thread quelconque alors que la condition attendue n'est pas vérifiée.  

Ce qu'il faut retenir
----------------------------------------

- On peut attendre que des threads se terminent avec ``join`` afin d'exploiter le résultat de leurs traitements. 

- On peut utiliser ``wait`` et ``notifyAll`` pour implémenter des commandes bloquantes: 

 - Tous les objets peuvent mettre en attente le thread courant avec ``wait``. 
 - Tous les objets peuvent réveiller le(s) thread(s) bloqué(s) par eux, avec ``notify(All)``. 


Pour aller plus loin
============================

A la maison. Tableaux de threads (10 min)
-------------------------------------------

- Téléchargez les classes :download:`Piscine <download/Piscine.java>` et
  :download:`Baigneur <download/Baigneur.java>`. Que font-elles ?

- Ecrivez une classe ``BaigneursTest`` qui lance des threads opérant sur 150 instances de 
  la classe ``Baigneur``, chacune connaissant un seul objet de type ``Piscine``: 

.. code-block:: java

	Piscine piscine = new Piscine();    //la piscine
	int n = 150;
	Thread[] baigneurs = new Thread[n]; 
	for (int i = 0; i < n; i++)         //les baigneurs
	    baigneurs[i] = new Thread( new Baigneur(piscine, 5) ); 
 
- Compilez, puis exécutez plusieurs fois. Est-ce que ça fonctionne ?

- Dans la classe ``Piscine``, utilisez à bon escient le mot-clé ``synchronized``. 


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

Ce que vous devez savoir faire
---------------------------------

- Exécuter des threads en concurrence. 
- Mettre en oeuvre l'exclusion mutuelle avec "synchronized". 
- Mettre en oeuvre des relations d'ordre temporelle avec "join", "wait", "notifyAll".  
