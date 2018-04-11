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
  (dans les environnements de développement par exemple), 

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
crée automatiquement un objet ``Class`` permettant d'obtenir tous les renseignements possibles concernant la 
classe chargée.  

On obtient un objet ``Class`` de 3 manières différentes: 

.. code-block:: java 

        Class<String> classe
	  = (Class<String>) Class.forName("java.lang.String"); //by name

.. code-block:: java 

        String aString = "azerty"; //from an object
	Class<String> classe = (Class<String>) aString.getClass();
   
.. code-block:: java 
	
	Class<String> classe = String.class; //from a class literal
	

Les méthodes de ``Class``
----------------------------------------

La classe ``Class`` contient des méthodes qui retournent des instances de
classes/interfaces définies dans le package ``java.lang.reflect``.

- ``Constructor[] getDeclaredConstructors()`` 	Renvoyer tous les constructeurs de la classe
- ``Field[] getDeclaredFields()`` 	Renvoyer un tableau de tous les attributs définis dans la classe
- ``Method[] getDeclaredMethods()`` 	Renvoyer un tableau de toutes les méthodes
- ``int getModifiers()`` 	Renvoyer un entier qu'il faut décoder pour connaître les modificateurs de la classe

Liste non exhaustive, consultez la `documentation <http://docs.oracle.com/javase/7/docs/api/>`_.


Ex.1. Inspecteur
--------------------------

- Téléchargez la classe :download:`XRayClass <download/XRayClass.java>`. 

- Compilez et exécutez-la en passant divers paramètres
  (par exemple, ``java.lang.String methods``, ``java.lang.Integer constructors``). 

    
Générer du code dynamiquement
=================================


Instance
-----------------

Créer un instance https://www.jmdoudoux.fr/java/dej/chap-introspection.htm

Appel de méthode
-------------------

invoke



Conclusion
==========================

Le coût
--------------------------

La réflexivité de Java contribue à sa flexibilité, mais elle a un coût a prendre en compte pour envisager son usage.

- l'appel aux méthodes réflexives (``getMethod``, ``newInstance``, etc.) ont un surcoût non négligeable,
- le code exploitant la réflexivité est moins lisible et plus complexe à comprendre que lorsque les appels sont directs,
- l'étape de compilation ne détecte plus les incohérences liées aux types ou aux exceptions. 

Bref, la réflexivité ne devrait être utilisé que lorsqu'aucune autre forme de programmation n'est appropriée. 

Capacités/connaissances
---------------------------------

qfdsqf
