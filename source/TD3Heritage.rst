=====================================
Hiérarchie de classes
=====================================


Héritage
==========================


Attention
----------------------------

L'héritage possède deux caractéristiques pouvant parfois être en contradiction: 

- l'**extension de code**, qui consiste à dériver une nouvelle classe d'une
  classe déjà existante, pour hériter du code de la classe existante et en 
  ajouter sans avoir à tout réécrire. 
- le **polymorphisme** qui modélise la relation dans laquelle un objet 
  peut être utilisé à la place d'un autre objet. 

Syntaxe et vocabulaire
---------------------------

.. code-block:: java 

	class B extends A { ... }

.. code-block:: java 

	class C extends A { ... }

================ ==============
``A``            ``B``, ``C``
================ ==============
base             dérivée
mère, parente    fille
super-classe     sous-classe
qui est hérité   qui hérite
qui est étendue  qui étend
type, sur-type   sous-type
================ ==============

Extension de code
==========================

Principe
----------------------------

La classe dérivée hérite des membres non marqués **private** de la classe 
de base (mis à part les constructeurs). Cette classe peut elle-même posséder des champs
et méthodes supplémentaires. C'est le principe de l'extension, suggéré par le 
mot-clé ``extends``.   

Le mot-clé **protected** au-devant des membres de la classe de base permet 
d'élargir leur accès à toutes les classes dérivées. 

Ex.1. Point/Vecteur (5 min)
----------------------------

Observez les fichiers :download:`Point.java <download/Point.java>` 
et :download:`Vecteur.java <download/Vecteur.java>`. 

- Que fait ce code ? 
- A quoi correspondent ``x`` et ``y`` dans chacun des fichiers ?
- A quoi sert le mot-clé ``super`` ?
  
Ex.2. Complexe (10 min)
----------------------------

Faites une classe ``Complexe``, qui étend ``Vecteur``, par les méthodes: 
 - ``double obtenirNorme()``
 - ``Complexe obtenirConjugue()``
 - ``Complexe multiplier(Complexe c)``

NB: pour :math:`z = x + iy, z' = x' +iy'`,
 - :math:`Nz = x^2 + y^2` (norme),
 - :math:`\bar{z} = x - iy` (conjugué),
 - :math:`z * z' = (xx' - yy') + i(xy' + yx')`. 

Ce qu'il faut retenir
----------------------------------

- Les classes sont organisées en une hiérarchie. Le mot-clé ``extends`` 
  indique qu'une classe descent d'une autre. 

- L'état et le comportement associés aux instances d'une classe 
  sont automatiquement partagés à toute classe d'un descendant  
  (propriété d'extension de code).

- Dans une classe, le mot-clé ``this`` permet d'adresser des requêtes 
  à soi-même, tandis que le mot-clé ``super`` permet d'adresser des 
  requêtes à son parent. 

Polymorphisme
============================

Principe
----------------------------

L'héritage modélise aussi la relation dans laquelle 
un objet de la classe dérivée peut être utilisé comme un objet 
de la classe de base, c'est ce qui fait de l'héritage 
un mécanisme complexe.  

.. code-block:: java 

	class B extends A { ... }

.. code-block:: java 

        B objetB = new B(); 
        A objetA = new B(); //transtypage ascendant implicite

Un objet de la classe B *est un* objet de la classe A et peut
être utilisé comme tel. 

Attention: cette relation n'est pas *symétrique*. 

Requêtes
----------------------------

- Une méthode ``methodeA`` non privée de la classe ``A``, 
  peut être appelée à partir de la variable ``objetA``: 

.. code-block:: java 

        objetA.methodeA(); //compile
        objetB.methodeA(); //compile (extension de code)

- Si une méthode ``methodeB`` n'est définie que dans la classe ``B``, 
  on ne peut l'appeler à partir de la variable ``objetA``:  

.. code-block:: java 

        objetA.methodeB(); //ne compile pas (objetA est de type A)
        objetB.methodeB(); //compile (objetB est de type B)

Transtypage
-------------------------

Vous connaissez le transtypage ascendant (= *upcast*) implicite:  

.. code-block:: java 

	A objetA = new B(); //transtypage ascendant implicite

A l'inverse, il est possible de réaliser explicitement un transtypage descendant (= *downcast*): 

.. code-block:: java 

	B objetB2 = (B) objetA; //transtypage descendant explicite

C'est utile quand on manipule une instance de ``B`` comme un ``A`` (passage de paramètres par exemple), 
mais qu'on a besoin d'appeler ``methodeB``.


Liaison dynamique
----------------------------

A l'exécution, la machine virtuelle choisit la méthode à appeler en réponse à une requête, 
c'est le principe de la **liaison dynamique**. 

La recherche de cette méthode commence avec la classe de l'objet auquel la requête est adressée. 
Si aucune méthode appropriée n'est trouvée, la recherche se poursuit dans la classe parente et 
ainsi de suite jusqu'à ce qu'une méthode soit trouvée (le compilateur a préalablement vérifié
qu'il y aura toujours ultimement une méthode appropriée).  



Ex.3. TestComplexe (5 min)
---------------------------------

Ecrivez une classe ``TestComplexe``, dans laquelle vous testez 
 - la cohérence de l'addition et de la soustraction des nombres complexes en appelant directement 
   la méthode ``testsUnitaires`` de la classe :download:`TestVecteur.java <download/TestVecteur.java>`. 
 - la cohérence de la multiplication avec la norme et la conjugaison (la partie réelle de 
   :math:`z\bar{z}` doit être égale à la norme :math:`Nz`).  


Surcharge
----------------------------

Dans une classe, on peut définir plusieurs méthodes ayant le même nom, pourvu que leurs signatures 
soient différentes. 

L'intérêt est de faciliter l'écriture du code client et de fournir des valeurs par défaut pour certains
arguments. Par exemple, un réel pur est un nombre complexe n'ayant qu'une partie réelle (la partie 
imaginaire est à zéro). Il est donc naturel de pouvoir appliquer les opérations non seulement sur 
des complexes, mais aussi sur des réels.  

Ex.4. Surcharge (5 min)
----------------------------

Dans la classe ``Complexe``, 
 - surchargez la méthode ``multiplier`` pour permettre la multiplication d'un nombre complexe avec un réel pur, 
 - surchargez le constructeur pour obtenir un nombre complexe à partir d'un réel pur. 

Redéfinition
----------------------------

Dans une classe fille, il est possible de redéfinir certaines méthodes 
dont elle hérite pour les implémenter d'une autre manière. 
En réponse à un appel à ``methodeAB`` adressé à ``objetB``, 
ce sera la code de la classe ``B`` qui sera exécuté (et non celui de la 
classe ``A``). 


Ne pas confondre **redéfinition** (= *overriding*), même signature, mais corps différent entre 
la classe de base et la classe dérivée, et **surcharge** (= *overloading*), même nom, 
mais signature différente, au sein d'une même classe.  


``java.lang.Object``
----------------------------

En Java, toutes les classes dérivent par défaut de ``java.lang.Object`` (cf. 
`l'API standard <http://docs.oracle.com/javase/7/docs/api/>`_).
Cette classe possède quelques méthodes pouvant être redéfinies comme 
``toString`` qui retourne une représentation textuelle de type ``String`` de l'objet
(nom de la classe, arobase, hash code par défaut). 

Ex.5. Redéfinition (5 min)
---------------------------------

- Redéfinissez la méthode ``toString`` dans votre classe ``Complexe`` de façon à 
  afficher les nombres en notation complexe (sous la forme :math:`x+iy`), plutôt qu'en notation 
  vectorielle (sous la forme :math:`(x,y)`). 

Pour aller plus loin
---------------------------------

Vous avez peut-être remarqué que le résultat de la somme de deux nombres complexes est de type ``Vecteur``,
car la méthode ``ajouter`` appelée est celle de la classe ``Vecteur``. 

.. code-block:: java 

        //a et b sont de type Complexe
        Vecteur vsum = a.ajouter(b); //compile
	Complexe csum = a.ajouter(b); //ne compile pas
 
C'est embêtant si on veut enchaîner les opérations: multiplier la somme obtenue par un autre nombre complexe, 
par exemple, car dans ce cas il faut absolument un objet de type ``Complexe``. 

Ex.6. Surcharge 2 (5 min)
---------------------------------

Dans la classe ``Complexe``, 

 - surchargez le constructeur pour obtenir un nombre complexe à partir d'un vecteur,  
 - ajoutez les méthodes ``ajouter`` et ``retirer`` avec le type de retour ``Complexe``. 
   Il suffit pour cela de constuire un nombre complexe à partir du vecteur retourné par
   les méthodes ``ajouter`` et ``retirer`` de la classe parente. Dans du code client, 
   vérifiez avec une instruction du type: 

.. code-block:: java 

        //a et b sont de type Complexe
        Complexe sum = a.ajouter(b); 

Ce qu'il faut retenir
----------------------------------

- ce que c'est qu'une **surcharge** (dans une classe, plusieurs méthodes
  ayant le même nom, mais une signature différente) et une **redéfinition**
  (une classe et ses descendantes ont chacune une méthode identique), 

- ce que c'est que le **polymorphisme** (toutes les instances d'une classe 
  peuvent être vus comme des instances d'une classe parente),

- le mécanisme de **liaison dynamique** (comment la machine virtuelle recherche
  à l'exécution la méthode à appeler en réponse à une requête).

Ce que vous devez savoir faire
---------------------------------

- Etendre une classe existante pour factoriser du code. 
- Expliquer le mécanisme du polymorphisme. 
