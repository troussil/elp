===========================================
TP: tic-tac-toe en réseau
===========================================

Prise en main (30 min)
=========================

L'objectif
-------------------------

Nous voulons programmer un tic-tac-toe en réseau. 

Le projet est déjà entamé. Vous devez télécharger :download:`ce code <download/TPJeu.tar.gz>`
puis l'enrichir. 

Le projet est organisé en 4 packages: 

- fr.insalyon.tc.elp.noyau pour les classes de base, comme celle modélisant la configuration des marques. 
- fr.insalyon.tc.elp.entree pour les classes modifiant la configuration courante.
- fr.insalyon.tc.elp.sortie pour les classes offrant une vue sur la configuration courante.
- fr.insalyon.tc.elp.app pour les applications combinant les classes des 3 packages précédents.
 
Package noyau
--------------------------

Il contient: 

- Couleur : couleur binaire (noir ou blanc)
- Marque : marque, identifiée par une couleur, déposée sur la grille par un joueur 
- Position : position donnée par un couple d'indices
- EnsembleDeMarques : configuration des marques 

Regardez attentivement ces classes que vous utiliserez. 

Positionnement
-------------------------

Nous avons les conventions suivantes: 

- Grille 3x3, 
- Identification des cases par un couple d'indices. On a repère absolu, 
  dont le centre se trouve en haut à gauche, le premier vecteur est horizontal, 
  pointé vers la gauche, le second est vertical pointé vers le bas. 


Package Entree
---------------------------

Il contient:

- Joueur : modélise un joueur qui, à son tour, dépose une marque de sa couleur sur la grille
- Arbitre : modélise un arbitre qui s'assure que les joueurs jouent à tour de rôle
- LecteurPosition : interface caractérisant les objets à qui on peut demander une position à laquelle déposer une nouvelle marque. 
- LecteurPositionDeFlux : LecteurPosition dans lequel la position est lue depuis un flux (entrée standard par exemple). 

 
Joueur vs Utilisateur
--------------------------

Un objet Joueur, a une couleur, connait la configuration des pions, l'arbitre, ainsi qu'un objet 
de type LecteurPosition. Si cet objet est du sous-type LecteurPositionDeFlux, 
alors le joueur est en attente d'une chaine de caractères sur un flux d'entrée contenant la
position qu'il doit jouer. 

Un utilisateur peut donc transmettre par le flux d'entrée standard son déplacement à 
l'objet Joueur qui le représente.   

Transmission textuelle
---------------------------

Pour transmettre une position sur le flux d'entrée standard, un utilisateur doit respecter 
la convention suivante: 

- une position s'écrit: ``i,j`` (abscisse, virgule, ordonnée). Par exemple ``1,2``. 

NB. la méthode ``toString`` des objets de type ``Position`` respecte cette convention. 

Package Sortie
---------------------------

Il contient pour l'instant: 

- VueShell: classe permettant d'afficher à l'écran la configuration des marques.

A l'avenir, nous voulons aussi avoir: 

- VueGraphique: classe permettant d'afficher dans une fenêtre graphique la configuration des marques. 

Package App
---------------------------

Il est vide! Créez vite une première application ``TicTacToeShell``, dans laquelle
les deux utilisateurs donnent la position de leur nouvelle marque, sous forme textuelle, 
dans le shell.  

Dans la méthode ``main``: 

- créez une configuration (instance ``EnsembleDeMarques``), 
- créez un lecteur de position (instance de ``LecteurPositionDeFlux``), 
- créez un arbitre, un joueur blanc, un joueur noir (instances de ``Arbitre`` et ``Joueur``),
- lancez deux threads, un pour chaque joueur.   

Vous pouvez jouer à l'aveugle: la grille n'est pas affichée! 


Modèle et vue (30 min)
=========================

Objectif
-------------------------

La classe ``EnsembleDeMarques`` (le modèle) ne connait pas la classe ``VueShell`` (la vue). 

On veut pourtant qu'elles puissent communiquer entre elles, pour 
que la vue soit mise à jour après la modification des données
(ajout ou suppression d'un pion sur une position). 

En même temps, on ne veut pas que le modèle dépende d'une vue particulière et on
voudrait avoir la possibilité d'offrir aisément zéro, une ou plusieurs vues sur 
le modèle.  

Pour réaliser cela, nous allons mettre en oeuvre le principe **Observable/Observer**. 

Observable
--------------------------

Notre modèle (``EnsembleDeMarques``) doit être observable (et observé par la vue). 

Pour être observable, il doit dériver de la classe 
`Observable <http://docs.oracle.com/javase/7/docs/api/java/util/Observable.html>`_. 

Il hérite notamment des méthodes 

- ``setChanged()``: indique que l'état du modèle à changer
- ``notifyObservers(Object arg)``: avertit les objets qui observent le modèle 
- ``clearChanged()``: oublie le changement

que vous devez appeler, dans l'ordre, à la fin de la méthode ``ajouterMarque``,
pour demander à la vue de procéder à une mise à jour de l'affichage.    

Observer
--------------------------

Notre vue (``VueShell``) doit observer notre modèle et déclencher l'affichage à 
chaque fois que le modèle est mis à jour après l'ajout d'une nouvelle marque. 

Elle doit satisfaire l'interface `Observer <http://docs.oracle.com/javase/7/docs/api/java/util/Observer.html>`_,
c'est-à-dire posséder une méthode ``update(Observable o, Object arg)`` (appelée par ``notifyObservers(Object arg)``). 
C'est dans cette méthode que vous devez déclencher l'affichage.  

L'objet ``o`` est l'objet observable qui appelle ``update`` (``EnsembleDeMarques``). 
Le paramètre ``arg`` permet d'ajouter une information pour la mise à jour; par exemple, 
la position à laquelle à été ajouté la nouvelle marque. 


Dans l'application ``TicTacToeShell``
------------------------------------------

Dans la méthode ``main``: 

- créez une vue (instance de ``VueShell``), 
- puis ajoutez-la comme observateur du modèle (instance de ``EnsembleDeMarques``) 
  avec la méthode ``addObserver(Observer o)``, héritée de `Observable`_. 

Vous pouvez commencer à jouer!


Serveur/Client (30 min)
=========================

Objectif
-------------------------

Jusqu'à maintenant, les deux utilisateurs jouent sur la même machine. 
Nous allons maintenant faire deux applications: ``TicTacToeShellServeur``
et ``TicTacToeShellClient``.  

Les deux applications auront toutes deux, un ensemble de marques, deux 
joueurs synchronisés par un arbitre, ainsi qu'une vue. Cependant, 
l'un des deux joueurs lira ses positions depuis la machine distante, 
tandis que l'autre enverra ses positions sur la machine distante. 

Ainsi, seuls des positions, sous forme textuelle, seront communiquées.  

Serveur
-------------------------

- Pour lire un déplacement depuis la machine distante, il suffit, pour 
  le joueur distant, de fournir à l'objet de type ``LecteurPositionDeFlux`` 
  le flux d'entrée du socket. Testez cette partie avec ``telnet``.

- Pour transmettre un déplacement, nous allons créer une nouvelle classe 
  ``JoueurTransmetteur`` qui dérive de ``Joueur`` et qui redéfinit la 
  méthode ``jouer`` de façon à, une fois une position choisie, 
  l'écrire sur le flux de sortie du socket. Testez encore avec ``telnet``. 
  

Client
-------------------------

Une fois que vous avez un serveur qui fonctionne, écrire 
l'application ``TicTacToeShellClient`` est un jeu d'enfant, 
car le code est parfaitement symétrique au code du serveur. 

Vous pouvez maintenant jouer depuis deux machines différentes. 
Mais l'interface du jeu n'est pas confortable.   


Interface graphique (45 min)
=============================

Objectif
---------------------------

Nous voulons créer une interface graphique pour notre jeu. 

Le package Sortie du projet sera enrichi:  

- d'une classe ``VueGraphique`` satisfaisant les interfaces ``Runnable`` et ``Observer``, 
- éventuellement accompagnée d'autres classes pour décomposer le code. 

La package App sera enrichi de nouvelles applications:
 
- ``TicTacToeGraphique`` pour jouer à deux sur la même machine,  
- les variantes client/serveur pour jouer sur des machines distinctes. 

Conception
---------------------------

La conception est libre, mais si vous ne savez pas quoi faire, vous pouvez: 
 
- écrire une classe ``Case`` dérivant de ``JButton`` avec un champs contenant la position correspondante.
- dans la classe ``VueGraphique``, organisez les cases dans un panel (``JPanel``) sous forme tabulaire (``GridLayout``). 

Vous pouvez commencer par tester votre affichage en donnant les positions par le shell. Mais comment lire les positions
directement sur l'interface graphique ? 


Evénements (45 min)
===========================

Objectif
---------------------------

Maintenant que nous avons une interface graphique, nous voulons
aussi que les utilisateurs puissent prescrire leurs déplacements 
par des actions (clics) sur l'interface graphique.  


Conception
---------------------------

1. Vous devez écrire un *listener*, par exemple ``CaseListener``. 
   Une instance de cette classe devrait récupérer la position 
   de la case choisie (et c'est pourquoi il est utile qu'une case
   connaisse sa position). 

2. Puis écrire une classe ``LecteurPositionDeVueGraphique`` qui satisfait
   l'interface ``LecteurPosition``. La vue, comme le *listener*, doivent
   connaitre le lecteur afin de lui transmettre la position de la case
   choisie.  

