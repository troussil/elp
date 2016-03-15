===========================================
TP: tic-tac-toe en réseau
===========================================

Prise en main (45 min)
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
- LecteurDePosition : interface caractérisant les objets à qui on peut demander une position à laquelle déposer une nouvelle marque. 
- LecteurDePositionDeFlux : LecteurDePosition dans lequel la position est lue depuis un flux (entrée standard par exemple). 

 
Joueur vs Utilisateur
--------------------------

Un objet Joueur, a une couleur, connait la configuration des pions, l'arbitre, ainsi qu'un objet 
de type LecteurDePosition. Si cet objet est du sous-type LecteurDePositionDeFlux, 
alors le joueur est en attente d'une chaine de caractères sur un flux d'entrée représentant le 
déplacement qu'il doit jouer. 

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

- créez un configuration (instance ``EnsembleDeMarques``), 
- créez un lecteur de position (instance de ``LecteurDePositionDeFlux``), 
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

NB. Pensez au *downcast* pour récupérer le type initial des objets passés en argments. Par exemple: 

- ``EnsembleDeMarques grille = (EnsembleDeMarques) o;`` 
- ``Position p = (Position) arg;``  

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
  le joueur distant, de fournir à l'objet de type ``LecteurDePositionDeFlux`` 
  le flux d'entrée du socket. Testez cette partie avec ``telnet``.

- Pour transmettre un déplacement, nous allons créer une nouvelle classe 
  ``JoueurTransmetteur`` qui dérive de ``Joueur`` et qui redéfinit la 
  méthode ``jouer`` de façon à, une fois un déplacement réalisé, 
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
 
- écrire une classe ``Damier`` dérivant de ``JPanel``. La vue possédera un damier. 
- écrire une classe ``CaseNoire`` dérivant de ``JPanel``. Le damier sera composé
  de cases blanches (objet ``JPanel`` par défaut) et de cases noires. Les cases 
  noires se chargent du dessin de la case et de la pièce qu'elle contient éventuellement. 
- les cases peuvent être organisées dans le damier sous forme tabulaire par un ``GridLayout``. 

Ce n'est pas forcément la meilleure solution.  

Comment dessiner ?
---------------------------

Vous aurez probablement besoin des méthodes de la classe 
`Graphics <http://docs.oracle.com/javase/7/docs/api/java/awt/Graphics.html>`_
pour dessiner les marques. 

- ``setColor``
- ``fillRect``
- ``fillOval`` 
- ...

Chaque composant graphique (de type ``JComponent``) possède
une méthode ``paintComponent`` prenant en entrée un objet de type ``Graphics``. 
Vous pouvez donc adapter l'aspect de vos composants en redéfinissant cette méthode,  
qui est appelé à la création du composant et à chaque appel de la méthode ``repaint``. 


Evénements (45 min)
===========================

Objectif
---------------------------

Maintenant que nous avons une interface graphique, nous voulons
aussi que les utilisateurs puissent prescrire leurs déplacements 
par des actions (clics) sur l'interface graphique.  

Dans un premier temps, nous supposons que l'utilisateur ne réalise
que des déplacements simples impliquant uniquement une position de 
départ et une positions d'arrivée (pas de rafles). 


Conception
---------------------------

1. Vous devez écrire un *listener*, par exemple ``CaseNoireListener``. 
   Une instance de cette classe devrait transmettre la position de 
   la case cliquée par l'utilisateur à la vue.   

2. Puis écrire une classe ``LecteurDePositionDeVue`` qui satisfait
   l'interface ``LecteurDePosition``. La vue devrait connaitre le 
   lecteur afin de lui transmettre les positions du déplacement.  

Pour aller plus loin
----------------------------

Pour que l'utilisateur puisse réaliser n'importe quel déplacement, 
deux stratégies sont possibles: 

- Un signal est utilisé par les utilisateurs pour indiquer la fin
  du déplacement (clic sur un bouton réservé à cet effet par exemple). 

- Le déplacement est considéré comme terminé dès qu'il est valide 
  (avec prise obligatoire et prise du plus grand nombre de pièces obligatoire
  selon les `règles du jeu`_). 

Fin
===========================

Rendu
---------------------------

- Une archive ``tar.gz`` contenant le projet Maven et portant le nom 
  des deux auteurs dans l'ordre alphabétique
  sous la forme suivante: ``Nom1-Nom2.tar.gz``. 

- A charger sur Moodle avant la date limite indiquée sur la plateforme. 

- Le projet ne doit comporter que les fichiers sources et 
  doit pouvoir être compilé sans erreur.  


Evolutions possibles
----------------------------

