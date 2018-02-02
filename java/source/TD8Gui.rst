===========================================
Interface graphique
===========================================

Pour commencer
=========================

Packages
-------------------------

- ``java.awt``: première bibliothèque. Fournit des interfaces et 
  des composants limités en code natif.  

- ``javax.swing``: seconde bibliothèque. Ne remplace pas ``java.awt``, 
  mais la complète avec des composants originaux et/ou performants, 
  écrits en Java.  

- On va utiliser ``javax.swing`` en priorité (composants commençant par ``J``), 
  mais aussi ``java.awt`` (*layout managers*, événements). 

Hello World
-----------------------------

.. literalinclude:: code/TD8/HelloWorldSwing.java
   :language: java


Event-Dispatching Thread
-------------------------

- Au lancement d'une application graphique, en plus du thread principal, 
  est lancé un second thread appelé *Event-Dispatching Thread* (EDT). 

- Une application graphique est réactive aux évenements (clic sur un bouton 
  par exemple). Or les réactions (affichage d'un commentaire par exemple) 
  sont placées dans une file et exécutées les unes après les autres dans l'EDT. 

- Les composants graphiques sont *thread unsafe*. C'est une erreur fréquente de les 
  manipuler depuis plusieurs threads. Les autres threads doivent soumettre leur code 
  à exécuter à l'EDT pour éviter les incohérences. 


Structure d'une fenêtre
==========================


Hiérarchie de conteneurs
---------------------------

Le nom des conteneurs de ``javax.swing`` commencent par ``J``. 

Chaque application graphique possède au moins un conteneur de haut-niveau: 

- ``JFrame`` (fenêtre)
- ``JDialog`` (boîte de dialogue)
- ``JApplet`` (fenêtre à intégrer dans une page web)

Ces conteneurs possèdent un *panneau d'affichage* (accessible par ``getContentPane()``, ``setContentPane()``)
sur lequel vous pouvez disposer des composants (qui dérivent de ``JComponent``). 

Composants graphiques
-----------------------------

Il existe de nombreux composants graphiques:  
  - ``JPanel`` (conteneur générique léger)
  - ``JLabel`` (étiquette de texte)
  - ``JButton`` (bouton)
  - ``JTextField`` (champs texte pouvant être éditable)
  - ``JMenuBar`` (barre des menus) 
  - ...

Consultez `l'API standard <http://docs.oracle.com/javase/7/docs/api/>`_  
pour la liste des composants disponibles dans ``javax.swing``. 

Personnaliser le panneau d'affichage
--------------------------------------

Habituellement, on crée un nouveau panneau d'affichage de zéro
en utilisant ``JPanel``, puis on l'ajoute au conteneur de haut 
niveau (typiquement de type ``JFrame``) avec ``setContentPane()``.   

.. code-block:: java 

        //Create a panel and add components to it.
        JPanel contentPane = new JPanel();
        contentPane.add(someComponent);
        contentPane.add(anotherComponent);

        topLevelContainer.setContentPane(contentPane);


Layout Manager
---------------------------

Il existe des modèles pour disposer les composants graphiques 
sur un panneau d'affichage:
 
- ``java.awt.BorderLayout`` (répartition en 5 zones: haut, bas, gauche, droite, centre)
- ``java.awt.FlowLayout`` (place les composants de gauche à droite, ligne par ligne) 
- ``java.awt.GridLayout`` (répartition en un tableau 2d régulier) 

- ``javax.swing.BoxLayout`` (place les composants les uns au-dessous des autres)
- ...

Consultez la page des `layout managers <http://docs.oracle.com/javase/tutorial/uiswing/layout/visual.html>`_
pour avoir un aperçu de ces modèles. 


Utiliser un Layout Manager
-----------------------------

Le modèle par défaut est ``FlowLayout``. Mais il est très simple d'en utiliser un autre
en le passant en paramètre, soit au constructeur de ``JPanel``, soit à la méthode 
``setLayout()``.  

.. code-block:: java 

        JPanel contentPane = new JPanel(new BorderLayout());
	//or: 
        //JPanel contentPane = new JPanel();
	//contentPane.setLayout( new BorderLayout() );  

        contentPane.add(someComponent, BorderLayout.CENTER);
        contentPane.add(anotherComponent, BorderLayout.PAGE_END);

        topLevelContainer.setContentPane(contentPane);



Ex.1. Calculatrice/Structure (20 min)
----------------------------------------

Reproduisez la fenêtre suivante: 

.. figure:: figs/Calculatrice.*
   :scale: 150 %
   :alt: Capture d'ecran du modèle
   :align: center

Ex.1. Détails
-------------------------------

- Le panneau d'affichage principal utilise un ``BorderLayout``. 
- En haut (PAGE_START), se trouve un ``JTextField``. 
- En bas (PAGE_END), se trouve un ``JLabel``. 
- A gauche (LINE_START), se trouve un ``JButton`` pour remettre la calculatrice à zéro. 
- A droite (LINE_END), se trouve un ``JButton`` pour afficher le résultat. 
- Au centre (CENTER), se trouve un panneau secondaire utilisant un ``GridLayout`` contenant
  des ``JButton`` pour les chiffres et les opérations.   
- Pensez à garder des références vers tous les composants. 

Gestion des evénements
==========================

Fonctionnement général
--------------------------

- A un composant graphique (``JButton``), on attache un objet capable d'écouter les événements (``ActionListener``). 
- Quand l'utilisateur réalise une action sur le composant (clic), un événement est généré (``ActionEvent``). 
- La machine virtuelle reçoit tous les événements, mais seul les événements écoutés déclenchent une réaction 
  (dont le code se trouve dans la méthode ``actionPerformed()`` de l'interface ``ActionListener``). 

Exemple de Listener
--------------------------

.. literalinclude:: code/TD8/ButtonBeeper.java
   :language: java

Exemple d'application
--------------------------

.. literalinclude:: code/TD8/BeeperApp.java
   :language: java
   :emphasize-lines: 10


Quelques types d'évenement
-----------------------------

- ``ActionEvent`` / ``ActionListener`` (déclenchés par ``JButton``)
- ``ItemEvent`` / ``ItemListener`` (déclenchés par ``JCheckBox``)
- ``MouseEvent`` / ``MouseListener`` (déclenchés par la souris sur un composant)
- ``KeyEvent`` / ``KeyListener`` (touche clavier) 
- ...


Consultez `l'API standard <http://docs.oracle.com/javase/7/docs/api/>`_  
pour la liste des événements disponibles dans ``java.awt.event``, 
le `tutoriel Evénements <http://docs.oracle.com/javase/tutorial/uiswing/events/intro.html>`_, 
ainsi que les `How To <http://docs.oracle.com/javase/tutorial/uiswing/components/componentlist.html>`_ 
consacrés aux composants pour savoir quel type d'événement ils déclenchent.  

ActionEvent / ActionListener
-----------------------------

Le couple ``ActionEvent`` / ``ActionListener`` est le plus commun. 

- ``ActionEvent`` est la classe des événements déclenchés par une action 
  bien définie sur un composant. Elle possède notamment la méthode ``getSource()`` pour
  obtenir le composant ayant déclenché l'événement.  

- Un objet issu d'une classe qui implémente l'interface ``ActionListener``, 
  doté de la méthode ``actionPerformed()``, est attaché à un tel composant 
  par la méthode ``addActionListener()``.


Ex.2. Calculatrice/Evenements (30 min)
---------------------------------------

- Faites en sorte que votre calculatrice affiche le résultat d'une suite d'additions ou de soustractions
  données par des actions sur les différents boutons. 

- Téléchargez la classe :download:`Processeur <download/Processeur.java>` qui se charge des calculs.

- Ecrivez un *listener* par type de bouton (un pour les chiffres, le plus, le moins, le ``=`` et le ``C``);
  chacun connaissant une instance de ``Processeur`` (calcul) et une instance de ``JTextField`` (affichage). 

- Astuce: les composants ``JButton`` et ``JTextField`` possèdent tous les deux les méthodes ``getText()``
  et ``setText()`` pour respectivement lire et écrire le texte attaché au composant. 

Ce qu'il faut retenir
----------------------------

- Une fenêtre (``JFrame``) est composée d'une hiérarchie de conteneurs (``JPanel``), 
  contenant eux-même des composants graphiques élémentaires (``JLabel``, ``JButton``, etc.). 

- Des modèles (*layout managers*) permettent de disposer les composants sur un panneau d'affichage. 

- Les composants graphiques peuvent s'abonner à des *listeners* (``ActionListener``), écoutant 
  des événements particuliers. Lorsqu'un énénement écouté (``ActionEvent``) est déclenché, le 
  code associé au *listeners* est exécutée par le *Event-Dispatching Thread*. 
  
Ce que vous devez savoir faire
---------------------------------

Réaliser une application graphique qui réagit aux actions de l'utilisateur. 
