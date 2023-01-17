# Mini-Projet

L'objectif de ce projet est d'écrire le programme elm d'une application web permettant à un joueur de deviner un mot dont les définitions sont affichées. Voici un [exemple](https://perso.liris.cnrs.fr/tristan.roussillon/GuessIt/) de ce qui est attendu. 

## Pré-requis

- Si besoin, revoir les quatre premières sections du [guide officiel](https://guide.elm-lang.org/) (Introduction, Core Language, The Elm Architecture, Types).
- Lire attentivement les cinquième et sixième sections. Concernant la sixième section, tu peux te focaliser sur les *commandes*, car ce projet ne nécessite pas de faire des *souscriptions*. Les sous-sections HTTP, JSON, Random sont par contre toutes les trois importantes pour le projet. 
- Note qu'il est fortement recommandé de commenter et structurer son code en [modules](https://guide.elm-lang.org/webapps/modules.html).
- Enfin, puisque l'affichage se fait en HTML, il sera utile de connaître 
[du package HTML](https://package.elm-lang.org/packages/elm/html/latest/)
les éléments suivants :
  - les fonctions `div`, `span`, `h1`, `ul`, `ol`, `li`, `input`, `label`, `text` de `Html`.
  Excepté `text`, toutes correpondent à une balise HTML du même nom. 
  - les fonctions `value`, `type_`, `checked`, `for`, `id`, `class` de `Html.Attributes`
  qui correspondent à des attributs de certaines des balises précédentes.
  - les fonctions `onClick` et `onInput` de `Html.Events`
  qui correspondent à des événements associés à certaines des balises précédentes.  

## Données

Les mots se trouve dans ce [fichier](https://perso.liris.cnrs.fr/tristan.roussillon/GuessIt/thousand_words_things_explainer.txt). Ce sont les 1000 mots les plus courants utilisés dans le livre *Thing Explainer: Complicated Stuff in Simple Words* de Randall Munroe. 

Les définitions d'un mot au format JSON peuvent être obtenus en utilisant [Free Dictionary API](https://dictionaryapi.dev/).  

## Description fonctionnelle et technique

Au chargement de la page, votre programme doit sélectionner au hasard un mot parmi ceux de la liste. Puis, il doit obtenir et afficher ces définitions à partir d'une requête HTTP adressée à [Free Dictionary API](https://dictionaryapi.dev/). L'utilisateur doit deviner et écrire le mot qu'il croit correspondre aux définitions. Il peut continuer tant qu'il n'a pas la réponse correcte. Quand il a écrit la réponse correcte, un message le lui confirme.

D'un point de vue technique, vous devez
- utiliser `Browser.element`; `Browser.sandbox` ne suffira pas car vous devrez effectuer des commandes.
- définir les types `Model` et `Msg` pour modéliser l'état de l'application et la façon de le faire évoluer.
- définir les fonctions `init`, `update`, `view`.

Bien sûr, vous êtes également encouragé à décomposer vos fonctions principales en fonctions élémentaires. Si vous avez une fonction qui fait plus de 25 lignes, il est certainement judicieux de la décomposer.

Je vous suggère d'avancer pas à pas. Par exemple, d'abord afficher tous les mots, puis afficher un seul mot choisi au hasard, puis afficher non pas le mot, mais ses définitions et enfin ajouter l'interaction avec l'utilisateur. En faisant ça, vous allez sans cesse complexifier vos types et fonctions, mais vous aurez en contre-partie toujours un programme fonctionnel. Vous allez peut-être aussi apprendre à écrire vos types et fonctions de façon à facilement les faire évoluer. 

## Améliorations possibles

Il y a de nombreuses façons d'améliorer cet [exemple](https://perso.liris.cnrs.fr/tristan.roussillon/GuessIt/), notamment concernant l'aspect général de la page et la façon dont les informations sont affichées. Le jeu lui-même pourrait être décliné en un mode débutant où toutes les définitions sont affichées et un mode expert où une seule définition, prise au hasard, est affichée. On peut aussi imaginé donner la possibilité à l'utilisateur de cliquer sur n'importe quel mot affiché sur la page pour obtenir sa prononciation (phonétique ou orale) ainsi que sa définition en utilisant la même API. Vous avez certainement vous-mêmes d'autres idées.   
 