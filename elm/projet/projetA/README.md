# Mini-Projet TcTurtle

L'objectif de ce projet est d'écrire le programme elm d'une application web permettant de visualiser le dessin produit par des commandes de tracé données par l'utilisateur. Cet [exemple](https://perso.liris.cnrs.fr/tristan.roussillon/TcTurtle) peut être considéré comme un modèle à reproduire, voire à améliorer. 

# Prérequis

## elm

- Si besoin, relisez les quatre premières sections du [guide officiel](https://guide.elm-lang.org/) (Introduction, Core Language, The Elm Architecture, Types).
- Au fur et à mesure de votre progression, vous aurez aussi besoin des références suivantes : 
  - https://package.elm-lang.org/packages/elm/core/latest/Basics
  - https://package.elm-lang.org/packages/elm/core/latest/List
  - https://package.elm-lang.org/packages/elm/core/latest/String
  - https://package.elm-lang.org/packages/elm/parser/latest (voir également cette introduction aux [parseurs](../parser/README.md) en elm)
- Notez qu'il est fortement recommandé de commenter et structurer son code en [modules](https://guide.elm-lang.org/webapps/modules.html).

## HTML, CSS, SVG

HTML décrit le contenu d'une page web, tandis que CSS décrit son apparence. SVG décrit des dessins vectoriels, pouvant être directement inclus à une page web. Vous n'aurez pas à programmer vous-même directement dans l'un de ces langages. Il vous suffit de reprendre le code source de [l'exemple](https://perso.liris.cnrs.fr/tristan.roussillon/TcTurtle/index.html) et sa feuille de [style](https://perso.liris.cnrs.fr/tristan.roussillon/TcTurtle/style.css). En revanche, vous devrez appeler certaines fonctions elm qui auront pour effet de créer dynamiquement les éléments correspondant dans la page. La connaissance de HTML et SVG est donc un plus. Au minimum, assurez-vous que vous sachiez utiliser les fonctions suivantes dans de petits programmes :

- `div`, `input`, `button`, `text` du package [Html](https://package.elm-lang.org/packages/elm/html/latest/),
- `svg`, `line` du package [Svg](https://package.elm-lang.org/packages/elm/svg/latest/).

## TcTurtle

Enfin, vous devez comprendre le langage que doit interpréter votre application. C'est un langage inventé dans notre cher département et inspiré des [Turtle graphics](https://en.wikipedia.org/wiki/Turtle_graphics). Il s'appelle donc naturellement *TcTurtle*. Il permet d'exprimer le chemin que suit un crayon pour dessiner. Il comporte 4 instructions, toutes avec paramètres : **Forward**, **Left**, **Right** et **Repeat**. L'instruction `Forward x` fait avancer le crayon de `x` unités dans la direction courante. L'instruction `Right x` (respectivement `Left x`) fait tourner la direction courante de `x` degrés à droite (respectivement à gauche). L'instruction `Repeat x [ yyy ]` répète `x` fois la suite d'instructions entre crochets. Les instructions sont séparées par des virgules et le programme entier se trouve entre crochets et sur une seule ligne de texte. Voici quelques exemples de programme :

- `[Repeat 360 [ Right 1, Forward 1]]`
- `[Forward 100, Repeat 4 [Forward 50, Left 90], Forward 100]`
- `[Repeat 36 [Right 10, Repeat 8 [Forward 25, Left 45]]]`
- `[Repeat 8 [Left 45, Repeat 6 [Repeat 90 [Forward 1, Left 2], Left 90]]]`

Utilisez cette [application](https://perso.liris.cnrs.fr/tristan.roussillon/TcTurtle/index.html) pour visualiser les dessins qu'ils encodent. 

# Travail à faire

Voici quelques indications pour vous permettre de mener à bien ce projet.

## Coeur du projet

Votre programme doit être réparti en trois modules :

- le programme principal qui décrit votre modèle de page et implémente les fonctions `init`, `update` et `view` de l'architecture elm,
- le module de parsing qui fournit une fonction `read` prenant en entrée une chaîne de caractères et retournant une structure de données que vous aurez définie pour représenter les programmes *TcTurtle*. Vous aurez besoin des fonctions `succeed`, `token`, `int`, `spaces`, `lazy`, `run` du package [Parser](https://package.elm-lang.org/packages/elm/parser/latest/),
- le module de dessin qui fournit une fonction `display` traduisant votre stucture de données en un élément svg.

Bien sûr, vous êtes également encouragé à décomposer vos fonctions principales en fonctions élémentaires. Si vous avez une fonction qui fait plus de 25 lignes, il est certainement judicieux de la décomposer.

Je vous suggère de prendre le temps de réfléchir aux types que vous avez besoin pour représenter une instruction *TcTurtle* et un programme *TcTurtle*. Ensuite, vous pourrez en parallèle implémenter la fonction `read` qui construit un programme à partir d'une chaine de caractère et implémenter la fonction `display` qui convertit ce programme en un élément svg. 

## Améliorations

Si vous êtes arrivé à une application fonctionnelle sans difficulté, voici quelques pistes d'améliorations possibles :

- permettre un tracé pas à pas, via un timer qui fait un trait toutes les demi-secondes ou via un bouton. 
- traiter les exceptions liées à la position et à la taille du dessin par rapport à la feuille : calculer la position de départ et l'unité en fonction de l'étendue ou donner la possibilité de déplacer, agrandir, rétrécir le dessin par rapport à la feuille.
- offrir à l'utilisateur la possibilité de maîtriser son crayon : couleur, largeur, état (levé ou abaissé).
- fournir des indications détaillées permettant à l'utilisateur de corriger son programme quand il n'est pas syntaxiquement correct.
- ...

Vous avez certainement vous-mêmes d'autres idées.   
 
## Astuce

Si vous obtenez une page blanche ou n'obtenez tout simplement pas ce que vous voulez, ouvrez la console du navigateur pour savoir quelle erreur s'est produite. 

## Rendu

Votre code devra être disponible sur un repository github dont vous aurez fourni l'adresse. Vous devez également accompagner votre code d'un document d'au maximum une page comparant elm et javascript dans le contexte de ce projet.  
 
## Sur l'utilisation de packages externes

Il y a (au moins) deux packages elm sur le thème 'turtle graphics': 

1. https://package.elm-lang.org/packages/mrdimosthenis/turtle-graphics/latest/TurtleGraphics
2. https://package.elm-lang.org/packages/mgold/elm-turtle-graphics/1.0.2/Turtle 

Vous pouvez regarder le code, vous en inspirer, mais attention : 
- dans les deux cas, le programme est donné par une liste de valeurs elm; aucun des deux packages ne donne la possibilité de parser une chaîne de caractère écrite dans le langage spécifié.  
- le deuxième package n'est pas compatible avec elm 0.19.1 et ne produit pas de svg; il utilise un autre moyen pour réaliser le dessin. 
- parce qu'ils offrent plus de fonctionnalités que celles exigées dans le sujet, les deux packages sont plus compliqués que nécessaires; il pourrait être plus laborieux d'utiliser ces packages, que de programmer le projet intégralement par vous-mêmes. 

En conclusion, 
- si vous êtes en général bon programmeur et déjà à l'aise avec elm, vous pouvez utiliser le premier package, qui facilitera certainement l'ajout des extensions mentionnées ci-dessus et que j'attendrai alors nombreuses,  
- sinon, je vous déconseille de les utiliser. 
