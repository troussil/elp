# Révision, préparation à l'examen

## Enoncés

### Sujet 1 : classes, collections, mécanisme d'abstraction.

Soit la classe suivante :
```
class Personne {
  public int identifiant;
  public String nom, prenom;
  public Personne(int identifiant, String nom, String prenom) {
    this.identifiant = identifiant;
    this.nom = nom;
    this.prenom = prenom;
  }
}
```

1. Implémenter la fonction suivante qui retourne la collection des personnes satisfaisant le critère donné :
```
static ArrayList<Personne> recherche(Collection<Personne> collection, 
                                     CritereDeRecherche critere) {
...
}
```

2. Ecrire du code client qui réalise une recherche par identifiant, puis une autre par nom, dans une collection d’au moins 5 personnes. Sur la sortie standard, doit être affiché visiblement, la collection de départ, puis le résultat des deux recherches.

### Sujet 2 : classe, tableau associatif, flux, exceptions

1. Ecrire une classe `Annuaire` qui, dans son constructeur et à partir d'un fichier donné, alimente un tableau associatif. Le fichier, supposé bien formaté, contient une entrée par ligne et chaque entrée est de la forme suivante : clée, `;`, valeur. Pour séparer une chaîne de caractère selon un délimiteur donné, vous pouvez utiliser la méthode `split` : 

```
  String[] parts = line.split(";"); //line est de type String
```

2. Ecrire une classe exécutable `AnnuaireServeur` capable d'attendre une connexion client sur le port 8080 et de répondre à ses requêtes une fois la connexion établie. Le protocole est très simple : le client envoie une clée sous la forme d'une ligne de caractères, le serveur renvoie la valeur associée d'après son annuaire (une instance de la classe `Annuaire`) ou la chaine `null` sinon, puis attend la prochaine requête. Note : vous pouvez tester votre serveur avec la commande `telnet localhost 8080`.

### Sujet 3 : conteneurs, flux, exceptions, reflexivité

Le fichier `https://liris.cnrs.fr/tristan.roussillon/ens/proc` contient un certain nombre de lignes, dont certaines sont en double. Ecrivez un programme qui va récupérer l'ensemble des lignes de ce fichier (lu directement en ligne), puis générer un fichier sur le disque. Selon un paramètre passé par l'utilisateur, votre programme doit pouvoir :

- créer une copie conforme du fichier d'origine,
- supprimer les doublons (peu importe l'ordre des lignes),
- supprimer les doublons et trier les lignes.

Le paramètre fourni doit correspondre à l'un des différents types de collections vus en TD (et pour adapter le comportement du programme en fonction du type de collection donné, vous pouvez utiliser l'API `reflection`). Dans tous les cas, votre programme doit afficher le nombre de lignes lues et écrites. Notez que vous pouvez obtenir un flux d'entrée vers un fichier en ligne en utilisant les classes `URL` et `URLConnection` du package `java.net` :

```
URL url = new URL("http://...");
InputStream is = url.openConnection().getInputStream();
```

### Sujet 4 : conteneur, flux, exceptions, librairie GSON 

1. Ecrivez une classe exécutable qui, pour un fichier donné en argument, ajoute chacune de ses lignes de texte à une collection de type `ArrayList`. Gérez les exceptions `ArrayIndexOutOfBoundsException`, `FileNotFoundException`, `IOException` (et plus si nécessaire) de manière à ce que votre programme ne s'arrête pas subitement si une exception est levée. 

2. Testez avec ce [fichier](mil_unuaj_vortoj_sen_afiksoj_kontakto.txt) qui contient un ensemble de mots. 

3. A l'aide de la classe `java.util.Random`, sélectionnez aléatoirement un mot de la liste et affichez-le. 

4. Nous allons maintenant utiliser l'API d'un dictionnaire en ligne [simplavortaro](http://www.simplavortaro.org/) pour obtenir les définitions du mot choisi. Il suffit d'utiliser l'URL `http://www.simplavortaro.org/api/v1/vorto/VOTRE_MOT` pour obtenir une chaîne de caractère au format JSON contenant les définitions de `VOTRE_MOT`. `VOTRE_MOT` désignera par la suite le mot choisi. A l'aide du code ci-dessous et en remarquant que l'objet `connection` possède une méthode `getInputStream` permettant d'obtenir un flux d'entrée, affichez la chaine de caractère correspondant à `VOTRE_MOT`. 
```java
URL url = new URL(request);
URLConnection connection = url.openConnection();
```
5. Pour extraire de la chaîne de caractère les informations qui nous intéressent, vous allez utiliser la [librairie gson](https://github.com/google/gson) qui est contenue dans le fichier `.jar` fourni. 
Dans le [package com.google.gson](https://www.javadoc.io/doc/com.google.code.gson/gson/latest/com.google.gson/com/google/gson/package-summary.html), se trouvent notamment les classes `Gson`, `JsonElement`, `JsonObject`, `JsonArray`. Vous pouvez obtenir un objet de type `JsonElement` à partir d'une variable `s` de type `String` avec le code suivant : 
```java
Gson gson = new Gson(); // Creates new instance of Gson
JsonElement element = gson.fromJson(s, JsonElement.class); //Converts the json string to JsonElement 
```
  - Téléchargez ce [JAR](gson-2.8.6.jar), ajoutez les deux lignes précédentes, puis recompilez en ajoutant le JAR au *class path* (`javac -cp gson-2.8.6.jar VOTRE_CLASSE.java`). Exécutez en ajoutant aussi le JAR au *class path* **en plus du répertoire courant** (`java -cp gson-2.8.6.jar:. VOTRE_CLASSE`).
  - Utilisez les classes `JsonObject`, qui représente un dictionnaire, et `JsonArray`, qui représente une liste, afin d'affichez toutes les définitions de `VOTRE_MOT`, indiquées par le mot-clé `difino`.
  
6. On suppose maintenant que `VOTRE_MOT` n'est pas affiché et inconnu de l'utilisateur. Le jeu consiste à le deviner à partir des définitions. Ajoutez une boucle d'interaction avec l'utilisateur afin qu'il puisse proposer des mots tant qu'il n'a pas trouvé le bon ou tant qu'il n'a pas tapé la lettre `q` pour obtenir la bonne réponse et quitter. 

## Corrections

### Sujet 1

Pour faire fonctionner la [solution du sujet 1](Personne.java), vous devez compiler avec `javac Personne.java` et exécutez avec `java Personne`. 

Observez qu'il est possible de définir plusieurs classes et interfaces dans un seul fichier ; tout dépend des mots-clés de visibilité de ces éléments. Observez également qu'il est possible d'ajouter une méthode `main` dans n'importe quelle classe afin de la tester. 

Questions subsidiaires : à quoi sert la méthode `toString`, présente dans la solution mais non mentionnée dans le sujet ? Que veut dire et à quoi sert l'annotation `@Override` ?

### Sujet 2

Pour faire fonctionner la solution du sujet 2, rassemblez en un répertoire : 

- le [fichier des paires clés-valeurs](source.txt), 
- le [fichier Annuaire.java](Annuaire.java). 

Pour compiler, vous devez être positionné dans le répertoire contenant les deux éléments précédents et taper `javac Annuaire.java`. Pour exécutez, tapez `java AnnuaireServeur`. Votre programme serveur est alors en attente de connection. Vous pouvez le tester avec `telnet` : tapez `telnet localhost 8080`, puis, par exemple, les chaines `essai` (vous recevrez la réponse `null`) et `TRO` (vous recevrez la réponse `tristan.roussillon@insa-lyon.fr`).

Questions subsidiaires : que veut dire le mot-clé `final` ? pourquoi avoir rajouté `true` en second argument lors de la création d'un objet de type `PrintWriter` (ligne 55) ?

### Sujet 3

Pour faire fonctionner la [solution du sujet 3](CopierFichier.java), vous devez compiler avec `javac CopierFichier.java` et exécutez avec

- `java CopierFichier java.util.ArrayList` pour avoir une copie conforme du fichier d'origine,
- `java CopierFichier java.util.HashSet` pour supprimer les doublons sans tenir compte de l'ordre des lignes,
- `java CopierFichier java.util.TreeSet` pour supprimer les doublons et trier les lignes.

Si vous ne comprenez pas pourquoi le comportement diffère en fonction du conteneur utilisé, revoyez la partie du cours consacrée aux collections. Si vous ne comprenez pas comment instancier une classe dont l'utilisateur donne le nom, revoyez la partie du cours consacrée à la réflexion. 

Remarque : il y a un avertissement à la compilation. Selon vous, d'où vient-il ? Vous pouvez lire [cette référence](http://www.angelikalanger.com/GenericsFAQ/FAQSections/TechnicalDetails.html#FAQ001) pour comprendre plus précisément ce qu'il se passe.

### Sujet 4

Pour tester la solution, rassemblez en un répertoire :

- le [fichier de données](mil_unuaj_vortoj_sen_afiksoj_kontakto.txt), 
- le [JAR](gson-2.8.6.jar) de la librairie GSON, 
- le [fichier java](App.java). 

Pour compiler, vous devez être positionné dans le répertoire contenant les trois éléments précédents et taper `javac -cp gson-2.8.6.jar App.java`. Pour exécutez, tapez `java -cp gson-2.8.6.jar:. App mil_unuaj_vortoj_sen_afiksoj_kontakto.txt`. 


