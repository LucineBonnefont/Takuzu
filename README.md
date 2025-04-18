# **Takuzu - Jeu de Logique**

## Description

Bienvenue dans **Takuzu**, notre application web interactive construite avec Shiny! Takuzu est un jeu de logique où vous devez remplir une grille de 0 et de 1 en respectant des règles simples. Que vous soyez un passionné de puzzles ou un amateur de défis, ce jeu est parfait pour passer le temps tout en exerçant votre cerveau.

### Règles du Jeu
Le but du jeu est de remplir une grille de 0 et de 1, en respectant les règles suivantes :
1. Chaque ligne et chaque colonne doit contenir un nombre égal de 0 et de 1.
2. Aucune ligne ou colonne ne peut comporter plus de deux cases consécutives avec la même valeur (ni trois 0 ni trois 1).


## Lancer l'application

### Prérequis
Avant de commencer, assurez-vous d’avoir installé R et RStudio sur votre machine. Si ce n'est pas déjà fait, vous pouvez les télécharger ici :
- [R - Télécharger](https://cran.r-project.org)
- [RStudio - Télécharger](https://rstudio.com)

### Étapes pour lancer le jeu :

1. **Clonez le projet** depuis GitHub avec la commande suivante :
   ```bash
   git clone git@github.com:LucineBonnefont/Takuzu.git
   ```

2. Ouvrez RStudio et chargez le fichier `app.R` du projet.

3. Une fois le fichier `app.R` ouvert, vous devrez peut-être installer les dépendances nécessaires. Pour cela, exécutez les commandes suivantes dans la console R:
 ```bash
install.packages(c("shiny", "shinyjs")) 
```

4. Lancez l'application en cliquant sur le bouton **Run App** en haut à droite de la fenêtre RStudio. Cela ouvrira une fenêtre de navigateur où l'application Shiny sera exécutée.

5. Vous êtes maintenant prêt à jouer !

## Fonctionnalités de l'application
- Interface interactive : Remplissez les cases de la grille directement dans votre navigateur.
- Vérification des règles : L'application vérifiera automatiquement si vous respectez les règles du jeu.
- Feedback instantané : Dès qu'une règle est violée, vous serez informé pour pouvoir ajuster votre solution.

## Auteurs
- Lucine BONNEFONT
- Damien MARIAC

Nous espérons que vous apprécierez ce jeu !


