# picross

Voici les étapes pour jouer au picross en local sur votre machine :

**Étape 1 : Prérequis**
Assurez-vous que votre système dispose des éléments suivants :

    - R installé avec les packages nécessaires
    - Un gestionnaire de packages R tel que install.packages() pour installer des packages R à partir de CRAN
    - Un programme pour extraire les archives tar.gz tel que tar sous Linux ou macOS, ou un logiciel comme 7-Zip sous Windows

**Étape 2 : Téléchargement et Extraction**
Téléchargez le fichier picross.tar.gz sur votre système. Placez-le dans un répertoire où vous souhaitez installer le programme. Ensuite, extrayez le contenu de l'archive en utilisant la commande suivante dans un terminal (ou utilisez un logiciel d'extraction d'archives graphique si vous êtes sous Windows) :

En Bash
tar -zxvf picross.tar.gz

Cela extraira les fichiers dans un dossier nommé "picross".

**Étape 3 : Installation des Dépendances**

Assurez-vous d'avoir installé toutes les dépendances nécessaires. Si votre programme nécessite des packages R spécifiques, vous pouvez les installer en utilisant la fonction install.packages() de R. Par exemple :

Dans R :
install.packages("nom_du_package")

Assurez-vous d'exécuter cette commande dans votre environnement R.

**Étape 4 : Exécution**
Une fois les dépendances installées, vous pouvez exécuter le programme en naviguant vers le répertoire où vous avez extrait les fichiers et en lançant votre script R ou en exécutant la commande spécifique pour démarrer votre application. Par exemple :

bash
Rscript nom_du_script.R

Ou, si c'est une application, vous pouvez lancer l'exécutable de votre application directement.

**Étape 5 : Utilisation**

Votre programme devrait maintenant être prêt à être utilisé. Suivez les instructions spécifiques à votre programme ou à votre application pour commencer à l'utiliser.

Ceci conclut les instructions d'installation pour votre archive tar.gz créée depuis R. Assurez-vous de consulter la documentation spécifique à votre programme ou à votre application pour des instructions plus détaillées sur son utilisation.

Ou bien si vous pouvez tout simplement jouer en ligendirectement :

lien du picross : <https://maxencelamure.shinyapps.io/Picross/>
