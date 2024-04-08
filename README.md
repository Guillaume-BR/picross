# picross

Voici les étapes pour installer le package picross en local sur votre machine et y jouer :

**Étape 1 : Prérequis**

Assurez-vous que votre système dispose des éléments suivants :

    - R installé avec les packages nécessaires
    - Un gestionnaire de packages R tel que install.packages() pour installer des packages R à partir de CRAN
    - Un programme pour extraire les archives tar.gz tel que tar sous Linux ou macOS, ou un logiciel comme 7-Zip sous Windows

**Étape 2 : Téléchargement et Extraction**

Téléchargez le fichier picross.tar.gz sur votre système. Placez-le dans un répertoire où vous souhaitez installer le programme. Ensuite, extrayez le contenu de l'archive en utilisant la commande suivante dans un terminal (ou utilisez un logiciel d'extraction d'archives graphique si vous êtes sous Windows) :

En Bash :  
tar -zxvf picross.tar.gz

Cela extraira les fichiers dans un dossier nommé "picross".

**Étape 3 : Installation des Dépendances**

Assurez-vous d'avoir installé toutes les dépendances nécessaires. shiny and shinyjs : 

Dans R :  
install.packages("shiny")   
install.packages("shinyjs") 

Assurez-vous d'exécuter cette commande dans votre environnement R.

**Étape 4 : Exécution**
Une fois les dépendances installées, vous pouvez exécuter le programme en naviguant vers le répertoire où vous avez extrait les fichiers et en lançant votre script R ou en exécutant la commande spécifique pour démarrer votre application. 

Dans Bash :  
Rscript picross.R

**Étape 5 : Utilisation**

Votre programme devrait maintenant être prêt à être utilisé. Suivez les instructions spécifiques à votre programme ou à votre application pour commencer à l'utiliser.

Ou bien si vous pouvez tout simplement jouer en ligne directement :

lien du picross : <https://maxencelamure.shinyapps.io/Picross/>
