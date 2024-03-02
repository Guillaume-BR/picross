library(shiny)
library(shinyjs)

niveaux_difficulte <- list(
  Cadeau = 0.9,
  Facile = 0.7,
  Moyen = 0.6,
  Difficile = 0.55,
  Impossible = 0.5
)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  headerPanel('Picross'),
  sidebarPanel(
    sliderInput("grid_size", label = "Taille de la grille", value = 5, min = 5, max = 20),
    selectInput('proportion', 'Difficulté', names(niveaux_difficulte)),
    actionButton("btn_generer", "Générer", disabled = TRUE),
    actionButton("btn_verifier", "Vérifier")
  ),
  mainPanel(
    uiOutput("grid_container")
  )
)

server <- function(input, output, session) {
  observeEvent(c(input$proportion, input$grid_size), {
    # Désactiver le bouton "Générer" tant que la taille de la grille n'est pas choisie
    shinyjs::disable("btn_generer")
    
    # Activer le bouton "Générer" une fois que la taille de la grille est choisie
    shinyjs::enable("btn_generer")
  })
  
  observeEvent(input$btn_generer, {
    # Définir les tailles de grille
    grid_size <- input$grid_size # Taille de la grille de jeu choisie
    ajout <- ceiling(input$grid_size/2) # Taille supplémentaire pour les indices
    size <- grid_size+ajout # Taille de la grille totale
    
    # Déclarer random_numbers en tant que variable réactive
    random_numbers <- reactiveVal(matrix(0, nrow = 1, ncol = 1))
    
    # Générer la matrice random_numbers_val avec des valeurs aléatoires uniquement pour la grille de jeu
    random_numbers_val <- matrix(0, nrow = size, ncol = size)
    
    # Définir difficulté ici prob=(1-p,p)
    p <- niveaux_difficulte[[input$proportion]]
    random_numbers_val[ajout+1:grid_size, ajout+1:grid_size] <- sample(c(0,1), prob = c(1-p, p), grid_size^2, replace = TRUE)
    random_numbers(random_numbers_val)
    
    # Exclure les lignes et colonnes prévues pour les indices
    num <- random_numbers_val[-c(1:ajout), -c(1:ajout)]
    numt <- t(num)
    #print(num)
    
    # DECOMPTE DES INDICES
    # Initialiser les matrices des indices
    counters_l <- counters_c <- matrix(0, nrow = grid_size, ncol = ajout)
    
    for (i in 1:grid_size) {
      c <- 1
      l <- 1
      # (Ré)initialiser le compteur pour chaque ligne
      counterc <- 0
      counterl <- 0
      for (j in 1:grid_size) { # Décompte par ligne
        if (num[i, j] == 1) { # Incrémenter pour chaque case noire (1) consécutive
          counterc <- counterc + 1
        } else { # Attribuer et réinitialiser le compteur si on tombe sur une case blanche (0)
          if (counterc > 0) {
            counters_l[i, c] <- counterc
            c <- c + 1
            counterc <- 0
          }
        } 
        # Même principe par colonne
        if (numt[i, j] == 1) {
          counterl <- counterl + 1
        } else {
          if (counterl > 0) {
            counters_c[i, l] <- counterl
            l <- l + 1
            counterl <- 0
          }
        }
      }
      # Attribuer le compteur en passant à une nouvelle ligne
      counters_l[i, c] <- counterc
      counters_c[i, l] <- counterl
      
      # Inverser les zéros remplacés par une chaîne de caractères vide avec les indices
      counters_c[i,] <- c(c(rep("", length(counters_c[i,][counters_c[i,] == 0]))), counters_c[i,][counters_c[i,] != 0])
      counters_l[i,] <- c(c(rep("", length(counters_l[i,][counters_l[i,] == 0]))), counters_l[i,][counters_l[i,] != 0])
      
      # Equivalent plus compréhensible
      #liste = counters_l[i,]
      #zeros <- length(liste[liste == 0])
      #zeros <- c(rep("", zeros))
      #non_zeros <- liste[liste != 0]
      #counters_l[i,] <- c(zeros, non_zeros)
    }
    
    # Transposition de la matrice des indices des colonnes
    counters_c <- t(counters_c)
    
    # AFFICHAGE GRILLE
    output$grid_container <- renderUI({
      grid_divs <- lapply(1:size, function(i) {
        lapply(1:size, function(j) {
          # Suppression des bordures hors de la grille de jeu et centrage des indices
          common_style <- paste("display: flex; justify-content: center; align-items: center; font-weight: bold; margin: 1px;", ifelse(i <= ajout | j <= ajout, "border: none;", "border: 1px solid black;"))
          
          if (i > ajout & j <= ajout) { # Affichage des indices lignes
            div(
              style = common_style,
              counters_l[i-ajout, j]
            )
          } else if (i <= ajout & j > ajout) { # Affichage des indices colonnes
            div(
              style = common_style,
              counters_c[i, j-ajout]
            )
          } else { # Affichage de la sous-grille de jeu
            div(
              class = "grid_cell", # Classe utilisée pour détecter les clics par la suite
              style = common_style,
              id = paste("cell_", i-ajout, "_", j-ajout)
            )
          }
        })
      })
      
      tags$div( # Affichage de la grille totale
        style = paste("width: 500px; height: 500px; display: grid; grid-template-columns: repeat(", size, ", 1fr); grid-template-rows: repeat(", size, ", 1fr);"),
        grid_divs
      )
    })
    
    # Définir la matrice du joueur
    joueur <- reactiveVal(matrix(0, nrow = grid_size, ncol = grid_size))
    
    # Nouvelle fonction pour mettre à jour la matrice du joueur
    update_player_matrix <- function(i, j) {
      current_joueur <- joueur()
      current_joueur[i, j] <- 1
      joueur(current_joueur)
    }
    
    # Détection clic (à corriger : ordres des couleurs buggé et double-clic)
    shinyjs::runjs('
    $(document).on("click", ".grid_cell", function(){
      var cell = $(this);
      var cellId = cell.attr("id");
      var coordinates = cellId.split("_");
      var i = coordinates[1];
      var j = coordinates[2];
      var currentColor = cell.css("background-color");

      if(currentColor === "rgb(255, 255, 255)" || currentColor === "white") {
        cell.css("background-color", "black");
        Shiny.onInputChange("cell_clicked", {row: i, col: j, color: "black"});
      } else if (currentColor === "rgb(0, 0, 0)" || currentColor === "black") {
        cell.css("background-color", "red");
        Shiny.onInputChange("cell_clicked", {row: i, col: j, color: "red"});
      } else {
        cell.css("background-color", "white");
        Shiny.onInputChange("cell_clicked", {row: i, col: j, color: "white"});
      }
    });
  ')
    
    # Mise à jour de la matrice du joueur lorsqu'une cellule est cliquée
    observeEvent(input$cell_clicked, {
      if (!is.null(input$cell_clicked)) {
        update_player_matrix(as.numeric(input$cell_clicked$row), as.numeric(input$cell_clicked$col))
      }
    })
    
    # Système de vérification (à corriger : affiche le message d'échec en changeant de taille de grille)
    observeEvent(input$btn_verifier, {
      if (!is.null(input$btn_verifier)) {
        if (identical(joueur(), num)) {
          showModal(modalDialog(
            title = "Résultat :",
            "Félicitations, Vous avez réussi !",
            easyClose = TRUE
          ))
        } else if (!identical(joueur(), num)) {
          showModal(modalDialog(
            title = "Résultat :",
            "Pas si intelligent que ça finalement...",
            easyClose = TRUE
          ))
        }
      }
    })
  })
}

shinyApp(ui, server)
