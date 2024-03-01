library(shiny)
library(shinyjs)

ui <- fluidPage(
  titlePanel("Picross"),
  shinyjs::useShinyjs(),
  mainPanel(
    fluidRow(
      column(12, align = "center", sliderInput("grid_size", label = "Taille de la grille", value = 5, min = 5, max = 20)), # Choix de la taille de la grille
      column(10, uiOutput("grid_container")) # Grille totale
    )
  )
)

server <- function(input, output, session) {
  
  # Déclarer random_numbers en tant que variable réactive
  random_numbers <- reactiveVal(matrix(0, nrow = 1, ncol = 1))
  decompte_grid <- reactiveVal(NULL)
  decompte_colonne <- reactiveVal(NULL)
  
  observeEvent(input$grid_size, {
    # Définir les tailles de grille
    grid_size <- input$grid_size # Taille de la grille de jeu choisie
    ajout <- ceiling(input$grid_size/2) # Taille supplémentaire pour les indices
    size <- grid_size+ajout # Taille de la grille totale
    
    # Générer la matrice random_numbers_val avec des valeurs aléatoires uniquement pour la grille de jeu
    random_numbers_val <- matrix(0, nrow = size, ncol = size)
    # Définir difficulté ici prob=(1-p,p)
    random_numbers_val[ajout+1:grid_size, ajout+1:grid_size] <- sample(c(0,1), prob = c(0.3, 0.7), grid_size^2, replace = TRUE)
    random_numbers(random_numbers_val)
    
    # Exclure les lignes et colonnes prévues pour les indices
    num <- random_numbers_val[-c(1:ajout), -c(1:ajout)]
    numt <- t(num)
    #print(num)
    
    # DECOMPTE DES INDICES
    # Initialiser les matrices des indices
    counters_l <- matrix(0, nrow = grid_size, ncol = ajout)
    counters_c <- matrix(0, nrow = grid_size, ncol = ajout) # Sera transposée par la suite
    
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
    counters_ct <- t(counters_c)
    
    # AFFICHAGE GRILLE
    output$grid_container <- renderUI({
      grid_divs <- lapply(1:size, function(i) {
        lapply(1:size, function(j) {
          # Suppression des bordures hors de la grille de jeu et centrage des indices
          common_style <- paste("text-align: center; font-weight: bold; margin: 1px;", ifelse(i <= ajout | j <= ajout, "border: none;", "border: 1px solid black;"))
          
          if (i > ajout & j <= ajout) { # Affichage des indices lignes
            div(
              style = common_style,
              counters_l[i-ajout, j]
            )
          } else if (i <= ajout & j > ajout) { # Affichage des indices colonnes
            div(
              style = common_style,
              counters_ct[i, j-ajout]
            )
          } else { # Affichage de la sous-grille de jeu
            div(
              class = "grid_cell", # Classe utilisée pour détecter les clics par la suite
              style = common_style,
              id = paste("cell_", i, "_", j)
            )
          }
        })
      })
      
      tags$div( # Affichage de la grille totale
        style = paste("width: 500px; height: 500px; display: grid; grid-template-columns: repeat(", size, ", 1fr); grid-template-rows: repeat(", size, ", 1fr);"),
        grid_divs
      )
    })
    
    # Détection clic
    shinyjs::runjs('
  $(document).on("click", ".grid_cell", function(){
    var cell = $(this);
    var currentColor = cell.css("background-color");

    if(currentColor === "rgb(255, 255, 255)" || currentColor === "white") {
      cell.css("background-color", "black");
    } else if (currentColor === "rgb(0, 0, 0)" || currentColor === "black") {
      cell.css("background-color", "red");
    } else {
      cell.css("background-color", "white");
    }
  });
')
    
    
    
  })
}

shinyApp(ui, server)


