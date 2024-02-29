library(shiny)
library(shinyjs)

ui <- fluidPage(
  titlePanel("Picross"),
  shinyjs::useShinyjs(),
  mainPanel(
    fluidRow(
      column(12, align = "center", sliderInput("grid_size", label = "Taille de la grille", value = 5, min = 5, max = 20)),
      column(10, uiOutput("grid_container"))
    )
  )
)

server <- function(input, output, session) {
  
  # Déclarer random_numbers en tant que variable réactive
  random_numbers <- reactiveVal(matrix(0, nrow = 1, ncol = 1))
  decompte_grid <- reactiveVal(NULL)
  decompte_colonne <- reactiveVal(NULL)
  
  observeEvent(input$grid_size, {
    # Définir la taille de la matrice en ajoutant 5 à grid_size
    # Attention il faut potentiellement grid_size/2 info
    size <- input$grid_size + 5
    
    # Générer la matrice avec des valeurs aléatoires uniquement pour les cases qui ne sont pas parmi les 5 premières lignes et 5 premières colonnes
    random_numbers_val <- matrix(0, nrow = size, ncol = size)
    #définir difficulté ici prob=(1-p,p)
    random_numbers_val[6:size, 6:size] <- sample(c(0,1), prob = c(0.3, 0.7),(size - 5)^2, replace = TRUE)
    random_numbers(random_numbers_val)
    
    # Exclure les 5 premières lignes et colonnes
    num <- random_numbers_val[-c(1:5), -c(1:5)]
    
    #DECOMPTE PAR LIGNE
    # EN commenant par la fin de la ligne, on aurait pas les 0
    # Initialiser le vecteur de compteurs
    counters_l <- matrix(0, nrow = size - 5, ncol = size - 5)
    
    for (i in 1:(size - 5)) {
      c <- 1
      counter <- 0  # Réinitialiser le compteur pour chaque ligne
      for (j in 1:(size - 5)) {
        if (num[i, j] == 1) { # Incrémenter pour case noire consécutive
          counter <- counter + 1
        } else { # Attribuer et réinitialiser le compteur si on tombe sur une case blanche
          if (counter > 0) {
            counters_l[i, c] <- counter
            c <- c + 1
            counter <- 0
          }
        }
      }
      counters_l[i, c] <- counter # Attribuer le compteur en passant à une nouvelle ligne
    }
    
    #DECOMPTE PAR COLONNE (même principe en inversant les indices)
    # En créant une fonction sur une matrice on pourrait l'appliquer à la transposée : plus simple peut-être
    counters_c <- matrix(0, nrow = size - 5, ncol = size - 5)
    
    for (j in 1:(size - 5)) {
      l <- 1
      counter <- 0
      for (i in 1:(size - 5)) {
        if (num[i, j] == 1) {
          counter <- counter + 1
        } else {
          if (counter > 0) {
            counters_c[l, j] <- counter
            l <- l + 1
            counter <- 0
          }
        }
      }
      counters_c[l, j] <- counter
    }
    
    # AFFICHAGE GRILLE
    output$grid_container <- renderUI({
      grid_divs <- lapply(1:size, function(i) {
        lapply(1:size, function(j) {
          common_style <- paste("margin: 1px;", ifelse(i <= 5 | j <= 5, "border: none;", "border: 1px solid black;"))
          
          if (i >= 5 & j <= 5) {
            div(
              class = "cell",
              style = paste("text-align: center; font-weight: bold; border: none;", common_style),
              counters_l[i-5, j]
            )
          } else if (i <= 5 & j >= 5) {
            div(
              class = "cell",
              style = paste("text-align: center; font-weight: bold; border: none;", common_style),
              counters_c[i, j-5]
            )
          } else {
            div(
              class = "grid_cell",
              style = paste("margin: 1px;", ifelse(i <= 5 | j <= 5, "border: none;", "border: 1px solid black;")),
              id = paste("cell_", i, "_", j)
            )
          }
        })
      })
      
      tags$div(
        id = "grid",
        style = paste("width: 500px; height: 500px; border: 1px solid black; display: grid; grid-template-columns: repeat(", size, ", 1fr); grid-template-rows: repeat(", size, ", 1fr);"),
        grid_divs
      )
    })
    
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
      cell.html(""); // Supprime le contenu de la cellule
    }
  });
')
    
    
    
  })
}

shinyApp(ui, server)


