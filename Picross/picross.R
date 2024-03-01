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
    # Définir la taille de la matrice totale
    g_size <- input$grid_size
    ajout <- ceiling(input$grid_size/2)
    size <- g_size+ajout
    
    # Générer la matrice avec des valeurs aléatoires uniquement pour la grille de jeu
    random_numbers_val <- matrix(0, nrow = size, ncol = size)

    #définir difficulté ici prob=(1-p,p)
    random_numbers_val[ajout+1:g_size, ajout+1:g_size] <- sample(c(0,1), prob = c(0.5, 0.5), g_size^2, replace = TRUE)
    random_numbers(random_numbers_val)
    
    # Exclure les 5 premières lignes et colonnes
    num <- random_numbers_val[-c(1:ajout), -c(1:ajout)]
    numt <- t(num)
    #print(num)
    
    # DECOMPTE PAR LIGNE
    # Initialiser le vecteur de compteurs
    counters_l <- matrix(0, nrow = g_size, ncol = ajout)
    counters_c <- matrix(0, nrow = g_size, ncol = ajout)
    
    for (i in 1:g_size) {
      c <- 1
      l <- 1
      counterc <- 0  # Réinitialiser le compteur pour chaque ligne
      counterl <- 0
      for (j in 1:g_size) {
        if (num[i, j] == 1) { # Incrémenter pour case noire consécutive
          counterc <- counterc + 1
        } else { # Attribuer et réinitialiser le compteur si on tombe sur une case blanche
          if (counterc > 0) {
            counters_l[i, c] <- counterc
            c <- c + 1
            counterc <- 0
          }
        } # même principe
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
      
      liste = counters_c[i,]
      zeros <- length(liste[liste == 0])
      zeros <- c(rep("", zeros))
      non_zeros <- liste[liste != 0]
      counters_c[i,] <- c(zeros, non_zeros)

      liste = counters_l[i,]
      zeros <- length(liste[liste == 0])
      zeros <- c(rep("", zeros))
      non_zeros <- liste[liste != 0]
      counters_l[i,] <- c(zeros, non_zeros)
    }

    counters_ct <- t(counters_c)
    
    # AFFICHAGE GRILLE
    output$grid_container <- renderUI({
      grid_divs <- lapply(1:size, function(i) {
        lapply(1:size, function(j) {
          common_style <- paste("text-align: center; font-weight: bold; margin: 1px;", ifelse(i <= ajout | j <= ajout, "border: none;", "border: 1px solid black;"))
          
          if (i > ajout & j <= ajout) { # indices lignes
            div(
              class = "cell",
              style = common_style,
              counters_l[i-ajout, j]
            )
          } else if (i <= ajout & j > ajout) { # indices colonnes
            div(
              class = "cell",
              style = common_style,
              counters_ct[i, j-ajout]
            )
          } else { #sous-grille de jeu
            div(
              class = "grid_cell",
              style = common_style,
              id = paste("cell_", i, "_", j)
            )
          }
        })
      })
      
      tags$div( # grille totale
        id = "grid",
        style = paste("width: 500px; height: 500px; display: grid; grid-template-columns: repeat(", size, ", 1fr); grid-template-rows: repeat(", size, ", 1fr);"),
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
    }
  });
')
    
    
    
  })
}

shinyApp(ui, server)


