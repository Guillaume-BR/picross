library(shiny)

ui <- fluidPage(
  titlePanel("Picross"),
  mainPanel(
    fluidRow(
      column(12, align = "center", sliderInput("grid_size", label = "Taille de la grille", value = 5, min = 5, max = 20)),
      column(12, uiOutput("top_grid")),  # Nouvelle colonne pour afficher le nombre de cases noires par colonne
      column(2, id = "left_column", uiOutput("left_grid")),  # Colonne pour afficher le nombre de cases noires par ligne
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
    size <- input$grid_size + 5

    # Générer la matrice avec des valeurs aléatoires uniquement pour les cases qui ne sont pas parmi les 5 premières lignes et 5 premières colonnes
    random_numbers_val <- matrix(0, nrow = size, ncol = size)
    random_numbers_val[6:size, 6:size] <- sample(0:1, (size - 5)^2, replace = TRUE)
    random_numbers(random_numbers_val)

    # Exclure les 5 premières lignes et colonnes
    num <- random_numbers_val[-c(1:5), -c(1:5)]

    # Afficher la nouvelle matrice
    print(num)



    # Initialiser le vecteur de compteurs
    counters <- matrix(0, nrow = size - 5, ncol = size - 5)

    for (i in 1:(size - 5)) {
      c=1
      counter <- 0  # Réinitialiser le compteur pour chaque ligne
      for (j in 1:(size - 5)) {
        if (num[i, j] == 1) {
          counter <- counter + 1
        } else {
          if (counter > 0) {
            counters[i,c] <- counter
            c <- c+1
            counter <- 0
          }
        }
      }
      counters[i,c] <- counter
    }

    # Afficher le vecteur de compteurs
    print(counters)

    # Sélectionner que les nombres strictement positifs de counters
    num_line <- c()
    num_line_tot <- c()
    for (i in 1:(size-5)){
      num_line <- c()
      for (j in 1:(size-5)){
        if (counters[i, j] > 0){
          num_line <- c(num_line, counters[i, j])
        }
      }
      num_line_tot <- c(num_line_tot, list(num_line))
    }

    print(num_line_tot[1])



    # Affichage grille principale
    output$grid_container <- renderUI({
      grid_divs <- lapply(1:size, function(i) {
        lapply(1:size, function(j) {
          if (i >= 5 & j <= 5) {
            div(
              class = "cell",
              style = "text-align: center; font-weight: bold; border: none;",
              counters[i-5]
            )
          } else {
            div(
              class = paste("cell cell_", random_numbers()[i, j]),
              style = if (random_numbers()[i, j] == 1) {
                paste("background-color: black; margin: 1px;", ifelse(i <= 5 | j <= 5, "border: none;", "border: 1px solid black;"))
              } else {
                paste("margin: 1px;", ifelse(i <= 5 | j <= 5, "border: none;", "border: 1px solid black;"))
              },
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
  })

  # Utilisation de la bibliothèque shinyjs pour appliquer des styles CSS supplémentaires
  shinyjs::useShinyjs()
}

shinyApp(ui, server)





























































