library(shiny)
library(shinyjs)

# Définir les niveaux de difficulté
niveaux_difficulte <- list(
  Cadeau = 0.85,
  Facile = 0.775,
  Moyen = 0.7,
  Difficile = 0.625,
  Impossible = 0.55
)

# Nouvelle fonction pour mettre à jour la matrice du joueur
#' Mets à jour la matrice du joueur
#' @param i indice ligne.
#' @param j indice colonne.
#' @returns A matrix.
#' @examples
#' update_player_matrix(1, 1)
#' update_player_matrix(3, 4)
#' @export

update_player_matrix <- function(i, j) {
  current_joueur <- joueur()
  current_value <- current_joueur[i, j]
  
  # Basculement entre 0 et 1
  new_value <- ifelse(current_value == 0, 1, ifelse(current_value == 1, 2, 0))
  
  current_joueur[i, j] <- new_value
  joueur(current_joueur)
}

# Fonction pour vérifier les positions des "1" dans les matrices
source("verif_pos.R")

# Définir les futures matrices réactives
joueur <- reactiveVal(matrix(0, nrow = 1, ncol = 1))
numr <-  reactiveVal(matrix(0, nrow = 1, ncol = 1))
blaguer <- reactiveVal(1)
timer <- reactiveVal(0)

# Interface
ui <- fluidPage(
  shinyjs::useShinyjs(),
  
  # Header
  headerPanel('Picross'),
  
  # Sidebar
  sidebarPanel(
    wellPanel(
      sliderInput("grid_size", label = "Taille de la grille", value = 5, min = 5, max = 20),
      selectInput('proportion', 'Difficulté', names(niveaux_difficulte)),
      textOutput('time')
    ),
    
    wellPanel(
      actionButton("btn_generer", "Générer", class = "btn-primary"),
      tags$style(HTML(".action-button { margin-right: 30px; }")),
      actionButton("btn_verifier", "Vérifier", class = "btn-success"),
      tags$style(HTML(".action-button { margin-right: 30px; }")),
      actionButton("btn_reset", "Recommencer", class = "btn-warning"),
      tags$style(HTML(".action-button { margin-right: 30px; }")),
      actionButton("btn_quitter", "Quitter", class = "btn-danger")
    )
  ),
  
  # Main Panel
  mainPanel(
    tabsetPanel(
      tabPanel(title = "La grille",
               textOutput("defaut"),
               uiOutput("grid_container")
      ),
      tabPanel(title = "Règle",
               htmlOutput("regle")
      )
    )
  )
)

server <- function(input, output, session) {
  observe({
    shinyjs::runjs('$("#btn_generer").click();')
  })
  
  # Initialiser le timer, non actif.
  active <- reactiveVal(FALSE)
  
  # Affichage du temps 
  output$time <- renderText({
    paste("Temps : ", timer() ," S")
  })
  
  # Message du début
  output$defaut <- renderText({
    "Choisissez une taille de grille et un niveau puis cliquez sur Générer."
  })
  
  output$regle <- renderText({
    return(
      "<p>Le but d’un picross est de noircir les cases de la grille selon des indications.</p>
      <p>Les nombres présents à gauche de la grille indiquent le nombre de cases à noircir sur la ligne correspondante.</p>
      <p>Les nombres présents en haut de la grille indiquent le nombre de cases à noircir sur la colonne correspondante.</p>
      <p>La séquence 5 2 signifie qu’il y a au moins une case vide (blanche) entre une séquence de cinq cases à la suite à noircir et une autre séquence de deux cases à la suite à noircir.</p>
      <p>Lorsqu’il y a une séquence donnée, elle est dans le bon ordre.</p>"
    )
  })
  
  
  
  #If timer is active, increase by one.
  observe({
    invalidateLater(1000, session)
    isolate({
      if(active() == TRUE)
      {
        timer(timer()+1)
      }
    })
  })
  
  # Déblocage du bouton de génération en choisissant les paramètres
  observeEvent(c(input$proportion, input$grid_size), {
    # Désactiver le bouton "Générer" tant que la taille de la grille n'est pas choisie
    shinyjs::disable("btn_generer")
    
    
    # Activer le bouton "Générer" une fois que la taille de la grille est choisie
    shinyjs::enable("btn_generer")
  })
  
  # Génération de la grille
  observeEvent(input$btn_generer, {
    timer(0)
    active(TRUE)
    blague <- 1
    blaguer(blague)
    
    # Définir les tailles de grille
    grid_size <- input$grid_size # Taille de la grille de jeu choisie
    ajout <- ceiling(input$grid_size/2) # Taille supplémentaire pour les indices
    size <- grid_size+ajout # Taille de la grille totale
    
    # Générer la matrice random_numbers_val avec des valeurs aléatoires uniquement pour la grille de jeu
    random_numbers_val <- matrix(0, nrow = size, ncol = size)
    
    # Définir difficulté ici prob=(1-p,p)
    p <- niveaux_difficulte[[input$proportion]]
    random_numbers_val[ajout+1:grid_size, ajout+1:grid_size] <- sample(c(0,1), prob = c(1-p, p), grid_size^2, replace = TRUE)
    
    # Exclure les lignes et colonnes prévues pour les indices
    numr(random_numbers_val[-c(1:ajout), -c(1:ajout)])
    num <- random_numbers_val[-c(1:ajout), -c(1:ajout)]
    numt <- t(num)
    
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
          common_style <- paste("display: flex; justify-content: center; align-items: center; font-weight: bold; margin: 1px;", ifelse(i <= ajout | j <= ajout, "border: none;", "border: 1px solid black; background-color: white"))
          
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
          } else if (i <= ajout & j <= ajout) { # Invisibiliser la sous-grille supérieure gauche
            div(
              style = common_style
            )
          } else { # Affichage de la sous-grille de jeu
            div(
              class = "grid_cell",
              style = common_style,
              id = paste("cell_", i-ajout, "_", j-ajout) # id unique par case
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
    joueur(matrix(0, nrow = grid_size, ncol = grid_size))
    
  })
  
  
  # Détection et coloration des cases cliquées
  shinyjs::runjs('
      $(document).on("click", ".grid_cell", function(){
        var cell = $(this);
        var cellId = cell.attr("id").split("_");
        var i = cellId[1];
        var j = cellId[2];
        var currentColor = cell.css("background-color");
        var newColor;
    
        if (currentColor === "rgb(255, 255, 255)" || currentColor === "white") {
          newColor = "black";
        } else if (currentColor === "rgb(0, 0, 0)" || currentColor === "black") {
          newColor = "red";
        } else {
          newColor = "white";
        }
    
        cell.css("background-color", newColor);
        Shiny.onInputChange("cell_clicked", {row: i, col: j, color: newColor});
      });
    ')
  
  # Mise à jour de la matrice du joueur lorsqu'une cellule est cliquée
  observeEvent(input$cell_clicked, {
    update_player_matrix(as.numeric(input$cell_clicked$row), as.numeric(input$cell_clicked$col))
  })
  
  # Système de vérification
  observeEvent(input$btn_verifier, {
    if (verifier_positions(numr(), joueur())) {
      response <- showModal(modalDialog(
        title = "Résultat :",
        paste("Félicitations, Vous avez réussi en ", timer() , "secondes !" ),
        easyClose = TRUE,
        footer = tagList(
          actionButton("btn_rejouer", "Rejouer"),
          actionButton("btn_arreter", "Quitter")
        )
      ))
    } else if (blaguer() == 1) {
      response <- showModal(modalDialog(
        title = "Résultat :",
        "Raté du premier coup !",
        easyClose = TRUE,
        footer = tagList(
          actionButton("btn_continuer", "Continuer"),
          actionButton("btn_arreter", "Quitter")
        )
      ))
    } else {
      response <- showModal(modalDialog(
        title = "Résultat :",
        paste("Raté en", blaguer(), "coups !"),
        easyClose = TRUE,
        footer = tagList(
          actionButton("btn_continuer", "Continuer"),
          actionButton("btn_arreter", "Quitter")
        )
      ))
    }
    blaguer(blaguer() + 1)
  })
  
  # Système de rejouabilité
  observeEvent(input$btn_rejouer, {
    timer(0)
    reactive(FALSE)
    output$grid_container <- renderUI({
      NULL
    })
    removeModal()
    observe({
      shinyjs::runjs('$("#btn_generer").click();')
    })
  })
  
  # Continuer le jeu
  observeEvent(input$btn_continuer, {
    removeModal()
    reactive(TRUE)
  })
  
  # Quitter l'application
  observeEvent(input$btn_quitter, {
    timer(0)
    stopApp()
  })
  
  # Quitter l'application
  observeEvent(input$btn_arreter, {
    stopApp()
  })
  
  # Réinitialiser la grille
  observeEvent(input$btn_reset, {
    timer(0)
    reactive(FALSE)
    joueur(matrix(0, nrow = input$grid_size, ncol = input$grid_size))
    shinyjs::runjs('$(".grid_cell").css("background-color", "white");')
  })
  
}

shinyApp(ui, server)
