library(shiny)
library(DT)

niveaux_difficulte <- list(
  Cadeau = 0.8,
  Facile = 0.7,
  Moyen = 0.6,
  Difficile = 0.55,
  Impossible = 0.4
)

tailles <- list(
  "5x5" = 5,
  "10x10" = 10,
  "15x15" = 15
)

mat_alea <- function(taille, niveau_difficulte) {
  if (!exists(niveau_difficulte, where = niveaux_difficulte)) {
    stop("Niveau de difficulté non valide.")
  }
  
  if (!exists(taille, where = tailles)) {
    stop("Taille non valide.")
  }
  
  proba_1 <- niveaux_difficulte[[niveau_difficulte]]
  taille_1 <- tailles[[taille]]
  
  matrice_aleatoire <- matrix(sample(c(0, 1), taille_1 * taille_1, replace = TRUE, prob = c(1 - proba_1, proba_1)), nrow = taille_1, ncol = taille_1)
  
  return(matrice_aleatoire)
}

ui <- fluidPage(
  headerPanel('Nonogramme'),
  sidebarPanel(
    selectInput('taille', 'Taille', names(tailles)),
    selectInput('proportion', 'Difficulté', names(niveaux_difficulte)),
    actionButton("action", "Vérification")
  ),
  mainPanel(
    DTOutput("matriceTable")
  )
)

server <- function(input, output, session) {
  matrice_aleatoire_reactive <- reactive({
    mat_alea(input$taille, input$proportion)
  })
  
  observeEvent(input$action, {
    showModal(modalDialog(
      title = "Matrice aléatoire générée",
      DTOutput("matriceTableModal")
    ))
  })
  
  output$matriceTable <- renderDT({
    datatable(matrice_aleatoire_reactive(), 
              options = list(
                dom = 't',
                autoWidth = TRUE,
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ))
  })
  
  output$matriceTableModal <- renderDT({
    datatable(matrice_aleatoire_reactive(), 
              options = list(
                dom = 't',
                autoWidth = TRUE,
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ))
  })
}

shinyApp(ui = ui, server = server)
