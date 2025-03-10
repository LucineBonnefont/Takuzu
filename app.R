library(shiny)
library(takuzu)

ui <- fluidPage(
  titlePanel("Jeu Takuzu"),
  sidebarLayout(
    sidebarPanel(
      numericInput("gridSize", "Taille de la grille (nombre pair) :", value = 6, min = 4, max = 12, step = 2),
      sliderInput("difficulty", "Difficulté (proportion de cases vides) :", min = 0, max = 1, value = 0.5, step = 0.1),
      actionButton("generate", "Générer la grille"),
      br(), br(),
      actionButton("check", "Vérifier la solution")
    ),
    mainPanel(
      uiOutput("gridUI"),
      br(),
      textOutput("result")
    )
  )
)

server <- function(input, output, session) {

  # Variable réactive pour stocker la grille générée
  puzzle <- reactiveVal(NULL)

  # Génère la grille lorsque l'utilisateur clique sur "Générer la grille"
  observeEvent(input$generate, {
    req(input$gridSize)
    new_puzzle <- generate_takuzu(n = input$gridSize, difficulty = input$difficulty)
    puzzle(new_puzzle)
    output$result <- renderText("")  # Efface tout message précédent
  })

  # Affichage de la grille sous forme d'une table de numericInput
  output$gridUI <- renderUI({
    req(puzzle())
    n <- nrow(puzzle())
    grid <- puzzle()

    # Construction d'une table HTML
    tags$table(
      style = "border-collapse: collapse;",
      # Pour chaque ligne i
      lapply(seq_len(n), function(i) {
        tags$tr(
          # Pour chaque colonne j
          lapply(seq_len(n), function(j) {
            cell_id <- paste0("cell_", i, "_", j)
            # Valeur initiale : si la case n'est pas NA, on met la valeur ; sinon NA
            numeric_value <- if (!is.na(grid[i, j])) grid[i, j] else NA

            tags$td(
              numericInput(
                inputId = cell_id,
                label = NULL,
                value = numeric_value,
                min = 0,
                max = 1,
                width = "60px"
              ),
              style = "border: 1px solid #ccc; padding: 5px; text-align: center;"
            )
          })
        )
      })
    )
  })

  # Vérification de la grille saisie par l'utilisateur
  observeEvent(input$check, {
    req(puzzle())
    n <- nrow(puzzle())
    user_grid <- matrix(NA, nrow = n, ncol = n)

    # On lit chaque numericInput pour reconstituer la grille
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        cell_id <- paste0("cell_", i, "_", j)
        user_grid[i, j] <- as.numeric(input[[cell_id]])
      }
    }

    # Vérifie si la grille est complète (pas de NA) et valide
    if (any(is.na(user_grid))) {
      output$result <- renderText("La grille est incomplète. Remplissez toutes les cases.")
    } else {
      if (is_solved_takuzu(user_grid)) {
        output$result <- renderText("Félicitations ! La grille est correctement résolue.")
      } else {
        output$result <- renderText("La solution n'est pas correcte.")
      }
    }
  })
}

shinyApp(ui = ui, server = server)
