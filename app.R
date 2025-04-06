library(shiny)
library(shinyjs)
library(takuzu)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    
    # Inclusion du script de confetti
    tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.5.1/dist/confetti.browser.min.js"),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('confetti', function(message) {
        confetti({
          spread: 5000,
          particleCount: 7000,
          origin: { x: 0.5, y: 1.45 },
          gravity: 0.5,
          startVelocity: 100,
          ticks: 200,
          colors: ['#FF5733', '#FF33FF', '#33FFF3', '#FFD700', '#FF0000', '#00FF00', '#1E90FF'],
          shapes: ['circle', 'square', 'triangle'],
          angle: 90,
          angleVariation: 5000,
          decay: 0.9,
          drift: 0,
          gravity: 0.4
        });
      });
    "))
  ),
  
  div(class = "background"),
  
  #menu
  div(id = "welcome", class = "screen",
      h1("TAKUZU", class = "title"),
      actionButton("start", "Je relève le défi", class = "btn-custom")
  ),
  
  #parametre pour le joueur
  hidden(div(id = "parameters", class = "screen",
             div(class = "parameters-zone",
                 h2("TAKUZU", class = "title"),
                 p("Taille de la grille", class = "intro-text"),
                 div(
                   actionButton("size_4", "4x4", class = "btn-size"),
                   actionButton("size_6", "6x6", class = "btn-size"),
                   actionButton("size_8", "8x8", class = "btn-size")
                 ),
                 p("Difficulté (proportion de cases vides)", class = "difficulty-text"),
                 sliderInput("difficulty", NULL, min = 0, max = 1, value = 0.5, step = 0.1),
                 actionButton("generate", "Générer la grille", class = "btn-custom"),
                 br()
             )
  )),
  
  #zone de jeu
  hidden(div(id = "game", class = "screen",
             h2("TAKUZU", class = "title"),
             uiOutput("gridUI"),
             div(class = "btn-container",
                 actionButton("edit_parameters", "Modifier les paramètres", class = "btn-custom"),
                 actionButton("new_grid", "Nouvelle grille", class = "btn-custom")
             ),
             textOutput("result")
  ))
)

server <- function(input, output, session) {
  
  puzzle <- reactiveVal(NULL)
  grid_size <- reactiveVal(NULL)
  
  # Identifiant unique pour le puzzle afin d'éviter la réutilisation d'inputId
  puzzleID <- reactiveVal(0)
  
  observeEvent(input$start, {
    hide("welcome")
    show("parameters")
  })
  
  observeEvent(input$size_4, { grid_size(4) })
  observeEvent(input$size_6, { grid_size(6) })
  observeEvent(input$size_8, { grid_size(8) })
  
  observeEvent(input$generate, {
    req(grid_size())
    puzzleID(puzzleID() + 1)  # Incrémente l'ID pour la nouvelle grille
    new_puzzle <- generate_takuzu(n = grid_size(), difficulty = input$difficulty)
    puzzle(new_puzzle)
    hide("parameters")
    show("game")
    output$result <- renderText("")
  })
  
  observeEvent(input$new_grid, {
    req(grid_size())
    puzzleID(puzzleID() + 1)
    new_puzzle <- generate_takuzu(n = grid_size(), difficulty = input$difficulty)
    puzzle(new_puzzle)
    hide("parameters")
    show("game")
    output$result <- renderText("")
  })
  
  observeEvent(input$edit_parameters, {
    hide("game")
    show("parameters")
  })
  
  combinedGrid <- reactive({
    req(puzzle())
    grid <- puzzle()
    n <- nrow(grid)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (is.na(grid[i, j])) {
          # Incorporation de puzzleID dans l'inputId
          cell_id <- paste0("cell_", puzzleID(), "_", i, "_", j)
          val <- input[[cell_id]]
          if (!is.null(val)) {
            grid[i, j] <- as.numeric(val)
          }
        }
      }
    }
    grid
  })
  
  output$gridUI <- renderUI({
    req(puzzle())
    originalGrid <- puzzle()
    userGrid <- combinedGrid()
    n <- nrow(originalGrid)
    feedback <- takuzu_feedback(userGrid)
    
    tags$table(
      style = "border-collapse: collapse;",
      lapply(seq_len(n), function(i) {
        tags$tr(
          lapply(seq_len(n), function(j) {
            cell_id <- paste0("cell_", puzzleID(), "_", i, "_", j)
            
            if (!is.na(originalGrid[i, j])) {
              tags$td(
                originalGrid[i, j],
                style = "
                  border: 1px solid #ccc;
                  padding: 5px;
                  text-align: center;
                  width: 60px;
                  height: 60px;
                  background-color: #f0f0f0;
                  font-weight: bold;
                "
              )
            } else {
              bg_color <- if (!is.na(userGrid[i, j]) && !feedback[i, j]) "red" else "#ffffff"
              tags$td(
                numericInput(
                  inputId = cell_id,
                  label = NULL,
                  value = if (!is.na(userGrid[i, j])) userGrid[i, j] else NA,
                  min = 0, max = 1, step = 1,
                  width = "50px"
                ),
                style = paste0("
                  border: 1px solid #ccc;
                  padding: 5px;
                  text-align: center;
                  width: 60px;
                  height: 60px;
                  background-color: ", bg_color, ";
                ")
              )
            }
          })
        )
      })
    )
  })
  
# Quand on gange c'est la fête  
  observe({
    req(combinedGrid())
    grid <- combinedGrid()
    
    if (!any(is.na(grid)) && is_solved_takuzu(grid)) {
      session$sendCustomMessage("confetti", list())
      output$result <- renderText("Félicitations ! La grille est correctement résolue.")
    } else {
      output$result <- renderText("")
    }
  })
}

shinyApp(ui = ui, server = server)

