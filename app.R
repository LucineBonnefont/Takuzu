library(shiny)
library(shinyjs)
library(takuzu)

ui <- fluidPage(
  useShinyjs(),  # Active shinyjs pour gérer l'affichage dynamique
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),  # Inclusion du fichier CSS
    tags$script(src = "https://cdn.jsdelivr.net/npm/canvas-confetti@1.5.1/dist/confetti.browser.min.js")
  ),
  
  tags$script(HTML("
      Shiny.addCustomMessageHandler('confetti', function(message) {
        confetti({
        spread: 5000,
          particleCount: 7000,  // Augmentation du nombre de confettis pour un effet plus intense
      origin: {  x: 0.5, y: 1.45  },   // Position de départ (milieu de l'écran)
      gravity: 0.5,         // Gravité plus forte pour faire tomber les confettis rapidement
      startVelocity: 100,    // Vitesse plus élevée pour que l'explosion soit encore plus massive
      ticks: 200,           // Durée de vie des confettis (plus longtemps avant de disparaître)
      colors: ['#FF5733', '#FF33FF', '#33FFF3', '#FFD700', '#FF0000', '#00FF00', '#1E90FF'],
      shapes: ['circle', 'square', 'triangle'],  // Formes variées pour plus de diversité visuelle
      angle: 90,            // Direction verticale pour que les confettis tombent droit
      angleVariation: 5000,
      decay: 0.9,           // Réduction progressive de la vitesse pour simuler une chute naturelle
      drift: 0,             // Pas de dérive horizontale
      gravity: 0.4            // Ajuste la chute pour une gravité plus naturelle
        });
      });
    ")),
  
  # Fond d'écran
  div(class = "background"),
  
  # Écran d'accueil
  div(id = "welcome", class = "screen",
      h1("TAKUZU", class = "title"),  # Titre stylisé
      actionButton("start", "Je relève le défi", class = "btn-custom")
  ),
  
  # Écran de choix des paramètres (taille et difficulté)
  hidden(div(id = "parameters", class = "screen",
             div(class = "parameters-zone",
                 h2("TAKUZU", class = "title"),
                 
                 # Petit texte avant "Choisissez la taille de la grille"
                 p("Taille de la grille", class = "intro-text"),
                 
                 # Boutons pour choisir la taille de la grille
                 div(
                   actionButton("size_4", "4x4", class = "btn-size"),
                   actionButton("size_6", "6x6", class = "btn-size"),
                   actionButton("size_8", "8x8", class = "btn-size")
                 ),
                 
                 # Slider pour la difficulté
                 p("Difficulté (proportion de cases vides)", class = "difficulty-text"),
                 sliderInput("difficulty", NULL, min = 0, max = 1, value = 0.5, step = 0.1),
                 
                 # Bouton pour générer la grille
                 actionButton("generate", "Générer la grille", class = "btn-custom"),
                 br()
             )
  )),
  
  # Zone du jeu (où la grille est affichée)
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
  
  # Stocke la grille générée et la taille de la grille
  puzzle <- reactiveVal(NULL)
  grid_size <- reactiveVal(NULL)  # Pour stocker la taille de la grille
  
  # Passage de l'écran d'accueil aux paramètres
  observeEvent(input$start, {
    hide("welcome")
    show("parameters")
  })
  
  # Choix de la taille de la grille
  observeEvent(input$size_4, { grid_size(4) })
  observeEvent(input$size_6, { grid_size(6) })
  observeEvent(input$size_8, { grid_size(8) })
  
  # Génération de la grille et passage à l'écran du jeu
  observeEvent(input$generate, {
    req(grid_size())
    new_puzzle <- generate_takuzu(n = grid_size(), difficulty = input$difficulty)
    puzzle(new_puzzle)
    hide("parameters")
    show("game")
    output$result <- renderText("")
  })
  
  # Bouton pour modifier les paramètres
  observeEvent(input$edit_parameters, {
    hide("game")
    show("parameters")
  })
  
  observeEvent(input$new_grid, {
    req(grid_size())
    new_puzzle <- generate_takuzu(n = grid_size(), difficulty = input$difficulty)
    puzzle(new_puzzle)
    hide("parameters")
    show("game")
    output$result <- renderText("")
  })
  
  # Reactive qui combine la grille générée et les saisies utilisateur
  combinedGrid <- reactive({
    req(puzzle())
    grid <- puzzle()
    n <- nrow(grid)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (is.na(grid[i, j])) {
          cell_id <- paste0("cell_", i, "_", j)
          val <- input[[cell_id]]
          if (!is.null(val)) {
            grid[i, j] <- as.numeric(val)
          }
        }
      }
    }
    grid
  })
  
  # Affichage de la grille sous forme de table avec des numericInput
  output$gridUI <- renderUI({
    req(puzzle())
    originalGrid <- puzzle()
    userGrid <- combinedGrid()
    n <- nrow(originalGrid)
    
    # Obtention du feedback par cellule via takuzu_feedback()
    feedback <- takuzu_feedback(userGrid)
    
    tags$table(
      style = "border-collapse: collapse;",
      lapply(seq_len(n), function(i) {
        tags$tr(
          lapply(seq_len(n), function(j) {
            cell_id <- paste0("cell_", i, "_", j)
            
            # Si la case est fixe (déjà remplie dans le puzzle), on l'affiche en texte non modifiable
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
              # Pour les cases modifiables, le fond est rouge si la saisie est incorrecte, sinon blanc
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
  
  # Observer qui vérifie automatiquement la grille et félicite le joueur si elle est correctement complétée
  observe({
    req(combinedGrid())
    grid <- combinedGrid()
    
    # Si la grille est complète (aucun NA) et respecte toutes les règles, on affiche le message
    if (!any(is.na(grid)) && is_solved_takuzu(grid)) {
      session$sendCustomMessage("win", list())
      session$sendCustomMessage("confetti", list())
      
      output$result <- renderText({
        "FÉLICITATIONS!"
      })
    } else {
      output$result <- renderText("")
    }
  })
  
}

shinyApp(ui = ui, server = server)
