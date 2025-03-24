library(shiny)
library(shinyjs)
library(shinyfullscreen)
library(takuzu)

ui <- fluidPage(
  useShinyjs(),  # Active shinyjs pour gérer l'affichage dynamique
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")  # Inclusion du fichier CSS
  ),

  # Fond d'écran
  div(class = "background"),

  # Écran d'accueil
  div(id = "welcome", class = "screen",
      h1("TAKUZU", class = "title"),  # Titre stylisé
      actionButton("start", "Je relève le défi", class = "btn-custom")  # Bouton pour commencer
  ),

  # Écran de choix des paramètres (taille et difficulté)
  hidden(div(id = "parameters", class = "screen",
             div(class="parameters-zone", # Ajout du cadre
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
             br(),
             )
  )),

  # Zone du jeu (où la grille est affichée)
  hidden(div(id = "game", class = "screen",
             h2("TAKUZU", class = "title"),
             uiOutput("gridUI"),
             div(class = "btn-container",  # Conteneur des boutons
               actionButton("check", "Vérifier la solution", class = "btn-custom"),
               actionButton("edit_parameters", "Modifier les paramètres", class = "btn-custom")
           ),
           textOutput("result")
  ))
)

server <- function(input, output, session) {

  # Stocke la grille générée et la taille de la grille
  puzzle <- reactiveVal(NULL)
  grid_size <- reactiveVal(NULL)  # Pour stocker la taille de la grille

  # Quand on clique sur "Je relève le défi", on passe à l'écran des paramètres
  observeEvent(input$start, {
    hide("welcome")  # Cache l'écran d'accueil
    show("parameters")  # Affiche l'écran de sélection des paramètres
  })

  # Choix de la taille de la grille via les boutons
  observeEvent(input$size_4, {
    grid_size(4)  # Met la taille de la grille à 4x4
  })

  observeEvent(input$size_6, {
    grid_size(6)  # Met la taille de la grille à 6x6
  })

  observeEvent(input$size_8, {
    grid_size(8)  # Met la taille de la grille à 8x8
  })

  # Quand on clique sur "Générer la grille", on génère la grille et passe à l'écran de jeu
  observeEvent(input$generate, {
    req(grid_size())  # Assurez-vous qu'une taille a été choisie
    new_puzzle <- generate_takuzu(n = grid_size(), difficulty = input$difficulty)
    puzzle(new_puzzle)
    hide("parameters")  # Cache l'écran des paramètres
    show("game")  # Affiche l'écran du jeu
    output$result <- renderText("")  # Efface le message de résultat
  })

  # Quand on clique sur "Revenir en arrière", on retourne à l'écran des paramètres
  observeEvent(input$back, {
    hide("parameters")  # Cache l'écran des paramètres
    show("welcome")  # Affiche l'écran d'accueil
  })

  # Quand on clique sur "Modifier les paramètres", on retourne à l'écran des paramètres
  observeEvent(input$edit_parameters, {
    hide("game")  # Cache l'écran du jeu
    show("parameters")  # Affiche l'écran des paramètres
  })

  # Affiche la grille sous forme de table avec des numericInput
  output$gridUI <- renderUI({
    req(puzzle())
    n <- nrow(puzzle())
    grid <- puzzle()

    tags$table(
      style = "border-collapse: collapse;",
      lapply(seq_len(n), function(i) {
        tags$tr(
          lapply(seq_len(n), function(j) {
            cell_id <- paste0("cell_", i, "_", j)
            numeric_value <- if (!is.na(grid[i, j])) grid[i, j] else NA

            tags$td(
              style = "width: 60px; height: 60px; border: 1px solid #ccc; text-align: center; background-color: white; position: relative;",
              numericInput(
                inputId = cell_id,
                label = NULL,
                value = numeric_value,
                min = 0,
                max = 1,
                width = "100%"
              )
            )
          })
        )
      })
    )
  })

  # Vérification de la solution lorsque l'utilisateur clique sur "Vérifier la solution"
  observeEvent(input$check, {
    req(puzzle())
    n <- nrow(puzzle())
    user_grid <- matrix(NA, nrow = n, ncol = n)

    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        cell_id <- paste0("cell_", i, "_", j)
        user_grid[i, j] <- req(input[[cell_id]], NA)
      }
    }

    # Vérification de la solution
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
