library(shiny)
library(shinyjs)
library(takuzu)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  div(class = "background"),
  
  div(id = "welcome", class = "screen",
      h1("TAKUZU", class = "title"),
      actionButton("start", "Je relève le défi", class = "btn-custom"),
      actionButton("rules_button", "?", class = "btn-rules")
  ),
  
  hidden(div(id = "rules", class = "screen",
             div(class = "parameters-zone",
                 h2("Règles du jeu TAKUZU", class = "title"),
                 p("Voici les règles pour résoudre une grille de TAKUZU :"),
                 tags$ul(
                   tags$li("Les cases vides doivent être remplies avec un 0 ou un 1."),
                   tags$li("Chaque ligne et chaque colonne doivent contenir autant de 0 que de 1."),
                   tags$li("Il ne peut pas y avoir plus de deux chiffres identiques consécutifs."),
                   tags$li("Le jeu est terminé lorsque toutes les cases sont correctement remplies."),
                   tags$li("Bon jeu")
                 ),
                 actionButton("close_rules", "Fermer", class = "btn-custom")
             )
  )),
  
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
  
  hidden(div(id = "game", class = "screen",
             h2("TAKUZU", class = "title"),
             uiOutput("gridUI"),
             div(class = "btn-container",
                 actionButton("edit_parameters", "Modifier les paramètres", class = "btn-custom"),
                 actionButton("new_grid", "Nouvelle grille", class = "btn-custom")
             ),
             div(
               id = "congratsMessage",
               class = "congrats",
               style = "display: none;",
               "Félicitations ! La grille est correctement résolue 🎉"
             )
  ))
)

server <- function(input, output, session) {
  
  grilleCourante <- reactiveVal(NULL)
  tailleGrille <- reactiveVal(NULL)
  idGrille <- reactiveVal(0)
  
  observeEvent(input$start, {
    hide("welcome")
    show("parameters")
  })
  
  observeEvent(input$rules_button, {
    hide("welcome")
    show("rules")
  })
  
  observeEvent(input$close_rules, {
    hide("rules")
    show("welcome")
  })
  
  observeEvent(input$size_4, { tailleGrille(4) })
  observeEvent(input$size_6, { tailleGrille(6) })
  observeEvent(input$size_8, { tailleGrille(8) })
  
  observeEvent(input$generate, {
    req(tailleGrille())
    idGrille(idGrille() + 1)
    nouvelleGrille <- generate_takuzu(n = tailleGrille(), difficulty = input$difficulty)
    grilleCourante(nouvelleGrille)
    hide("parameters")
    show("game")
    output$result <- renderText("")
  })
  
  observeEvent(input$new_grid, {
    req(tailleGrille())
    idGrille(idGrille() + 1)
    nouvelleGrille <- generate_takuzu(n = tailleGrille(), difficulty = input$difficulty)
    grilleCourante(nouvelleGrille)
    hide("parameters")
    show("game")
    output$result <- renderText("")
  })
  
  observeEvent(input$edit_parameters, {
    hide("game")
    show("parameters")
  })
  
  grilleCombinee <- reactive({
    req(grilleCourante())
    gr <- grilleCourante()
    n <- nrow(gr)
    for (i in seq_len(n)) {
      for (j in seq_len(n)) {
        if (is.na(gr[i, j])) {
          cid <- paste0("case_", idGrille(), "_", i, "_", j)
          valeur <- input[[cid]]
          if (!is.null(valeur)) {
            gr[i, j] <- as.numeric(valeur)
          }
        }
      }
    }
    gr
  })
  
  output$gridUI <- renderUI({
    req(grilleCourante())
    grOriginale <- grilleCourante()
    grUtilisateur <- grilleCombinee()
    n <- nrow(grOriginale)
    retour <- takuzu_feedback(grUtilisateur)
    
    tags$table(
      style = "border-collapse: collapse;",
      lapply(seq_len(n), function(i) {
        tags$tr(
          lapply(seq_len(n), function(j) {
            cid <- paste0("case_", idGrille(), "_", i, "_", j)
            
            if (!is.na(grOriginale[i, j])) {
              tags$td(
                grOriginale[i, j],
                style = "border: 1px solid #ccc; padding: 5px; text-align: center; width: 60px; height: 60px; background-color: #f0f0f0; font-weight: bold;"
              )
            } else {
              couleur <- if (!is.na(grUtilisateur[i, j]) && !retour[i, j]) "red" else "#ffffff"
              tags$td(
                numericInput(
                  inputId = cid,
                  label = NULL,
                  value = if (!is.na(grUtilisateur[i, j])) grUtilisateur[i, j] else NA,
                  min = 0, max = 1, step = 1,
                  width = "50px"
                ),
                style = paste0("border: 1px solid #ccc; padding: 5px; text-align: center; width: 60px; height: 60px; background-color: ", couleur, ";")
              )
            }
          })
        )
      })
    )
  })
  
  observe({
    req(grilleCombinee())
    gr <- grilleCombinee()
    if (!any(is.na(gr)) && is_solved_takuzu(gr)) {
      shinyjs::show("congratsMessage")
      output$result <- renderText("")
    } else {
      shinyjs::hide("congratsMessage")
      output$result <- renderText("")
    }
  })
  
}

shinyApp(ui = ui, server = server)
