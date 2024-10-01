# Task Name: Bezugsnorm_erkennen ######################

library(shiny)
library(bslib)
library(shinyjs)
library(dplyr)
library(learnr)
library(shinycssloaders)


## UI #########################################################################
ui <- page_fixed(
  useShinyjs(),
  card(
  card(
      card_header(
          "Aufgabe: Bezugsnorm in Aussage erkennen",
          class = "bg-dark"),
      card_body(
      htmlOutput("prompt_task")
      )
    ),
  
  shinyjs::hidden(card(id = "feedbackpanel_task",
                       card_header(
                         "Feedback",
                         class = "bg-dark"),
                       card_body(
                         withSpinner(
                           htmlOutput("feedback_task"),
                           proxy.height = "50px",
                           color = "#8cd000"))
  )
  ),
  card(
    uiOutput("ui_answers_task"),
    actionButton("show_feedback_task", 
                 "Prüfe meine Lösung!",
                 icon = icon("microscope")),
    actionButton("reshuffle_task", 
                 "Diese Aufgabe wiederholen",
                 icon = icon("repeat")),
    actionButton("new_task", 
                 "Neue Aufgabe derselben Art",
                 icon = icon("plus-circle"))
  )      
))

server <- function(input, output, session) {

  ##############################################################################
  # Backend for task  ##########################################################
  ##############################################################################

  
  ## Parameter solution matrix for task  ####
  q_a_matrix_qshuffeled <- 
    tibble(
      Answers_and_Distractors = c("Kriteriale Bezugsnorm", "Soziale Bezugsnorm", "Individuelle Bezugsnorm"),
      
      `Bei Schüler:in X hat sich die Leistung von Woche 6 zu 8 verschlechtert.` = c(F,F,T),
      `Im Test zeigt sich, dass Schüler:in X eine Verbesserung durchlebte, vor allem von Woche 4 zu Woche 6.` = c(F,F,T),
      `Schüler:in X hat sich verschlechtert.` = c(F,F,T),
      `Manche Schüler:innen verschlechtern sich und manche Schüler:innen verbessern sich deutlich.` = c(F,F,T),
      `Einige Schüler:innen verbessern sich.` = c(F,F,T),
      `Zentrale Ergebnisse hier sind, dass Schüler:in X von Stufe 5 auf Stufe 1 gerutscht ist.` = c(F,F,T),
      
      `Schüler:in X liegt innerhalb der mittleren 90% der Vergleichsgruppe.` = c(F,T,F),
      `Die Klasse schneidet sogar besser als Vergleichsgruppe ab.` = c(F,T,F),
      `Diese Klasse scheint besser abgeschnitten zu haben als die Vergleichsgruppe, beziehend auf die mittleren 50%.` = c(F,T,F),
      `Einige Schüler:innen sind besser als der Landesdurchschnitt.` = c(F,T,F),
      `Es gibt einige Schüler:innen, die sind (viel) schlechter als die anderen.` = c(F,T,F),
      `Es gibt nur wenige Ausreißer von den anderen weg nach unten hin, aber einige davon gravierend.` = c(F,T,F),
      `Die meisten der Kinder liegen weit über dem Durchschnitt` = c(F,T,F),
      
      `Die große Mehrheit der Schüler:innen befinden sich weit über dem Grundanspruch/Mindeststandard für eine Klasse 9.` = c(T,F,F),
      `Die meisten Schüler erfüllen den Anspruch oder sind sogar darüber.` = c(T,F,F),
      `Y kann den text Y in einer Minute fehlerfrei abschreiben` = c(T,F,F),
      `Schüler:in X kann chemische Reaktionen von Aggregatszustandsänderungen unterscheiden.` = c(T,F,F),
      `Schüler:in X kann den Strahlensatz sicher auf Alltagsprobleme anwenden` = c(T,F,F)
      ) %>% 
    # shuffle order of questions
    relocate(1, 2, sample(3:ncol(.), ncol(.) - 2))
  
  ## Number of 
  
  ## Shuffle answers
  q_a_matrix_qashuffeled <- 
    reactive({
      input$reshuffle_task # answer shuffelling induced by both buttons
      input$new_task
      
      q_a_matrix_qshuffeled %>%
        sample_frac(., size = 1)
    })
  
  ## Select task 
  nth_task <- reactive({
    # starting with 1
    (as.numeric(input$new_task) %% (ncol(q_a_matrix_qshuffeled) - 1)) + 1
  })
  
  
  ## Render UI for Answers ###
  output$ui_answers_task <- renderUI({
    input$reshuffle_task
    input$new_task
    radioButtons(
      "answers_task",
      "Bitte ankreuzen",
      q_a_matrix_qashuffeled() %>% 
        select(1, nth_task() + 1) %>% 
        na.omit(.) %>% 
        pull(Answers_and_Distractors),
      selected = character(0)
    )
  })
  
  ## Prompt task 
  output$prompt_task <- renderText({
    paste("<b>Entlang welcher Bezugsnorm bewertet die folgende Aussage eine Leistung?</b><i>",
          names(q_a_matrix_qashuffeled())[2:ncol(q_a_matrix_qshuffeled)][nth_task()],
          "</i>"
          )
  }) 
  
  ## Correct answers ###
  correct_answers_task <- reactive({
    
    vector_of_correct_answers <- 
      q_a_matrix_qashuffeled() %>% 
      pull(nth_task() + 1)
    
    correct_answers_task <- 
      q_a_matrix_qashuffeled() %>%
      filter(vector_of_correct_answers == T) %>%
      pull(Answers_and_Distractors)
    
    return(correct_answers_task)
  })
  
  ## Feedback task  ####
  output$feedback_task <- renderText({   
    
    if(is.null(input$answers_task)){
      HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:  <br>✓ ", 
                 paste(correct_answers_task(), collapse = ", <br>✓ "),
                 "<br><i>",
                 learnr::random_encouragement(),
                 "</i>"))
    }else{
      if(setequal(correct_answers_task(), input$answers_task)){
        paste("Richtig!", learnr::random_praise())}else{
          HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:  <br>✓ ", 
                     paste(correct_answers_task(), collapse = ", <br>✓ "),
                     "<br><i>",
                     learnr::random_encouragement(),
                     "</i>"))
        }
    }
  })
  
  ## Show and Hide Feedback ####################################################
  ## Show feedback on button click 
  observeEvent(input$show_feedback_task, {
    showElement(id = "feedbackpanel_task")
  })
  
  ## Hide feedback on solution change or new plot type
  observeEvent(c(input$answers_task, input$new_task), {
    hideElement(id = "feedbackpanel_task")
  })
  
  ## Reset answer on new plot type
  observeEvent(c(input$reshuffle_task, input$new_task), {
    reset(id = "answers_task")
  })
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)
