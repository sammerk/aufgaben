# Task Name: Guetekriterium_in_Definition_erkennen ######################

library(shiny)
library(miniUI)
library(shinyjs)
library(hrbrthemes)
library(PearsonDS)
library(tidyverse)
library(learnr)
library(googledrive)
library(googlesheets4)
library(shinycssloaders)

## Googlesheets Connection Setup ###############################################
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets/"
)

gs4_auth()


## UI #########################################################################
ui <- miniPage(
  useShinyjs(),
  miniContentPanel(
    wellPanel(
      h4("Aufgabe: Gütekriterium anhand der Definition erkennen"),
      htmlOutput("prompt_task")
    ),
    shinyjs::hidden(wellPanel(id = "feedbackpanel_task",
                              withSpinner(
                                htmlOutput("feedback_task"),
                                proxy.height = "50px",
                                color = "#8cd000"))
    ),
    wellPanel(
      uiOutput("ui_answers_task"),
      actionButton("show_feedback_task", 
                   "Prüfe meine Lösung!",
                   icon = icon("send")),
      actionButton("reshuffle_task", 
                   "Diese Aufgabe wiederholen",
                   icon = icon("repeat")),
      actionButton("new_task", 
                   "Neue Aufgabe derselben Art",
                   icon = icon("plus"))
    )      
  )  
)



server <- function(input, output, session) {
  
  # Global functions ###########################################################
  ## round2 rounds .5 upwards
  round2 = function(x, n) {
    posneg = sign(x)
    z = abs(x)*10^n
    z = z + 0.5 + sqrt(.Machine$double.eps)
    z = trunc(z)
    z = z/10^n
    z*posneg
  }

  ##############################################################################
  # Backend for task  ##########################################################
  ##############################################################################
  
  # The global logic is to create a tibble containing  
  #      - answers & distractors (column 1)
  #      - questions (headers of columns 2 - m)
  #      - correct solutions (columns 2 - m without headers)
  #
  # Then 
  #     - reshuffle columns (without the first) to randomize order of questions
  #     - reshuffle rows to randomize order of answers & distractors
  
  
  ## Parameter solution matrix for task  ####
  q_a_matrix_qshuffeled <- 
    tibble(
      Answers_and_Distractors = c("Konstruktvalidität",
                                  "Interne Validität", 
                                  "Externe Validität", 
                                  "Statistische Validität"),
      `Welches Gütekriterium befasst sich mit der Frage, ob ein Messinstrument tatsächlich genau das Merkmal erfasst, das es zu messen vorgibt?` =  c(T,F,F,F), # adjust line 132 if coded 0/1 #1
      `Welches Gütekriterium befasst sich mit der Frage, wie eindeutig die Befunde einer Studie Hinweise auf Ursache-Wirkungs-Verhältnisse geben können?` =  c(F,T,F,F), #2
      `Welches Gütekriterium befasst sich mit der Frage, inwieweit der vorgefundene Effekt auf andere Orte, Zeiten, Situationen und Personen generalisierbar ist?` =  c(F,F,T,F), #3
      `Welches Gütekriterium befasst sich mit der Frage, wie gut die verwendeten statistischen Datenanalysen zur Forschungsfrage und zum Datenmaterial passen?` =  c(F,F,F,T), #4
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
    paste(names(q_a_matrix_qashuffeled())[2:ncol(q_a_matrix_qshuffeled)][nth_task()],
          "")
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

  
  ## URL Variable fetching #####################################################
  url_vars <- reactive({
    parseQueryString(session$clientData$url_search)
  })
  
  ## Usage Logging #############################################################
  observeEvent(input$show_feedback_task, {
    if(!is.null(input$answers_task)){
      sheet_append("1AZf7EQk-M2Wgej3WJXG1J2xRdql8b7Xiq0SvIiZogUo",
                   tibble(PID = ifelse(is.null(url_vars()$PID), 
                                       "PID is missing", #to keep ncol constant
                                       url_vars()$PID), # Person identifier from URL
                          task_name = "Guetekriterium_in_Definition_erkennen",
                          task_version = "repeatable_and_parametrized",
                          time = Sys.time(),
                          timezone = Sys.timezone(),
                          result = # correct or wrong sol. provided by student
                            case_when(is.null(input$answers_task) ~ 
                                        "false_solution",
                                      setequal(correct_answers_task(), 
                                               input$answers_task) ~ 
                                        "correct_solution",
                                      TRUE ~  "false_solution")
                   ),
                   sheet = 1)
    }
  })
}


# Create Shiny object
shinyApp(ui = ui, server = server)
