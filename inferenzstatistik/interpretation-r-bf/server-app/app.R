## Task Name: Interpret_r_BF ######################################################

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
      h4("Pearsons's r und Bayes Faktor BF₁₀ interpretieren"),
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
      Answers_and_Distractors = c("Es liegt moderate Evidenz (3 < BF₁₀ < 10) für die Alternativhypothese (Pearson's r > 0) vor.",
                                  "Es liegt starke Evidenz (10 ≤ BF₁₀) für die Alternativhypothese (Pearson's r > 0) vor.",
                                  "Es liegt moderate Evidenz (1/10 < BF₁₀ < 1/3) für die Nullhypothese (Pearson's r = 0) vor.",
                                  
                                  "Es liegt starke Evidenz (BF₁₀ ≤ 1/10) für die Nullhypothese (Pearson's r = 0) vor.",
                                  "Die inferenzstatistische Prüfung ist inkonklusiv (1/3 < BF₁₀ < 3)",
                                  "Die Alternativhypothese ist wahr.", 
                                  
                                  "Die Nullhypothese ist wahr.",
                                  "Es liegt ein substantieller (mindestens kleiner) Effekt vor."),
      
      `Eine Forscherin untersucht die Korrelation zwischen der Klassengröße (variiert zwischen 21 und 27 Schülerinnen und Schülern) und dem Ergebnis eines Fachwissenstests. Dazu erhebt sie beide Variablen bei N = 18678 Schülerinnen und Schülern in 778 Klassen und erhält ein Pearson's r von .01 mit einem BF₁₀ < 1/1000.` =  
        c(0,NA,NA,  1,0,NA,  0,0),
      
      `Ein Forscher untersucht, inwiefern Intelligenz und Gehalt korreliert sind. Dazu erhebt er beide Variablen bei N = 58678 Studierenden und erhält ein Pearson's r von .23 mit einem BF₁₀ > 100.` =  
        c(NA,1,0,  NA,0,0,  NA,1),
      
      `Eine Forscherin untersucht, inwiefern Gewissenhaftigkeit und schulische Leistung korreliert sind. Dazu erhebt er beide Variablen bei N = 678 Studierenden und erhält ein Pearson's r von .13 mit einem BF₁₀ = 63.` =      
        c(NA,1,0,  NA,0,0,  NA,1),
      
      `Ein Forscher untersucht, inwiefern die Fachkompetenz einer Lehrkraft mit dem Lernfortschritt der Schülerinnen und Schüler assoziiert sind. Dazu erfrasst er beide Variablen bei N = 142 Lehrkröften und deren Schülerinnen und Schüler und erhält ein Pearson's r von .34 mit einem BF₁₀ = 12.` =      
        c(NA,1,0,  NA,0,0,  NA,1),
      
      `Ein Forscher untersucht, inwiefern die Begeisterung für den pädagogischen Beruf mit dem Lernfortschritt der Schülerinnen und Schüler assoziiert sind. Dazu erhebt er beide Variablen bei N = 568 Studierenden und erhält ein Pearson's r von .11 mit einem BF₁₀ = 9` =      
        c(1,NA,0,  NA,0,0,  NA,1),
      
      `Ein Forscher untersucht, inwiefern die Verträglichkeit  für den pädagogischen Beruf mit dem Lernfortschritt der Schülerinnen und Schüler assoziiert sind. Dazu erhebt er beide Variablen bei N = 568 Studierenden und erhält ein Pearson's r von .11 mit einem BF₁₀ = 9` =      
        c(1,NA,0,  NA,0,0,  NA,1),
      
      `Ein Forscher untersucht, inwiefern Gehalt der Eltern mit dem Gehalt von Befragten korreliert sind. Dazu erhebt er beide Variablen bei N = 664 Studierenden und erhält ein Pearson's r von .13 mit einem BF₁₀ = 23.` =      
        c(NA,1,0,  NA,0,0,  NA,1),
      
      `Ein Forscher untersucht, inwiefern Intelligenz und Gehalt korreliert sind. Dazu erhebt er beide Variablen bei N = 56 Studierenden und erhält ein Pearson's r von .23 mit einem BF₁₀ = 2.5.` =      
        c(0,NA,0,  NA,1,NA,  0,1),
      
      `Eine Forscherin untersucht, inwiefern Gewissenhaftigkeit und schulische Leistung korreliert sind. Dazu erhebt er beide Variablen bei N = 23 Studierenden und erhält ein Pearson's r von .13 mit einem BF₁₀ = 0.8.` =      
        c(0,NA,0,  NA,1,NA,  0,1),
      
      `Ein Forscher untersucht, inwiefern die Fachkompetenz einer Lehrkraft mit dem Lernfortschritt der Schülerinnen und Schüler assoziiert sind. Dazu erfrasst er beide Variablen bei N = 43 Lehrkröften und deren Schülerinnen und Schüler und erhält ein Pearson's r von .34 mit einem BF₁₀ = 2.6` =      
        c(0,NA,0,  NA,1,NA,  0,1),
      
      `Ein Forscher untersucht, inwiefern die Begeisterung für den pädagogischen Beruf mit dem Lernfortschritt der Schülerinnen und Schüler assoziiert sind. Dazu erhebt er beide Variablen bei N = 68 Studierenden und erhält ein Pearson's r von .11 mit einem BF₁₀ = 0.7` =      
        c(0,NA,0,  NA,1,NA,  0,1),
      
      `Ein Forscher untersucht, inwiefern die Verträglichkeit  für den pädagogischen Beruf mit dem Lernfortschritt der Schülerinnen und Schüler assoziiert sind. Dazu erhebt er beide Variablen bei N = 53 Studierenden und erhält ein Pearson's r von .11 mit einem BF₁₀ = 2.1` =      
        c(0,NA,0,  NA,1,NA,  0,1),
      
      `Ein Forscher untersucht, inwiefern Gehalt der Eltern mit dem Gehalt von Befragten korreliert sind. Dazu erhebt er beide Variablen bei N = 62 Studierenden und erhält ein Pearson's r von .13 mit einem BF₁₀ = 2.9.` =      
        c(0,NA,0,  NA,1,NA,  0,1)
      
      ) %>%
    # shuffle order of questions
    relocate(1, 2, sample(2:ncol(.), ncol(.) - 2))
  
  ## Shuffle answers
  q_a_matrix_qashuffeled <- 
    reactive({
      # shuffle answers if one of both buttons is pressed
      input$reshuffle_task 
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
    checkboxGroupInput("answers_task",
                       "Bitte ankreuzen",
                       q_a_matrix_qashuffeled() %>% 
                         select(1, nth_task() + 1) %>% 
                         na.omit(.) %>% 
                         pull(Answers_and_Distractors)
                       )
  })
  
  ## Prompt task 
  output$prompt_task <- renderText({
    paste(names(q_a_matrix_qashuffeled())[2:ncol(q_a_matrix_qshuffeled)][nth_task()])
  }) 
  
  ## Correct answers ###
  correct_answers_task <- reactive({

    vector_of_correct_answers <- 
      q_a_matrix_qashuffeled() %>% 
      select(1, nth_task() + 1) %>% 
      na.omit(.) %>% 
      pull(2)
    
    correct_answers_task <- 
      q_a_matrix_qashuffeled() %>%
      select(1, nth_task() + 1) %>% 
      na.omit(.) %>% 
      filter(vector_of_correct_answers == 1) %>%
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
        HTML(paste("Richtig! <br><i>", 
                   learnr::random_praise(),
                   "<i>"))}else{
          HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:  <br>✓ ", 
                     paste(correct_answers_task(), collapse = ", <br>✓ "),
                     "<br><i>",
                     learnr::random_encouragement(),
                     "</i>"))
        }
    }
  })
  
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
      sheet_append("1AZf7EQk-M2Wgej3WJXG1J2xRdql8b7Xiq0SvIiZogUo",
                   tibble(PID = ifelse(is.null(url_vars()$PID), 
                                       "PID is missing", #to keep ncol constant
                                       url_vars()$PID), # Person identifier from URL
                          task_name = "Interpret_r_BF",
                          task_version = "repeatable_and_parametrized",
                          time = Sys.time(),
                          timezone = Sys.timezone(),
                          new_task = as.numeric(input$new_task),
                          reshuffle_task = as.numeric(input$reshuffle_task),
                          result = # correct or wrong sol. provided by student
                            case_when(is.null(input$answers_task) ~ 
                                        "false_solution",
                                      setequal(correct_answers_task(), 
                                               input$answers_task) ~ 
                                        "correct_solution",
                                      TRUE ~  "false_solution"),
                          ncorrect = 
                          # intersect(shouldbecrossed, defactocrossed) +
                          #   intersect(shouldbenotcrossed, defactonotcrossed) 
                          #     
                            length(intersect(correct_answers_task(), 
                                            input$answers_task)) +
                            length(intersect(
                              setdiff(q_a_matrix_qashuffeled() %>% 
                                        select(1, nth_task() + 1) %>% 
                                        na.omit(.) %>% 
                                        pull(Answers_and_Distractors), 
                                      correct_answers_task()),
                              setdiff(q_a_matrix_qashuffeled() %>% 
                                        select(1, nth_task() + 1) %>% 
                                        na.omit(.) %>% 
                                        pull(Answers_and_Distractors), 
                                      input$answers_task))),
                          n_q = q_a_matrix_qashuffeled() %>% 
                            select(1, nth_task() + 1) %>% 
                            na.omit(.) %>% 
                            nrow(.),
                          partial_credit = max(0,
                                               (n_q - 2*(n_q - ncorrect))/n_q)
                              
                          ),
                   sheet = 2)
  })
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)
