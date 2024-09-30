## Task Name: Unterschied_wiss_Guetekrit_erkennen ##############################
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
      h4("Aufgabe: Studienvergleich"),
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
      Answers_and_Distractors =  c("Externe Validität",
                                   "Interne Validität",
                                   "Ethische Strenge",
                                   "Präsentationsqualität"), 
      `Forschergruppe A untersucht in einem experimentellen Setting den Einfluss von Hunger auf die Leistung in einem Gedächtnistest. Dazu werden die 60 Studierenden in zwei Gruppen aufgeteilt. Die Kontrollgruppe kann ganz normal essen. Die Experimentalgruppe darf 12 h vor dem Experiment nicht mehr essen und auch nur ungesüßte Getränke zu sich nehmen. Im Gedächtnistest zeigte sich ein signifikanter Unterschied derart, dass die hungrigen Studierenden sich weniger Wörter merken konnten als die Kontrollgruppe. Forschergruppe B untersucht in einem experimentellen Setting den Einfluss von Hunger auf die Leistung in einem Gedächtnistest. Dazu werden die 20 Studierenden in zwei Gruppen aufgeteilt. Die Kontrollgruppe kann ganz normal essen. Die Experimentalgruppe darf 12 h vor dem Experiment nicht mehr essen und auch nur ungesüßte Getränke zu sich nehmen. Im Gedächtnistest zeigte sich ein signifikanter Unterschied derart, dass die hungrigen Studierenden sich weniger Wörter merken konnten als die Kontrollgruppe.`   = c(T,T,F,F),
      `Forschergruppe A hat an einer Stichprobe von 20 Grundschulen in Baden-Württemberg über drei Monate hinweg eine wöchentliche “bewegte Pause” für Lehrkräfte angeboten. Die Maßnahme wurde am Ende des Zeitraumes evaluiert und es zeigte sich, dass teilnehmende Lehrkräfte signifikant höhere Werte beim subjektiven Wohlbefinden in einem Fragebogen angaben, als zu Beginn der Maßnahme und auch im Vergleich zu einer Kontrollgruppe von Grundschullehrkräften, die ein solches Angebot nicht erhalten hatten. Die Forschenden leiten daraus eine Empfehlung zur Fortführung der Maßnahme ab. Forschergruppe B hat an einer Stichprobe von 20 Grundschulen in ganz Deutschland über drei Monate hinweg eine wöchentliche “bewegte Pause” für Lehrkräfte angeboten. Die Maßnahme wurde am Ende des Zeitraumes evaluiert und es zeigte sich, dass teilnehmende Lehrkräfte signifikant höhere Werte beim subjektiven Wohlbefinden in einem Fragebogen angaben, als zu Beginn der Maßnahme und auch im Vergleich zu einer Kontrollgruppe von Grundschullehrkräften, die ein solches Angebot nicht erhalten hatten. Die Forschenden leiten daraus eine Empfehlung zur Fortführung der Maßnahme ab.`    = c(T,F,F,F),
      `Zwei Forscherinnen planen eine Studie durchzuführen und die dadurch entstehenden Daten anschließend zu publizieren, um die Allgemeinheit an den Forschungsanstrengungen (und damit auch an Steuergeldern) möglichst umfangreich partizipieren zu lassen. Beide erzielen durch Recodierungen und Ausschluss von Variablen eine vollständige Anonymisierung des Datensatzes. Auf einem Workshop zur Datenpublikation erfahren beide, dass man datenschutzrechtlich gesehen die Proband*innen nicht darüber informieren muss, dass die anonymisierten Daten publiziert werden, die Wissenschaftsgesellschaften dies aber aus Fairnessgründen empfehlen. Eine Forscherin nimmt daraufhin einen Hinweis (“die Forschungsdaten dieses Projektes werden nach vollständiger Anonymisierung veröffentlicht”) in den Fragebogen auf, die andere nicht. Ansonsten sind ihre Studien identisch.` = c(F,F,T,F)) %>%
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
    paste(HTML("<i>Worin unterscheiden sich die beiden im Folgenden dargestellen Studien?</i><br>"),
          names(q_a_matrix_qashuffeled())[2:ncol(q_a_matrix_qshuffeled)][nth_task()],
          "")
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
                          task_name = "Unterschied_wiss_Guetekrit_erkennen",
                          task_version = "repeatable_and_parametrized",
                          time = Sys.time(),
                          timezone = Sys.timezone(),
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
                   sheet = 1)
  })
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)
