## Task Name: Interpret_U3_p ######################################################

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
      h4("Cohen's U₃ und p-Wert interpretieren"),
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
      Answers_and_Distractors = c("Es liegt eine signifikante (bei ⍺-Niveau = .05) Mittelwertsdifferenz vor.",
                                  "Der vorliegende Effekt kann von den Daten auf die Population verallgemeinert werden.",
                                  "Der vorliegende Effekt kann nicht von den Daten auf die Population verallgemeinert werden.",
                                  
                                  "Es liegt Evidenz für die Nullhypothese (Cohen's $U_3$ = .5) vor.",
                                  "Die Alternativhypothese ist wahr.", 
                                  "Die Nullhypothese ist wahr.",
                                  
                                  "Es liegt ein substantieller (mindestens kleiner) Effekt vor."),
      
      `Eine Forscherin untersucht, inwiefern sich Lehramtsstudierende von anderen Studierenden bzgl. der Persönlichkeitseigenschaft »Offenheit« (Extraversion) unterscheiden. Sie findet eine im Durchschnitt größere Offenheit bei den Lehramtsstudierenden (Cohen's U₃ = .61, p = .001)` =  
        c(1,1,NA,  0,0,NA,  1),
      
      `Eine Forscherin untersucht, inwiefern sich Lehramtsstudierende von anderen Studierenden bzgl. des sozialen Berufsinteresses unterscheiden. Sie findet eine im Durchschnitt ein höheres soziales Interesse bei den Lehramtsstudierenden (Cohen's U₃ = .81, p = .001)` =  
        c(1,1,NA,  0,0,NA,  1),
      
      `Ein Forscher untersucht, inwiefern sich Lehramtsstudierende von anderen Studierenden bzgl. ihrer kognitiven Fähigkeiten unterscheiden. Sie findet ein Cohen's U₃ = .52 mit einem p-Wert von .562` =  
        c(0,NA,1,  0,NA,0,  0),
      
      `Ein Dozent untersucht, inwiefern sich Klausurergebnisse von Studierenden unterscheiden, je nachdem ob sie konzentriert, direkt vor der Klausur lernen (massed practice) oder verteilt über das Semester (spaced learning). Er findet ein Cohen's U₃ = .72 zugunsten des spaced learning mit einem p-Wert von .00001` =  
        c(1,1,NA,  0,0,NA,  1),
      
      `Ein Dozent untersucht, inwiefern sich Klausurergebnisse von Studierenden unterscheiden, wenn er eine zufällig ausgewählte Hälfte der Studierenden dazu anleitet, während des Lernens selbst zu verbalisieren und sich selbst Fragen zu stellen. Er findet ein Cohen's U₃ = .709 mit einem p-Wert < .001` =  
        c(1,1,NA,  0,0,NA,  1),
      
      `Eine Forscherin untersucht, ob die, durch die Lehrkraft eingeschätzte, Aufmerksamkeit der Schülerinnen und Schüler durch die Anwesenheit eines Schulhundes während der Stunde erhöht wird. In einem experimentellen Design vergleicht sie hierfür Stunden mit und ohne Anwesenheit eines Schulhundes in 35 Klassen. Sie findet ein Cohen's U₃ = .45 mit einem p-Wert = .862` =  
        c(0,NA,1,  0, NA,0,  0),
      `Ein Forschungsteam beschäftigt sich mit der Effizienz von Bewegungspausen im Unterricht. Dazu vergleicht sie Unterrichtsstunden in denen Bewegungseinheiten stattgefunden haben, mit denen ohne Bewegungsunterbrechungen. Am Ende der Stunden bearbeiten die Schülerinnen und Schüler jeweils den Aufmerksamkeits-Belastungs-Test d2. Sie finden ein Cohen's U₃ = .59 mit einem p-Wert = .678` =  
        c(0,0,NA,  0, NA,0,  1),
      `Ein Doktorand untersucht den Effekt von Schriftarten auf die Lesefähigkeiten von Schülerinnen und Schülern mit Dyslexie. Er vergleicht dafür die Leistungen eines Textes in der serifenlosen Schriftart Comic Sans mit der Serifenschrift Times New Roman. Er findet ein Cohen's U₃ = .602 mit einem p-Wert = .09` =  
        c(0,0,NA,  0, NA,0,  1),
      `Eine Forscherin untersucht den Einfluss von Lob auf die Motivation von Schülerinnen und Schülern. Dazu teilt sie die 8 Lehrkräfte zufällig in eine Kontrollbedingung und eine Experimentalbedingung ein. Die Kontrollgruppe erhält keine gesonderten Instruktionen, während die Lehrkräfte der Experimentalgruppe aufgefordert werden, in der kommenden Woche die Schülerinnen und Schüler so viel wie möglich zu loben. Am Ende der Woche vergleicht sie die Werte in einem Motivationsfragebogen. Sie findet ein Cohen's U₃ = .309 mit einem p-Wert = .102` =  
        c(0,NA,1,  0, NA,0,  1),
      `Ein Forscher vergleicht die Unterrichtsqualität zwischen öffentlichen und privaten Schulen. Er findet ein Cohen's U₃ = .523 mit einem p-Wert = .245` =  
        c(0,NA,1,  0, NA,0,  0),
      
      
      
      
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
    withMathJax(
    checkboxGroupInput("answers_task",
                       "Bitte ankreuzen",
                       q_a_matrix_qashuffeled() %>% 
                         dplyr::select(1, nth_task() + 1) %>% 
                         na.omit(.) %>% 
                         pull(Answers_and_Distractors)
                       )
    )
  })
  
  ## Prompt task 
  output$prompt_task <- renderText({
    paste(names(q_a_matrix_qashuffeled())[2:ncol(q_a_matrix_qshuffeled)][nth_task()],
          "<br><b> Welche Aussagen sind wahr?</b>")
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
                          task_name = "Interpret_U3_p",
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
