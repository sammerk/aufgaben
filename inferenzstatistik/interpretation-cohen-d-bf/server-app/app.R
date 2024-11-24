## Task Name: Interpret_d_BF ######################################################

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
      h4("Cohen's d und Bayes Faktor interpretieren"),
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
      Answers_and_Distractors = 
        c("Es liegt moderate Evidenz (3 < BF₁₀ < 10) für die Alternativhypothese (Cohen's d > 0) vor.",
          "Es liegt starke Evidenz (10 ≤ BF₁₀) für die Alternativhypothese (Cohen's d > 0) vor.",
          "Es liegt moderate Evidenz (1/10 < BF₁₀ < 1/3) für die Nullhypothese (Cohen's d = 0) vor.",
          
          "Es liegt starke Evidenz (BF₁₀ ≤ 1/10) für die Nullhypothese (Cohen's d = 0) vor.",
          "Die inferenzstatistische Prüfung ist inkonklusiv (1/3 < BF₁₀ < 3)",
          "Die Alternativhypothese ist wahr.", 
          
          "Die Nullhypothese ist wahr.",
          "Es liegt ein substantieller (mindestens kleiner) Effekt vor."),
      
      `Eine Bildungsverwaltung führt in zufällig ausgewählten Kommunen einen monetären Bonus für Lehrerinnen und Lehrer ein, die in der Schulleiterbewertung und in Tests ihrer Schülerinnen und Schüler besonders gut abschneiden. Sie erfasst auch ob diese Kommunen als Ganzes durch dieses Anreizsystem besser in der Schülerleistung werden und findet ein Cohen's d = .05 und einen BF₁₀ > 10000` =  
        c(0,1,NA,  0,0,0,  NA,0),
      `Ein Forscherteam untersucht, ob Schülerinnen und Schüler, die an einem Vorschulprogramm teilgenommen haben nach der 1. Klasse bessere Leseleistungen erzielen als diejenigen, die an keinem Vorschulprogramm teilgenommen haben. Sie erhalten ein Cohen's d = .45 und einen BF₁₀ = 128` =  
        c(0,1,NA,  0,0,0,  NA,1),
      `Eine Forscherin untersucht, ob Kinder mit körperlichen Vorerkrankungen im schulischen Umfeld schlechter abschneiden, als Schülerinnen und Schüler bei denen keinerlei chronischen Erkrankungen bekannt sind. Sie erhält ein Cohen's d = .23 und einen BF₁₀ = 19` =  
        c(0,1,NA,  0,0,0,  NA,1),
      `Ein Forscher untersucht in einem experimentellen Design die Wirksamkeit eines Trainings zur Reduktion von Prüfungsangst bei Schülerinnen und Schülern in Klassenstufe 7. Ihn interessiert, ob sich die Prüfungsleistungen im Fach Mathematik dadurch verbessern lassen.  Er erhält ein Cohen's d = .04 und einen BF₁₀ = 9` =  
        c(1,0,NA,  0,0,NA,  0,0),
      `Eine Forschergruppe beschäftigt sich mit Einflüssen im häuslichen Umfeld auf die Bildungswege von Schülerinnen und Schülern. Dazu untersuchen sie in zwei repräsentativen Stichproben, ob Schülerinnen und Schüler an Gymnasien zu Hause auf mehr Bücher zugreifen können als Schülerinnen und Schüler an Realschulen. Sie erhalten ein Cohen's d = .56 und einen BF₁₀ > 1000` =  
        c(0,1,NA,  0,0,0,  NA,1),
      `Eine Doktorandin untersucht für ihre Doktorarbeit die Unterschiede zwischen einfachem Feedback (richtig/falsch) und informativen Feedback (richtig/falsch plus richtige Lösung bzw. weitere Hinweise). In einem Experiment mit 240 Studentinnen und Studenten hat sie eigens für das Experiment einen Text mit Faktenwissen gestaltet. Eine Gruppe bekommt nach der Lernphase Übungsaufgaben mit einfachem Feedback, die andere Gruppe mit informativem Feedback. Beim Vergleich der Gruppen in einem abschließenden Test erhält sie ein Cohen's d = .47 und einen BF₁₀ = 210` =  
        c(0,1,NA,  0,0,0,  NA,1),
      `Eine Forscherin untersucht, ob Lehramtsstudierende, die nach dem 01.01.2000 geboren wurden in einem Test zur digitalen Kompetenz besser abschneiden als Lehramtsstudierende, die vor dem 01.01.2000 geboren wurden. Sie erhält ein Cohen's d = .2 und einen BF₁₀ = .027` =  
        c(0,0,NA,  1,0,0,  NA,1),
      `Ein Forscher beschäftigt sich mit der Frage, ob es sinnvoll ist Schülerinnen und Schüler in weiterführenden Schulen eine Schulung zum Speedreading zu geben. Dazu überprüft er zunächst experimentell, ob die Schülerinnen und Schüler davon profitieren. Er vergleicht daher zwei Gruppen (mit und ohne Einführung im Speedreading) in Bezug auf Ihr Textverständnis. Er ermittelt ein Cohen's d =- .2 und einen BF₁₀ = .5` =  
        c(NA,0,0,  0,1,0,  NA,1),
      `Ein Forscher untersucht, ob Schüler bessere schulische Leistungen im Leseverständnis erzielen, wenn Sie in der Grundschule von Lehrern unterrichtet werden. Dazu werden an zehn zweizügigen Grundschulen in einer lang angelegten Studie die neuen Erstklässler zufällig in eine Klasse mit Klassenlehrer und eine Klasse mit Klassenlehrerin zugeteilt. Beim Vergleich nach Klassenstufe 2 erhalten die Forscher ein Cohen's d = .02 und einen BF₁₀ = 0.09` =  
        c(0,0,NA,  1,0,NA,  0,0),
      `Eine Doktorandin möchte untersuchen, ob klassische Musik lernförderlich ist. Sie teilt 10 Ganztagsschulen zufällig einer Kontrollgruppe und eine Experimentalgruppe zu. In der Kontrollgruppe macht sie keine Vorgaben. In der Experimentalgruppe wird über das gesamte Schuljahr während der Lernzeit leise klassische Musik abgespielt. Am Anfang und am Ende des Schuljahres absolvieren alle Schülerinnen und Schüler einen standardisierten Wissenstest. Beim Vergleich der Lernzuwächse in den beiden Gruppen findet sie ein Cohen's d = -.03 und einen BF₁₀ < 1/1000` =  
        c(0,0,NA,  1,0,NA,  0,0),
      
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
                          task_name = "Interpret_d_BF",
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
