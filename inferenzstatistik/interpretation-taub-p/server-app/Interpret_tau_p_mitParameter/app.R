## Task Name: Interpret_tau_p ######################################################

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
      h4("Kendall's τᵇ und p-Wert interpretieren"),
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
      Answers_and_Distractors = c("Es liegt eine signifikante (bei ⍺-Niveau = .05) Assoziation vor.",
                                  "Der vorliegende Effekt kann von den Daten auf die Population verallgemeinert werden.",
                                  "Der vorliegende Effekt kann nicht von den Daten auf die Population verallgemeinert werden.",
                                  
                                  "Es liegt Evidenz für die Nullhypothese (Kendall's τᵇ = 0) vor.",
                                  "Die Alternativhypothese ist wahr.", 
                                  "Die Nullhypothese ist wahr.",
                                  
                                  "Es liegt ein substantieller (mindestens kleiner) Effekt vor."),
      `Ein Forscher untersucht, inwiefern die tägliche Fernsehdauer mit der schulischen Leistung assoziiert ist. Dazu erhebt er beide Variablen bei N = 5805 Schülerinnen und Schülern und erhält ein Kendall's τᵇ von -.18 mit einem p < .001` =      
        c(1,1,NA, 0,0,NA, 1),
      
      `Ein Forscher untersucht, inwiefern die tägliche Fernsehdauer mit der schulischen Leistung assoziiert ist. Dazu erhebt er beide Variablen bei N = 24 Schülerinnen und Schüler und erhält ein Kendall's τᵇ von -.18 mit einem p = .426.` =      
        c(0,0,NA,  0,NA,0,  1),
      
      `Zwei Team-Klassenlehrerinnen vergeben unabhängig voneinander Grundschulempfehlungen (Werkrealschule, Realschule, Gymnasium) für N = 18 Schülerinnen und Schüler. Sie überprüfen die Übereinstimmung, indem sie Kendall's τᵇ berechnen. Dies liegt bei -.73 mit einem p = .426.` =      
        c(0,0,NA,  0,NA,0,  1),
      
      `Zwei Team-Klassenlehrerinnen vergeben über Jahre hinweg immer wieder unabhängig voneinander Grundschulempfehlungen für ihre gemeinsam unterrichteten vierten Klassen (Werkrealschule, Realschule, Gymnasium; insgesamt N = 148 Schülerinnen und Schüler. Sie überprüfen die Übereinstimmung, indem sie Kendall's τᵇ berechnen. Dies liegt bei -.73 mit einem p < .001.` =      
        c(1,1,NA,  0,0,NA,  1),
      
      `Ein Forscher untersucht, inwiefern der höchste Schulabschluss eines Probanden mit dem der Eltern des Probanden assoziiert ist. Dazu erhebt er beide Variablen bei N = 58 Studierenden und erhält ein Kendall's τᵇ von .3 mit einem p = .021.` =      
        c(1,1,NA, 0,0,NA, 1),
      `Ein Forscher untersucht, inwiefern der höchste Schulabschluss eines Probanden mit dem der Eltern des Probanden assoziiert ist. Dazu erhebt er beide Variablen bei N = 17 Studierenden und erhält ein Kendall's τᵇ von .3 mit einem p = .228.` =      
        c(0,NA,1, 0,NA,0, 1),
      `Ein Forscher untersucht, inwiefern der höchste Schulabschluss eines Probanden mit dem der Eltern des Probanden assoziiert ist. Dazu erhebt er beide Variablen bei N = 4312 Studierenden und erhält ein Kendall's τᵇ von .04 mit einem p = .008.` =      
        c(1,1,NA, 0,0,NA, 0),
      `Ein Forscher untersucht, inwiefern der höchste Schulabschluss eines Probanden mit dem der Eltern des Probanden assoziiert ist. Dazu erhebt er beide Variablen bei N = 125 Studierenden und erhält ein Kendall's τᵇ von .03 mit einem p = .75.` =      
        c(0,NA,1, 0,NA,0, 0),
      
      `Eine Studentin untersucht in ihrer Masterarbeit, inwiefern Pärchen nach einem gemeinsamen Restaurantbesuch ähnliche Onlinebewertungen für das Essen abgeben (0-5 Sterne). Dazu befragt sie N = 63 Päärchen und erhält ein Kendall's τᵇ von .28 mit einem p = .019.` =      
        c(1,1,NA, 0,0,NA, 1),
      `Eine Studentin untersucht in ihrer Masterarbeit, inwiefern Pärchen nach einem gemeinsamen Restaurantbesuch ähnliche Onlinbewertungen für das Essen abgeben (0-5 Sterne). Dazu befragt sie N = 17 Päärchen und erhält ein Kendall's τᵇ von .30 mit einem p = .318.` =      
        c(0,NA,1, 0,NA,0, 1),
      `Eine Studentin untersucht in ihrer Masterarbeit, inwiefern Pärchen nach einem gemeinsamen Restaurantbesuch ähnliche Onlinbewertungen für das Essen abgeben (0-5 Sterne). Dazu befragt sie N = 5312 Päärchen und erhält ein Kendall's τᵇ von .03 mit einem p = .007.` =      
        c(1,1,NA, 0,0,NA, 0),
      `Eine Studentin untersucht in ihrer Masterarbeit, inwiefern Pärchen nach einem gemeinsamen Restaurantbesuch ähnliche Onlinbewertungen für das Essen abgeben (0-5 Sterne). Dazu befragt sie N = 131 Päärchen und erhält ein Kendall's τᵇ von .04 mit einem p = .71.` =      
        c(0,NA,1, 0,NA,0, 0),
      
      `Ein Student untersucht in seiner Masterarbeit, inwiefern Pärchen ähnliche Ernährungsformen (0 = vegane Ernährung, 1 = ovo-lacto Vegetarismus, 2 = Vegetarismus, 3 = keine Einschränkung) zeigen. Dazu befragt er N = 65 Päärchen und erhält ein Kendall's τᵇ von .30 mit einem p = .019.` =      
        c(1,1,NA, 0,0,NA, 1),
      `Ein Student untersucht in seiner Masterarbeit, inwiefern Pärchen ähnliche Ernährungsformen (0 = vegane Ernährung, 1 = ovo-lacto Vegetarismus, 2 = Vegtarismus, 3 = keine Einschränkung) zeigen. Dazu befragt er N = 22 Päärchen und erhält ein Kendall's τᵇ von .29 mit einem p = .418.` =      
        c(0,NA,1, 0,NA,0, 1),
      `Ein Student untersucht in seiner Masterarbeit, inwiefern Pärchen ähnliche Ernährungsformen (0 = vegane Ernährung, 1 = ovo-lacto Vegetarismus, 2 = Vegtarismus, 3 = keine Einschränkung) zeigen. Dazu befragt er N = 3315 Päärchen und erhält ein Kendall's τᵇ von .02 mit einem p = .004.` =      
        c(1,1,NA, 0,0,NA, 0),
      `Ein Student untersucht in seiner Masterarbeit, inwiefern Pärchen ähnliche Ernährungsformen (0 = vegane Ernährung, 1 = ovo-lacto Vegetarismus, 2 = Vegtarismus, 3 = keine Einschränkung) zeigen. Dazu befragt er N = 1119 Pärchen und erhält ein Kendall's τᵇ von .03 mit einem p = .61.` =      
        c(0,NA,1, 0,NA,0, 0),
      
      `Ein Thinktank untersucht inwiefern Väter und Söhne sich auf einer Skala links - eher links - mitte - eher rechts - rechts politisch ähnlich verorten und befragt dazu N = 65 Väter mit ihren Söhnen. Die Daten zeigen ein Kendall's τᵇ von .35 mit einem p = .019.` =      
        c(1,1,NA, 0,0,NA, 1),
      `Ein Thinktank untersucht inwiefern Väter und Söhne sich auf einer Skala links - eher link - mitte - eher rechts - rechts politisch ähnlich verorten und befragt dazu N = 22 Väter mit ihren Söhnen. Die Daten zeigen ein Kendall's τᵇ von .28 mit einem p = .538.` =      
        c(0,NA,1, 0,NA,0, 1),
      `Ein Thinktank untersucht inwiefern Väter und Söhne sich auf einer Skala links - eher link - mitte - eher rechts - rechts politisch ähnlich verorten und befragt dazu N = 3315 Väter mit ihren Söhnen. Die Daten zeigen ein Kendall's τᵇ von .03 mit einem p = .002.` =      
        c(1,1,NA, 0,0,NA, 0),
      `Ein Thinktank untersucht inwiefern Väter und Söhne sich auf einer Skala links - eher link - mitte - eher rechts - rechts politisch ähnlich verorten und befragt dazu N = 1119 Väter mit ihren Söhnen. Die Daten zeigen ein Kendall's τᵇ von .03 mit einem p = .71.` =      
        c(0,NA,1, 0,NA,0, 0)) %>%
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
    names(q_a_matrix_qashuffeled())[2:ncol(q_a_matrix_qshuffeled)][nth_task()]
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
                          task_name = "Interpret_tau_p",
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
