# Task Name: steigerung-externe-validität ######################
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
        "Aufgabe: Steigerung der externen Validität"),
      class = "bg-dark"),
    card_body(
      HTML('Eine Forscherin untersucht, ob die Verwendung dynamischer Geometriesoftware (z.B. GeoGebra) den Erwerb von konzeptuellem Wissen fördert. Dazu erfasst sie Schülerleistungen der 7. Klasse N = 63 anhand eines entsprechenden Tests nach der Durchführung der Unterrichtseinheit zum "Satz vom Umkreis" bei Lehrerinnen, die entweder mit oder ohne die dynamische Geometriesoftware arbeiten und über diesen Einsatz auch selbst entscheiden konnten. Es konnte ein statistisch bedeutsamer Unterschied zugunsten der Lernenden, die GeoGebra genutzt haben, nachgewiesen werden.
Eine Forschergruppe möchte sich die Vorteile der Verwendung von dynamischer Geometriesoftware genauer anschauen. Sie nutzt diese Studie als Grundlage für weitere Forschungsbemühungen. Sie will jedoch die externe Validität erhöhen. Ist die folgende Maßnahme hierzu zielführend?'),
      htmlOutput("prompt_task")
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
      Answers_and_Distractors = c("zielführend",
                                  "nicht zielführend"),
      `Erhöhung der Stichprobengröße` =  c(T,F), # adjust line 132 if coded 0/1 #1
      `Wahl eines experimentellen Forschungsdesigns` =  c(F,T), #2
      `Randomisierte Zuteilung der Teilnehmenden zu Lehrer*innen mit und ohne Nutzung der dynamischen Geometriesoftware` =  c(F,T),#3
      `Alle Mädchen mit GeoGebra unterrichten und alle Jungen ohne` =  c(F,T), #4
      `Kontrolle von Störvariablen, wie Vorwissen der Schüler*innen` =  c(F,T),#5
      `Die Studie im Labor durchführen` =  c(F,T),#6
      `Untersuchung bei Studierenden oder anderen Klassenstufen planen` =  c(T,F),) %>% #7
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
    paste("<b>",
          names(q_a_matrix_qashuffeled())[2:ncol(q_a_matrix_qshuffeled)][nth_task()],
          "</b>")
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
                          task_name = "Steigerung_externe_Validitaet",
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
