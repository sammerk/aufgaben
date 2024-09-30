# Task Name: Deskriptive_Explanantive_Explorative_Studien ######################

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
      card_header(
          "Aufgabe: Erkentnisinteresse in Forschungsfrage erkennen",
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
                   icon = icon("send")),
      actionButton("reshuffle_task", 
                   "Diese Aufgabe wiederholen",
                   icon = icon("repeat")),
      actionButton("new_task", 
                   "Neue Aufgabe derselben Art",
                   icon = icon("plus"))
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
      Answers_and_Distractors = c("Deskriptive Studie", "Explanative Studie", "Explorative Studie"),
      `Ein Forscherteam geht der Frage nach, wie viele Studierende an Pädagogischen Hochschulen Eltern sind.` =  c(T,F,F), # adjust line 132 if coded 0/1
      `Ein Forscherteam sucht nach möglichen Einflussfaktoren elterlicher Unterstützung auf die schulbezogene Motivation ihrer Kinder.` =  c(F,F,T),
      `Ein Forscherteam untersucht, ob sich die positiven Effekte einer Lehrmethode bei der Anwendung in der Sekundarstufe auch für die Primarstufe replizieren lassen.` =  c(F,T,F),
      `Ein Forscherteam erfasst die Altersstruktur der angestellten Lehrkräfte zum Stichtag 01.01.2022.` =  c(T,F,F),
      `Ein Forscherteam geht der Frage nach, ob die simultane Präsentation von Bild und Ton entsprechend der Dual Code Theory auch den Erwerb von Wörtern einer Fremdsprache fördert.` =  c(F,T,F),
      `Ein Forscherteam sucht nach personellen Faktoren, die mit Studienerfolg einhergehen.` =  c(F,F,T),
      `Ein Forscherteam untersucht die Anzahl von Schüler*innen mit Deutsch als Zweitsprache in den verschiedenen Stadtteilen von Karlsruhe. ` =  c(T,F,F),
      `Ein Forscherteam untersucht die Hypothese, ob das Lernen mit Pausen (spaced learning) schon bei Kindern im Grundschulalter zu besseren Lernleistungen im Vergleich zu massiertem Lernen führt.` =  c(F,T,F),
      `Ein Forscherteam sucht nach Faktoren, die den Abbruch des Lehramtsstudiums in höheren Fachsemestern beeinflussen.` =  c(F,F,T)) %>% 
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
          "<br><b>Wird dabei eine deskriptive, explanative oder explorative Studie durchgeführt?</b>")
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
