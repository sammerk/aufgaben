# Task Name: Inferenzniveau_erkennen ######################
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
        "Aufgabe: Inferenzniveau",
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
  
 
  ## Parameter solution matrix for task  ####
  q_a_matrix_qshuffeled <- 
    tibble(
      Answers_and_Distractors = c("als hochinferent", "als niedriginferent"),
      `Anzahl der Wortmeldungen der einzelnen Schüler*innen` = c(F,T), #1
      `Sozialform des Unterrichts` = c(F,T),
      `Anteil an Mädchen in der Klasse` = c(F,T),
      `Einsatz von Arbeitsblättern` = c(F,T),
      `Anzahl Schüler*innen mit braunen Haaren` = c(F,T),
      `Redeanteil Lehrkraft` = c(F,T),
      `Lautstärke während Einzelarbeit` = c(F,T),
      `Nutzung digitaler Medien` = c(F,T),
      `Zeitpunkt der Erteilung von Hausaufgaben` = c(F,T),
      `Anzahl fehlender Schüler*innen` = c(F,T), #10
      `Unterrichtsklima` = c(T,F),
      `Aufmerksamkeit der Schüler*innen` = c(T,F),
      `Klassenführung` = c(T,F),
      `Kompetenz der Lehrkraft` = c(T,F),
      `Interesse der Schüler*innen am Thema` = c(T,F),
      `kognitive Aktivierung` = c(T,F),
      `Unterrichtsstörungen` = c(T,F),
      `Grad der Differenzierung` = c(T,F),
      `Didaktische Reduktion` = c(T,F),
      `Akkustische Verständlichkeit der Lehrkraft` = c(T,F)) %>% 
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
    paste("<b>Bezeichnet man die folgende Variable sinnvollerweise als hoch- oder niedriginferent?</b>",
          names(q_a_matrix_qashuffeled())[2:ncol(q_a_matrix_qshuffeled)][nth_task()])
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
