# Task Name: Itempolaritaet_erkennen ######################
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
        "Aufgabe: Itempolarität",
      class = "bg-dark"),
    card_body(
      htmlOutput("prompt_task")
    )
  ),
  
  # komplett kopierbar für SC Aufgaben
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
      Answers_and_Distractors = c("als unipolar", "als bipolar"),
      `Stamm: Wie angespannt fühlen Sie sich im Moment?; Anwortskala: gar nicht … sehr` = c(T,F),
      `Stamm: Wie wichtig ist Ihnen der Umweltschutz?; Antwortskala: gar nicht wichtig … sehr wichtig` = c(T,F),
      `Stamm: Wie hilfsbereit schätzen Sie sich selbst ein?; Antwortskala: kaum … sehr` = c(T,F),
      `Stamm: Wie positiv ist heute Ihre Stimmung?; Antwortskala: gar nicht … sehr` = c(T,F),
      `Stamm: Wie sehr stimmen Sie der Aussage zu?; Antwortskala: gar nicht … sehr stark` = c(T,F),
      `Stamm: Wie zufrieden sind Sie mit der Lehrveranstaltung?; Antwortskala: gar nicht zufrieden … sehr zufrieden` = c(T,F), #6
      
      `Stamm: Im Augenblick fühle ich mich…; Anwortskala: gespannt … gelöst` = c(F,T),
      `Stamm: Welchen Stellenwert hat für Sie der Umweltschutz? unwichtig … wichtig` = c(F,T),
      `Stamm: Wie schätzen Sie sich selbst ein?; Antwortskala: hilfsbereit … egoistisch` = c(F,T),
      `Stamm: Wie ist Ihre Stimmung heute?; Antwortskala: sehr negativ … sehr positiv` = c(F,T),
      `Stamm: Wie schätzen Sie die Aussage ein?; Antwortskala: lehne vollständig ab … stimme vollständig zu` = c(F,T),
      `Stamm: Wie zufrieden sind Sie mit der Lehrveranstaltung?; Antwortskala: sehr unzufrieden … äußerst zufrieden` = c(F,T)) %>% 
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
    paste("<br><b>Bezeichnet man das folgende Item sinnvollerweise als uni- oder bipolar?</b><br>",
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
