## Task name guetekriterien-gemischt-mc
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
      card_header("Aufgabe: Ist die Aussage wahr?",
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
      Answers_and_Distractors = c("wahr",
                                  "falsch"),
      `Eine Erhöhung der internen Validität führt nicht zwangsläufig zu einer Erhöhung der externen Validität, eher im Gegenteil."` =  c(T,F), # adjust line 132 if coded 0/1 #1
      `Auch ohne interne Validität kann ein hohes Maß an externer Validität einer Studie gegeben sein.` =  c(T,F), #2
      `Eine Erhöhung der internen Validität führt automatisch zu einer Erhöhung der externen Validität.` =  c(F,T),#3
      `Zur Einschätzung der Relevanz einer Studie gibt es keine feste Maßzahl.` =  c(T,F), #4
      `Eine Studie ist nur dann als relevant zu bezeichnen, wenn eine neue Theorie aus ihr abgeleitet werden kann.` =  c(F,T),#5
      #`Durch die Erhöhung der Stichprobengröße verringert sich die ethische Strenge.` =  c(T,F), #6 fachlich eher nicht richtig
      `Durch die Erhöhung der Stichprobengröße in einer Untersuchung erhöht sich die externe Validität.` =  c(T,F),#7
      `Präsentationsqualität wird gesteigert, wenn die Ergebnisse neben der Vorstellung im wissenschaftlichen Kontext auch in zusammengefasster und vereinfachter Form für ein breiteres Publikum zugänglich gemacht werden.` =  c(T,F),#8
      `Präsentationsqualität ist dann verletzt, wenn die Ergebnisse von Forschungskolleg*innen auch unter Einsicht in die Originaldaten und -materialien nicht nachvollzogen werden können.` =  c(T,F)) %>% #9
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
}


# Create Shiny object
shinyApp(ui = ui, server = server)
