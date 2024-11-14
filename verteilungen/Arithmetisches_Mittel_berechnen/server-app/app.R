# Task Name: Arithmetisches_Mittel_berechnen ###################################

library(shiny)
library(miniUI)
library(shinyjs)
library(hrbrthemes)
library(tidyverse)
library(learnr)
library(shinycssloaders)
set.seed(25051982)




## UI #########################################################################
ui <- miniPage(
  useShinyjs(),
  miniContentPanel(
    wellPanel(
      h4("Arithmetisches Mittel berechnen"),
      htmlOutput("prompt_task")
    ),
    shinyjs::hidden(wellPanel(id = "feedbackpanel_task",
                              withSpinner(
                                htmlOutput("feedback_task"),
                                proxy.height = "50px",
                                color = "#8cd000"))), 
    wellPanel(
      numericInput("answers_task", 
                   "Ihre Lösung:",
                   value = NA),
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
  #      - draw a random sample
  #      - compare solution to answer
  
  
  
  ## Data for task ####
  data_task <- reactive({
    input$new_task
    sample(1:100, 7)
  })
  
  ## Render UI for question ####
  output$prompt_task <- renderText({
    paste("Berechnen Sie das arithmetische Mittel (Mean) der Datenreihe<br>",
          "X = ", 
          paste(data_task(), collapse=", "),
          "<br><i>Runden Sie das Ergebnis auf zwei Nachkommastellen.<i>")
  })
  
  
  ## Correct solution ###
  correct_answers_task <- reactive({
    round2(mean(data_task()), n = 2)
  })
  

  ## Feedback task  ####
  output$feedback_task <- renderText({   
    
    if(is.na(input$answers_task)){
      HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:", 
                 correct_answers_task(),
                 "<br><i>",
                 learnr::random_encouragement(),
                 "</i>"))
    }else{
      if(round2(input$answers_task, 2) == correct_answers_task()){
        paste("Richtig!", learnr::random_praise())}else{
          HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:", 
                     correct_answers_task(),
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
