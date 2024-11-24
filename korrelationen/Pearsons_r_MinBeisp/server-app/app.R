# Task Name: Pearsons_r_EE ##############################################

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

set.seed(25051982)

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
      h4("Pearson's r"),
      htmlOutput("prompt_task"),
      tableOutput("table"), # prop rounded 2 digits
     # tableOutput("table2") # abs rounded 0 digits
    ),
    shinyjs::hidden(wellPanel(id = "feedbackpanel_task",
                              withSpinner(
                                htmlOutput("feedback_task"),
                                proxy.height = "50px",
                                color = "#8cd000"))), 
    wellPanel(
      numericInput("answers_task", 
                   "Ihre Lösung: r = ",
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
  data <- reactive({
     data <- 
       tibble(A = 1:4 + sample(-3:3, 1),
              B = A,
              C = -A,
              D = 2*A,
              E = -2*A,
              #G = - sample(1:3, 1, re),
              H = c(1,4,4,1),
              I = c(5,2,2,5))
     
     data <- 
       data %>% 
       relocate(1, 2, sample(3:ncol(.), ncol(.) - 2))
    
    return(data)
  })
  
  output$table <- renderTable({
    data()[,c(1, input$new_task %% 8 + 2)]
    }, digits = 0)
  

  
  ## Render UI for question ####
  output$prompt_task <- renderText({
    "Welches Pearson's r wird sich für folgendes Minimaldatenbeispiel zeigen?<br><i>(Eine Rechnung ist nicht unbedingt nötig)</i>"
  })
  
  
  ## Correct solution ###
  correct_answers_task <- reactive({
    cor(data()[,2], data()[,c(input$new_task %% 8 + 2)])
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
                          task_name = "Pearsons_r_EE",
                          task_version = "repeatable_and_parametrized",
                          time = Sys.time(),
                          timezone = Sys.timezone(),
                          
                          new_task = as.numeric(input$new_task),
                          reshuffle_task = as.numeric(input$reshuffle_task),
                          
                          result = case_when(is.na(input$answers_task) ~ "false_solution",
                                             round(input$answers_task, 2) == correct_answers_task() ~ "correct_solution",
                                             T ~ "false_solution")),
                   sheet = 2)
    }
  })
}







# Create Shiny object
shinyApp(ui = ui, server = server)
