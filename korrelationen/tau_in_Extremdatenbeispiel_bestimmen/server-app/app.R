# Task Name: Kendalls_tau_EE ##############################################

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
  withMathJax(),
  useShinyjs(),
  miniContentPanel(
    wellPanel(
      h4("Kendalls \\(\\tau_b\\)"),
      p(HTML("Welches Kendall's \\(\\tau_b\\) wird sich für folgendes Minimaldatenbeispiel zeigen?<br><i>(Eine Rechnung ist nicht unbedingt nötig)</i>")),
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
                   "Ihre Lösung: \\(\\tau_b\\) = ",
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
       tibble(A = c(2,4,6,8)*sample(c(-1, 1, -2, 2), 1),
              B = 2*A,
              C = -A,
              D = -2*A,
              E = A + sample(1:3, 1),
              `F` = A - sample(1:3, 1))
     
     data$G <- c(data$A[3], data$A[1], data$A[4], data$A[2])
     
     data <- 
       data %>% 
       relocate(1, 2, sample(3:ncol(.), ncol(.) - 2))
    
    return(data)
  })
  
  output$table <- renderTable({
    data()[,c(1, input$new_task %% 6 + 2)]
    }, digits = 0)
  
  
  ## Correct solution ###
  correct_answers_task <- reactive({
    cor(data()[,2], data()[,c(input$new_task %% 6 + 2)], method = "kendall")
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
                          task_name = "Kendalls_tau_EE",
                          task_version = "repeatable_and_parametrized",
                          time = Sys.time(),
                          timezone = Sys.timezone(),
                          new_task = as.numeric(input$new_task),
                          reshuffle_task = as.numeric(input$reshuffle_task),
                          result = case_when(is.na(input$answers_task) ~ "false_solution",
                                             round(input$answers_task, 2) == correct_answers_task() ~ "correct solution",
                                             T ~ "false_solution")),
                   sheet = 2)
    }
  })
}


# Create Shiny object
shinyApp(ui = ui, server = server)
