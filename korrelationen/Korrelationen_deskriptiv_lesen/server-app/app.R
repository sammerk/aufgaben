## Task Name: Korrelogramme_deskriptiv_lesen ####################################################

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
library(bayestestR)

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
      h4("Korrelogramme interpretieren"),
      htmlOutput("prompt_task"),
      plotOutput("plot", height = "400px", width = "400px")
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
  
  ## Matrix with variables and values
  shape <- reactive({
    input$new_task
    
    sample(c("unif", "skew1", 
             "skew2", "normal","u"))
    })
  
  data <- reactive({
    input$new_task
    
    data <- 
      tibble(u = sample(distribution_beta(100, .6, .6), 100),
             unif = sample(distribution_beta(100, 1, 1), 100),
             normal = sample(distribution_beta(100, 4, 4), 100),
             skew1 = sample(distribution_beta(100, 1, 8), 100),
             skew2 = sample(distribution_beta(100, 8, 1), 100)
      ) %>% 
      select(as.character(shape()))
    
    names(data) <- "A"
    
    data$B <- (data$A + rnorm(100, 0, sample(c(.15, 60), 1)))*sample(c(1,-1),1)
    
    return(data)
  })
  
  output$plot <- renderPlot({
    
    ggplot(data(), aes(A, B)) + 
      geom_point(color = "#8CD000") +
      stat_smooth(se = F, method = "lm", color = "#8CD000") +
      geom_rug(color = "#8CD000", alpha = .5) + 
      theme_ipsum() + 
      ggtitle("Streudiagramm") +
      labs(caption = "Die x- und y-Koordinaten jedes Punktes sind\nauf den jeweiligen Achsen als Striche dargestellt")
    
  })
  
  
  
  questions_answers <- reactive({
    input$reshuffle_task
    input$new_task
    tibble(
      questions = c("A ist linksschief verteilt",
                    "A ist rechtsschief verteilt",
                    "A und B sind stark positiv korreliert",
                    "A und B sind stark negativ korreliert"),
      answers = c(DescTools::Skew(data()$A) < -1,
                  DescTools::Skew(data()$A) > 1,
                  cor(data()$A, data()$B) > .4,
                  cor(data()$A, data()$B) < -.4)
      )%>% 
      sample_frac(1)
  })
  

  ## Render UI for Answers ###
  output$ui_answers_task <- renderUI({
    input$reshuffle_task
    input$new_task
    checkboxGroupInput("answers_task",
                       "Bitte zutreffendes ankreuzen",
                       questions_answers()$questions
                       )
  })
  
  ## Prompt task 
  output$prompt_task <- renderText({
    paste("Welche Aussagen bzgl. der graphisch dargestellten Daten sind wahr?")
  }) 

  
  ## Correct answers ###
  correct_answers_task <- reactive({
    questions_answers() %>% 
      filter(answers == T) %>% 
      pull(questions)
  })
  

  ## Feedback task  ####
  output$feedback_task <- renderText({   
    
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
                          task_name = "Korrelogramme_deskriptiv_lesen",
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
                              setdiff(questions_answers()$questions, 
                                      correct_answers_task()),
                              setdiff(questions_answers()$questions, 
                                      input$answers_task))),
                          n_q = 4,
                          partial_credit = max(0,
                                               (n_q - 2*(n_q - ncorrect))/n_q)
                              
                          ),
                   sheet = 2)
  })
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)
