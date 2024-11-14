## Task Name: Visuelle_Schaetzung_Mean_Median ##################################
library(shiny)
library(miniUI)
library(shinyjs)
library(hrbrthemes)
library(tidyverse)
library(learnr)
library(googledrive)
library(googlesheets4)
library(shinycssloaders)
library(shinythemes)
library(thematic)
library(bayestestR)

## Thematic setup ##############################################################
#thematic_shiny(font = "auto")

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
  theme = bslib::bs_theme(
    bg = "#202123",
    fg = "#b8bcc2",
    primary = "#62DC3A"
  ),
  useShinyjs(),
                 miniContentPanel(
                   wellPanel(
                     h4("Aufgabe:"),
                     p("Schätzen Sie das arithmetische Mittel (Mean) und den Median (Median) der unten graphisch dargestellten Daten"),
                     plotOutput("plot_task", width = "400px")
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
                     #,verbatimTextOutput("debug")
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
  
  ## Render UI for Answers #####################################################
  output$ui_answers_task <- renderUI({
    input$reshuffle_task
    input$new_task
    tagList(numericInput("user_mean", "Mean", numeric(0)),
            numericInput("user_median", "Median", numeric(0)))
  })
  

  
  
  ## Data for task #############################################################
  data_task <- reactive({
    input$new_task
    tibble(data = distribution_beta(1000, 
                                    runif(1,.5,3), 
                                    runif(1,.5,3)
                                    )*sample(c(-100:-1, 1:100), 1) + 
             sample(-500:500,1),
           dummy = "data")
  })
  
 
  
  ## Plot task #################################################################
  output$plot_task <- renderPlot({
    
    
    if((as.numeric(input$new_task) %% 5) == 0){
      plot <- 
        ggplot(data_task(), aes(data)) +
        geom_histogram(fill = "#8cd00030",
                       color = "#8cd000") +
        theme_modern_rc() + 
        xlab("") + 
        ylab("") + 
        ggtitle("Histogramm")
    }
    
    if((as.numeric(input$new_task) %% 5) == 1){
      plot <- 
        ggplot(data_task(), aes(dummy, data)) +
        geom_violin(width = .5, 
                    fill = "#8cd00030",
                    color = "#8cd000") +
        theme_modern_rc() + 
        theme(axis.text.y=element_blank()) + 
        xlab("") + 
        ylab("") + 
        ggtitle("Violinplot") + 
        coord_flip()
    }
    
    if((as.numeric(input$new_task) %% 5) == 2){
      plot <- 
        ggplot(data_task(), aes(dummy, data)) +
        ggforce::geom_sina(alpha = .3,
                           fill = "#8cd00030",
                           color = "#8cd000") +
        theme_modern_rc() + 
        theme(axis.text.y=element_blank()) + 
        xlab("") + 
        ylab("") + 
        ggtitle("Sinaplot") + 
        coord_flip()
    }
    
    if((as.numeric(input$new_task) %% 5) == 3){
      plot <- 
        ggplot(data_task(), aes(dummy, data)) +
        geom_boxplot(fill = "#8cd00030",
                     color = "#8cd000") +
        theme_modern_rc() + 
        theme(axis.text.y=element_blank()) + 
        xlab("") + 
        ylab("") + 
        ggtitle("Boxplot") + 
        coord_flip()
    }
    
    if((as.numeric(input$new_task) %% 5) == 4){
      plot <- 
        ggplot(data_task(), aes(data)) +
        geom_density(fill = "#8cd00030",
                     color = "#8cd000") +
        theme_modern_rc() + 
        theme(axis.text.y=element_blank()) + 
        xlab("") + 
        ylab("") + 
        ggtitle("Densityplot")
    }
    
    return(plot)
  })
  
  ## Feedback task #############################################################
  output$feedback_task <- renderText({
    mean <- round2(mean(data_task()$data), 2)
    mean_Abweichung <- round2(100/mean*input$user_mean, 2)
    median <- round2(median(data_task()$data), 2)
    median_Abweichung <- round2(100/median*input$user_median, 2)
    
    if(!is.na(input$user_mean) & !is.na(input$user_median)){
      feedback <-
        paste(
          "Tatsächlicher Mean = ",
          mean,
          " &rArr; Ihre Schätzung entspricht ",
          mean_Abweichung,
          "% des tatsächlichen Werts.<br>",
          "Tatsächlicher Median = ",
          median,
          " &rArr; Ihre Schätzung entspricht ",
          median_Abweichung,
          "% des tatsächlichen Werts."
        )
    }
    if(is.na(input$user_mean) | is.na(input$user_median)){
      feedback <- 
        paste("Bitte geben Sie eine Schätzung für Mean <i>und</i> Median ein")
    }
    
    return(feedback)
    
  })
  
  ## Show and Hide Feedback ####################################################
  ### Show feedback on button click 
  observeEvent(input$show_feedback_task, {
    showElement(id = "feedbackpanel_task")
  })
  
  ### Hide feedback for new data
  observeEvent(c(input$new_task, input$reshuffle_task), {
    hideElement(id = "feedbackpanel_task")
  })
  
  ### Hide feedback when answer changes 
  observeEvent(c(input$user_MAD, input$user_SD), {
    hideElement(id = "feedbackpanel_task")
  })
  
  ## URL Variable fetching #####################################################
  url_vars <- reactive({
    parseQueryString(session$clientData$url_search)
  })
  
  ## Usage Logging #############################################################
  observeEvent(input$show_feedback_task, {
    #if(!is.null(input$user_MAD) & !is.null(input$user_SD)){
      sheet_append("1AZf7EQk-M2Wgej3WJXG1J2xRdql8b7Xiq0SvIiZogUo",
                   tibble(PID = ifelse(is.null(url_vars()$PID), 
                                       "PID is missing", #to keep ncol constant
                                       url_vars()$PID), # Person identifier from URL
                          task_name = "Visuelle_Schaetzung_Mean_Median",
                          task_version = "guessing",
                          time = Sys.time(),
                          timezone = Sys.timezone(),
                          result = # percentage of true solutions as strings
                            paste("Mean_perc =",
                                  round2(100/round2(mean(data_task()$data), 2)*input$user_mean, 2),
                                  "Median_perc =",
                                  round2(100/round2(median(data_task()$data), 2)*input$user_median, 2),
                                  sep = )
                          ),
                   sheet = 1)
     # }
    })
  
  
  ## Debugging #################################################################
  output$debug <- renderText({
    input$user_MAD
  })
}


# Create Shiny object
shinyApp(ui = ui, server = server)
