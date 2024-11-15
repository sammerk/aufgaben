## Task Name: Visual_Guessing_Cliff's d SC #####################################
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
library(MASS)
library(GGally)
library(bslib)
library(bayestestR)
library(ggdist)


## Googlesheets Connection Setup ###############################################
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets/"
)

gs4_auth()


## UI ##########################################################################
ui <- miniPage(
   theme = bs_theme(
    bg = "#202123",
    fg = "#b8bcc2",
    primary = "#62DC3A",
    base_font = font_google("Roboto Condensed")
  ),
  useShinyjs(),
  withMathJax(),
                 miniContentPanel(
                   wellPanel(
                     h4("Aufgabe:"),
                     p("In der himmlischen Weihnachtswerkstatt batteln sich die beiden Elfenteams »Jinglies« und »Sparklies«, wer den besseren Medaillenspiegel (Bronze < Silber < Gold < Platin) hat. Schätzen Sie  $$\\text{Cliff's } d = \\frac{\\#(Sparklies_i > Jinglies_i) - \\#(Sparklies_i < Jinglies_i)}{\\#Sparklies \\cdot \\#Jinglies}$$"),
                     withSpinner(
                       plotOutput("plot_task", width = "600px"),
                       proxy.height = "50px",
                       color = "#8cd000")
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
    
    choices <- -10:10*0.3 + Cld()
    choices <- round(choices[choices <= 1 & choices >= -1], 2)
    
    radioButtons("answers_task",
                 "Cliff's d =",
                 choices = choices,
                 selected = character(0))
  })
  

    
    data <- reactive({
      input$new_task
      
      n <- sample(30:30, 1)
      
      data <- 
        tibble(u = distribution_beta(n, .5, .5),
               unif = distribution_beta(n, 1, 1),
               normal = distribution_beta(n, 4, 4),
               skew1 = distribution_beta(n, 1, 1.3),
               skew2 = distribution_beta(n, 1.3, 1),
               skew3 = distribution_beta(n, 5, 20),
               skew4 = distribution_beta(n,20, 5),
               u_2 = distribution_beta(n, .4, .6),
               unif_2 = distribution_beta(n, 1.1, 1),
               normal_2 = distribution_beta(n, 4.1, 3.9),
               skew1_2 = distribution_beta(n, .9, 1.2),
               skew2_2 = distribution_beta(n, 1.2, .9),
               skew3_2 = distribution_beta(n, 4, 21),
               skew4_2 = distribution_beta(n,17, 3)) %>%
        mutate(PID = 1:n()) %>% 
        gather(variable, value, -PID) %>% 
        mutate(Medallien = case_when(value < .25 ~ "Bronze",
                                     value < .5 ~ "Silber",
                                     value < .75 ~ "Gold",
                                     T ~ "Platin"),
               Medallien = factor(Medallien, levels = c("Bronze", "Silber", 
                                                        "Gold", "Platin"))) %>% 
        dplyr::select(-value) %>% 
        spread(variable, Medallien) %>% 
        dplyr::select(-PID) %>% 
        dplyr::relocate(sample(1:ncol(.)))
      
        
      names(data) <- c("Jinglies", "Sparklies", LETTERS[1:13])
      
      data <- 
        data %>% 
        dplyr::select(Jinglies, Sparklies) %>% 
        mutate_all(as.character) %>% 
        gather(Team, Medallien) %>% 
        mutate(Medallien = factor(Medallien, levels = c("Bronze", "Silber", 
                                                      "Gold", "Platin"))) 
      data
  
    return(data)
  })
  
  output$plot_task <- renderPlot({
    
    ggplot(data(), aes(Medallien)) + 
      facet_wrap(~Team) +
      geom_bar(fill = "#8cd000", color = "#8cd000") +
      theme_modern_rc() +
      ylab("Anzahl") +
      ggtitle("Medallienspiegel", "von Jinglies und Sparklies") +
      theme(strip.text = element_text(color = "white"))
    
  })
  
  ## Auxiliary Variables #######################################################
  Cld <- reactive({
    -1*effsize::cliff.delta(Medallien_num ~ Team, data = data() %>% 
                    mutate(Medallien_num = as.numeric(Medallien)))$estimate %>% 
      round(., 2)
  })
  
 
  
  ## Feedback task #############################################################
  output$feedback_task <- renderText({
   
    A_Abweichung <- abs(isolate(Cld()) - as.numeric(input$answers_task))
   
    
    
    if(is.null(input$answers_task)){
      HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:  <br>✓ ", 
                 paste(isolate(Cld()), collapse = ", <br>✓ "),
                 "<br><i>",
                 learnr::random_encouragement(),
                 "</i>"))
    }else{
      if(A_Abweichung == 0){
        HTML(paste("Richtig! <br><i>", 
                   learnr::random_praise(),
                   "<i>"))}else{
                     HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:  <br>✓ ", 
                                paste(isolate(Cld()), collapse = ", <br>✓ "),
                                "<br><i>",
                                learnr::random_encouragement(),
                                "</i>"))
                   }
    }
    
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
  observeEvent(input$answers_task, {
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
                          task_name = "Visual_Guessing_Cliffs_d_SC",
                          task_version = "guessing_SC",
                          time = Sys.time(),
                          timezone = Sys.timezone(),
                          new_task = as.numeric(input$new_task),
                          reshuffle_task = as.numeric(input$reshuffle_task),
                          result = ifelse(abs(isolate(Cld()) - 
                                                as.numeric(input$answers_task)) == 0,
                                          "correct_solution",
                                          "false_solution"),
                          Cld = Cld(),
                          result = input$user_Cld),
                   sheet = 2)
     # }
    })
  
  
  ## Debugging #################################################################

}


# Create Shiny object
shinyApp(ui = ui, server = server)
