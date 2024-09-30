## Task Name: Visual_Guessing_U3 SC #############################################
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
                     p("In der himmlischen Weihnachtswerkstatt batteln sich die beiden Elfenteams »Jinglies« und »Sparklies«, wer mit der Fertigung welcher Geschenke schneller fertig ist. Dabei kommen die unten dargestellten Daten zustande. Schätzen Sie $$U_3 = \\frac{\\#(Sparklies_i > \\overline{Jinglies})}{\\#Sparklies}$$"),
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
    
    choices <- -15:15*0.2 + u3()
    choices <- round(choices[choices <= 1 & choices >= 0], 2)
    
    radioButtons("answers_task",
                 "Cohen's U3 =",
                 choices = choices,
                 selected = character(0))
  })
  

  
  data <- reactive({
    input$new_task
    
    n <- sample(30:300, 1)
    
    data <- 
      tibble(`Geschenk A Jinglies` = distribution_normal(n)*runif(1, .5, 5)) %>% 
      mutate(`Geschenk A Jinglies` = `Geschenk A Jinglies` + abs(min(`Geschenk A Jinglies`)) + runif(1, 0, sd(`Geschenk A Jinglies`)*5),
             `Geschenk A Sparklies` = `Geschenk A Jinglies` + runif(1, .1, 1.5)*sd(`Geschenk A Jinglies`)) %>% 
      gather(variable, `Fertigungsdauer [Sekunden]`) %>% 
      mutate(Geschenke = substr(variable, 1, 10),
             Elfenteam = substr(variable, 12, 99))
    
    # randomize WHO is faster
    if(sample(1:2, 1) == 1){
      data <- 
        data %>% 
        mutate(Elfenteam = ifelse(Elfenteam == "Sparklies", "Jinglies", "Sparklies")
        )
    }
    
    # randomize WHAT is faster
    if(sample(1:2, 1) == 1){
      data <- 
        data %>% 
        mutate(Geschenke = ifelse(Geschenke == "Geschenk A", "Geschenk B", "Geschenk A")
        )
    }
    
    
    return(data)
  })
  
  output$plot_task <- renderPlot({
    
    if(sample(1:4, 1) == 1){
      ggplot(data(), aes(Elfenteam, `Fertigungsdauer [Sekunden]`)) +
        facet_wrap(~Geschenke) +
        geom_boxplot(color = "#8cd000", fill = "#8cd00030", width = .2) +
        theme_modern_rc() +
        coord_flip()
    }else{
      if(sample(1:3, 1) == 1){
        ggplot(data(), aes(Elfenteam, `Fertigungsdauer [Sekunden]`)) +
          facet_wrap(~Geschenke) +
          stat_dots(color = "#8cd000", fill = "#8cd00030") +
          theme_modern_rc() +
          coord_flip() 
      }else{
        if(sample(1:2, 1) == 1){
          ggplot(data(), aes(Elfenteam, `Fertigungsdauer [Sekunden]`)) +
            facet_wrap(~Geschenke) +
            stat_halfeye(color = "#8cd000", fill = "#8cd00030") +
            theme_modern_rc() +
            coord_flip()
        }else{
          ggplot(data(), aes(Elfenteam, `Fertigungsdauer [Sekunden]`)) +
            facet_wrap(~Geschenke) +
            stat_histinterval(color = "#8cd000", fill = "#8cd00030") +
            theme_modern_rc() +
            coord_flip()
        }
      }
    }
    
  })
  ## Auxiliary Variables #######################################################
  d <- reactive({
   -1*effsize::cohen.d(`Fertigungsdauer [Sekunden]` ~ Elfenteam, data = data())$estimate %>% 
      round(., 2)
  })
  
  u3 <- reactive({
    pnorm(d())%>% 
      round(., 2)
  })
  
  ol <- reactive({
    2*pnorm(-abs(d())/2)%>% 
      round(., 2)
  })
  
  ## Feedback task #############################################################
  output$feedback_task <- renderText({
   
    u3_Abweichung <- abs(u3() - as.numeric(input$answers_task))
    
    if(is.null(input$answers_task)){
      HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:  <br>✓ ", 
                 paste(isolate(u3()), collapse = ", <br>✓ "),
                 "<br><i>",
                 learnr::random_encouragement(),
                 "</i>"))
    }else{
      if(u3_Abweichung == 0){
        HTML(paste("Richtig! <br><i>", 
                   learnr::random_praise(),
                   "<i>"))}else{
                     HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:  <br>✓ ", 
                                paste(isolate(u3()), collapse = ", <br>✓ "),
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
                          task_name = "Visual_Guessing_U3_SC",
                          task_version = "guessing",
                          time = Sys.time(),
                          timezone = Sys.timezone(),
                          new_task = as.numeric(input$new_task),
                          reshuffle_task = as.numeric(input$reshuffle_task),
                          result = ifelse(abs(isolate(u3()) - 
                                                as.numeric(input$answers_task)) == 0,
                                          "correct_solution",
                                          "false_solution"),
                          u3 = u3()
                          ),
                   sheet = 2)
     # }
    })
  
  
  ## Debugging #################################################################
  output$debug <- renderText({
    input$user_V
  })
}


# Create Shiny object
shinyApp(ui = ui, server = server)
