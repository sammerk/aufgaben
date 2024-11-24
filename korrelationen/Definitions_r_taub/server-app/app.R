# Task Name: Definitions_r_tau ######################

library(shiny)
library(miniUI)
library(shinyjs)
library(tidyverse)
library(learnr)
library(shinycssloaders)
set.seed(25051982)



## UI #########################################################################
ui <- miniPage(
  withMathJax(),
  useShinyjs(),
  miniContentPanel(
    wellPanel(
      h4("Aufgabe: Aussagen zu Kendall's 𝜏ᵇ und Pearson's r"),
      htmlOutput("prompt_task")
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
  

  ##############################################################################
  # Backend for task  ##########################################################
  ##############################################################################
  
  q_and_a <- reactive({
    tibble(
      questions = c(
        # TRUE
        "Pearson's r stellt eine Effektstärke dar.",
        "Kendall's 𝜏ᵇ stellt eine Effektstärke dar.",
        "Kendall's 𝜏ᵇ kann den Zusammenhang zweier ordinaler Variablen beschreiben",
        "Kendall's 𝜏ᵇ kann den Zusammenhang zweier nicht-normalverteilten intervallskalierter Variablen beschreiben",
        "Kendall's 𝜏ᵇ kann den Zusammenhang zweier intervallskalierter Variablen beschreiben",
        "Kendall's 𝜏ᵇ kann theoretisch Werte zwischen -1 und 1 annehmen (-1 ≤ 𝜏ᵇ ≤ 1)",
        "Pearson's r kann theoretisch Werte zwischen -1 und 1 annehmen (-1 ≤ r ≤ 1)",
        "Kendall's 𝜏ᵇ stellt einen umso stärkeren Effekt dar, »je weiter 𝜏ᵇ von der Null entfernt ist«",
        "Kendall's 𝜏ᵇ stellt einen Nulleffekt dar, wenn 𝜏ᵇ = 0 gilt.",
        "Pearsons's r stellt einen umso stärkeren Effekt dar, »je weiter r von der Null entfernt ist«",
        "Pearsons's r stellt einen Nulleffekt dar, wenn r = 0 gilt",
        "Kendall's 𝜏ᵇ stellt einen umso stärkeren Effekt dar, je größer der Betrag von 𝜏ᵇ (|𝜏ᵇ|) ist",
        "Pearsons's r stellt einen umso stärkeren Effekt dar, je größer der Betrag von r (|r|) ist",
        "Pearson's r kann den Zusammenhang zweier intervallskalierter Variablen beschreiben",
        # FALSE
        "Pearson's r  stellt eine Inferenzstatistik dar.",
        "Kendall's 𝜏ᵇ stellt eine Inferenzstatistik dar.",
        "Kendall's 𝜏ᵇ kann den Zusammenhang zweier nominaler Variablen beschreiben",
        "Kendall's 𝜏ᵇ kann theoretisch Werte zwischen -∞ und ∞ annehmen (-∞ ≤ 𝜏ᵇ ≤ ∞)",
        "Kendall's r kann theoretisch Werte zwischen -∞ und ∞ annehmen (-∞ ≤ r ≤ ∞)",
        "Kendall's 𝜏ᵇ stellt einen starken Effekt dar, wenn es positiv ist",
        "Pearsons's r stellt einen starken Effekt dar, wenn es positiv ist",
        "Je größer die Stichprobe, desto größer Pearson's r",
        "Je größer die Stichprobe, desto größer Kendall's 𝜏ᵇ",
        "Pearson's r kann den Zusammenhang zweier nominaler Variablen beschreiben",
        "Pearson's r kann den Zusammenhang zweier ordinaler Variablen beschreiben",
        "Kendall's 𝜏ᵇ stellt einen Nulleffekt dar, wenn 𝜏ᵇ = 0.5 gilt.",
        "Pearsons's r stellt einen Nulleffekt dar, wenn r = 0.5 gilt"),
    solutions = c(rep(T, 14),
                  rep(F, 13)))%>% 
      sample_frac(1)
  
  })
  
  ## Render UI for Answers ###
  output$ui_answers_task <- renderUI({
    input$reshuffle_task
    input$new_task
    withMathJax(
    checkboxGroupInput(
      "answers_task",
      "Bitte ankreuzen",
      q_and_a()$questions[(as.numeric(input$new_task) %% 9 * 3 + 1):(as.numeric(input$new_task) %% 9 * 3 + 3)],
      selected = character(0)
    ))
  })
  
  ## Prompt task 
  output$prompt_task <- renderText({
    paste("Welche Aussagen sind wahr?")
  }) 
  
  ## Correct answers ###
  correct_answers_task <- reactive({
    q_and_a()[(as.numeric(input$new_task) %% 9 * 3 + 1):(as.numeric(input$new_task) %% 9 * 3 + 3), ] %>% 
      filter(solutions == T) %>% 
      pull(questions)
  })
  
  ## Feedback task  ####
  output$feedback_task <- renderText({   
    
  #  if(is.null(input$answers_task)){
  #    HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:  <br>✓ ", 
  #               paste(correct_answers_task(), collapse = ", <br>✓ "),
  #               "<br><i>",
  #               learnr::random_encouragement(),
  #               "</i>"))
  #  }else{
      if(setequal(correct_answers_task(), input$answers_task)){
        paste("Richtig!", learnr::random_praise())}else{
          HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:  <br>✓ ", 
                     paste(correct_answers_task(), collapse = ", <br>✓ "),
                     "<br><i>",
                     learnr::random_encouragement(),
                     "</i>"))
       # }
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
