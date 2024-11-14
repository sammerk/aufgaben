## Name: mean_median_Q1_IQR_sd_aus_grafik_abschaetzen ##########################

library(shiny)
library(miniUI)
library(shinyjs)
library(hrbrthemes)
library(PearsonDS)
library(tidyverse)
library(learnr)
library(shinycssloaders)

set.seed(25051982)


## UI #########################################################################
ui <- miniPage(
  useShinyjs(),
                 miniContentPanel(
                   wellPanel(
                     h4("Verteilung beschreiben"),
                     p("Gegeben sei die folgende Verteilung:"),
                     plotOutput("plot_task", width = "400px")
                   ),
                   shinyjs::hidden(wellPanel(id = "feedbackpanel_task",
                                             withSpinner(
                                               htmlOutput("feedback_task"),
                                               proxy.height = "50px",
                                               color = "#8cd000")
                                             )
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
  
  ## Reactive Paramenters for task  ############################################
  mean_r <- reactive({
    input$new_task
    sample(seq(-100, 100, 10), 1)
  })
  sd_r <- reactive({
    input$new_task
    sample(seq(1, 13, 4), 1)
  })
  modality_r <- reactive({
    input$new_task
    sample(c("unimod", "bimod", "trimod"), 1)
  })
  plottype_r <- reactive({
    input$new_task
    sample(c("hist", "violin", "sina"), 1)
  })
  skewness_r <- reactive({
    input$new_task
    sample(c(0, .8, -.8),1)
  })
  position_of_correct_answer_r <- reactive({
    input$new_task
    input$reshuffle_task
    sample(1:6,1)
  })
  
  
  ## Data for task #############################################################
  data_task <- reactive({
    data <- tibble(
      unimod_raw =
        rpearson(
          600,
          moments = c(
            mean = 0,
            variance = 1,
            skewness = skewness_r(),
            kurtosis = 3
          )
        ),
      
      bimod_raw =
        c(rpearson(
          300,
          moments = c(
            mean = 0,
            variance = 1,
            skewness = skewness_r(),
            kurtosis = 3
          )
        ),
        rpearson(
          300,
          moments = c(
            mean = ifelse(skewness_r() == 0, 4, 3),
            variance = 1,
            skewness = skewness_r(),
            kurtosis = 3
          )
        )),
      
      trimod_raw =
        c(
          rpearson(
            200,
            moments = c(
              mean = 0,
              variance = 1,
              skewness = skewness_r(),
              kurtosis = 3
            )
          ),
          rpearson(
            200,
            moments = c(
              mean = ifelse(skewness_r() == 0, 4, 3),
              variance = 1,
              skewness = skewness_r(),
              kurtosis = 3
            )
          ),
          rpearson(
            200,
            moments = c(
              mean = ifelse(skewness_r() == 0, 8, 6),
              variance = 1,
              skewness = skewness_r(),
              kurtosis = 3
            )
          )
        ),
      unimod = (unimod_raw - mean(unimod_raw))/sd(unimod_raw)*sd_r() +  mean_r(),
      bimod = (bimod_raw - mean(bimod_raw))/sd(bimod_raw)*sd_r() + mean_r(),
      trimod = (trimod_raw - mean(trimod_raw))/sd(trimod_raw)*sd_r() + mean_r(),
      dummy = 1
    )
    
    
    data_to_plot <- data[,c("dummy", modality_r())]
    names(data_to_plot) <- c("dummy", "data")
    
    return(data_to_plot)
  })
  
  ## Answer strings task #######################################################
  answerstrings_task <- reactive({
    
    # Create answer strings
    dist_descriptionstring_true <-
      paste(
        case_when(
          modality_r() == "unimod" ~ "... unimodal, ",
          modality_r() == "bimod" ~ "... bimodal, ",
          TRUE ~ "... trimodal, "),
        case_when(
          skewness_r() == 0.8 ~ "rechtsschief, ",
          skewness_r() == -0.8 ~ "linksschief, ",
          TRUE ~ "symmetrisch, "),
        "hat einen Mittwelwert von ",
        mean_r(),
        " und das erste Quartil liegt bei ",
        round(quantile(data_task()$data, .25), 1),
        sep = "")
    
    dist_descriptionstring_false01 <-
      paste(
        case_when(
          modality_r() == "unimod" ~ "... bimodal, ",
          modality_r() == "bimod" ~ "... trimodal, ",
          TRUE ~ "... unimodal, "),
        case_when(
          skewness_r() == 0.8 ~ "rechtsschief, ",
          skewness_r() == -0.8 ~ "linksschief, ",
          TRUE ~ "symmetrisch, "),
        "hat einen Mittwelwert von ",
        mean_r(),
        " und das erste Quartil liegt bei ",
        round(quantile(data_task()$data, .25), 1),
        sep = "")
    
    dist_descriptionstring_false02 <-
      paste(
        case_when(
          modality_r() == "unimod" ~ "... bimodal, ",
          modality_r() == "bimod" ~ "... unimodal, ",
          TRUE ~ "... trimodal, "),
        case_when(
          skewness_r() == 0.8 ~ "linksschief, ",
          skewness_r() == -0.8 ~ "rechtsschief, ",
          TRUE ~ "symmetrisch, "),
        "hat einen Mittwelwert von ",
        mean_r(),
        " und das erste Quartil liegt bei ",
        round(quantile(data_task()$data, .25), 1) - 1.5*sd_r(), #  immer false!
        sep = "")
    
    dist_descriptionstring_false03 <-
      paste(
        case_when(
          modality_r() == "unimod" ~ "... bimodal, ",
          modality_r() == "bimod" ~ "... unimodal, ",
          TRUE ~ "... trimodal, "),
        case_when(
          skewness_r() == 0.8 ~ "linksschief, ",
          skewness_r() == -0.8 ~ "rechtsschief, ",
          TRUE ~ "symmetrisch, "),
        "hat einen Mittwelwert von ",
        mean_r() + 2*sd_r(), #  immer false!
        " und das erste Quartil liegt bei ",
        round(quantile(data_task()$data, .25), 1) + 2*sd_r(),#  immer false!
        sep = "")
    
    dist_descriptionstring_false04 <-
      paste(
        case_when(
          modality_r() == "unimod" ~ "... unimodal, ",
          modality_r() == "bimod" ~ "... bimodal, ",
          TRUE ~ "... trimodal, "),
        case_when(
          skewness_r() == 0.8 ~ "linksschief, ",
          skewness_r() == -0.8 ~ "rechtsschief, ",
          TRUE ~ "symmetrisch, "),
        "hat einen Mittwelwert von ",
        mean_r() + 2*sd_r(), #  immer false!
        " und das erste Quartil liegt bei ",
        round(quantile(data_task()$data, .25), 1) -1*sd_r(),#  immer false!
        sep = "")
    
    dist_descriptionstring_false05 <-
      paste(
        case_when(
          modality_r() == "unimod" ~ "... unimodal, ",
          modality_r() == "bimod" ~ "... bimodal, ",
          TRUE ~ "... trimodal, "),
        case_when(
          skewness_r() == 0.8 ~ "rechtsschief, ",
          skewness_r() == -0.8 ~ "linksschief, ",
          TRUE ~ "symmetrisch, "),
        "hat einen Mittwelwert von ",
        mean_r(),
        " und das erste Quartil liegt bei ",
        round(quantile(data_task()$data, .25), 1) + 1*sd_r(), #  immer false!
        sep = "")
    
    # concatenate answerstrings ordered according parameter 
    answerstrings <- character(6) # initialize empty vector
    answerstrings[position_of_correct_answer_r()] <- #place correct answer
      dist_descriptionstring_true
    position_of_incorrect_answers <- # positions of incorrect answers
      setdiff(1:6, position_of_correct_answer_r())
    # place incorrect answers
    answerstrings[position_of_incorrect_answers[1]] <- 
      dist_descriptionstring_false01
    answerstrings[position_of_incorrect_answers[2]] <- 
      dist_descriptionstring_false02
    answerstrings[position_of_incorrect_answers[3]] <- 
      dist_descriptionstring_false03
    answerstrings[position_of_incorrect_answers[4]] <- 
      dist_descriptionstring_false04
    answerstrings[position_of_incorrect_answers[5]] <- 
      dist_descriptionstring_false05
    
    return(answerstrings)
  })
  
  ## Render AnswerUI task ######################################################
  
  output$ui_answers_task <- renderUI({
    radioButtons("answers_task",
                 "Welche Aussage ist korrekt? Die dargestellte Verteilung ist ...",
                 choices = c(answerstrings_task()[1],   
                             answerstrings_task()[2],
                             answerstrings_task()[3],
                             answerstrings_task()[4],
                             answerstrings_task()[5],
                             answerstrings_task()[6]),
                 selected = character(0))
  })
  
  ## Plot task #################################################################
  output$plot_task <- renderPlot({
    if(plottype_r() == "hist"){
      plot <- 
        ggplot(data_task(), aes(data)) +
        geom_histogram() +
        theme_ipsum_rc() + 
        xlab("") + 
        ylab("") + 
        ggtitle("Histogramm")
    }
    
    if(plottype_r() == "violin"){
      plot <- 
        ggplot(data_task(), aes(dummy, data)) +
        geom_violin(width = .5) +
        theme_ipsum_rc() + 
        theme(axis.text.y=element_blank()) + 
        xlab("") + 
        ylab("") + 
        ggtitle("Violinplot") + 
        coord_flip()
    }
    
    if(plottype_r() == "sina"){
      plot <- 
        ggplot(data_task(), aes(dummy, data)) +
        ggforce::geom_sina(alpha = .3) +
        theme_ipsum_rc() + 
        theme(axis.text.y=element_blank()) + 
        xlab("") + 
        ylab("") + 
        ggtitle("Sinaplot") + 
        coord_flip()
    }
    
    return(plot)
  })
  
  ## Feedback task #############################################################
  output$feedback_task <- renderText({    

    if(is.null(input$answers_task)){
      HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:  <br>✓ ", 
                 paste(answerstrings_task()[position_of_correct_answer_r()], collapse = ", <br>✓ "),
                 "<br><i>",
                 learnr::random_encouragement(),
                 "</i>"))
    }else{
      if(setequal(answerstrings_task()[position_of_correct_answer_r()], input$answers_task)){
        HTML(paste("Richtig! <br><i>", 
                   learnr::random_praise(),
                   "<i>"))}else{
                     HTML(paste("<b>Leider nicht korrekt!</b> Richtig wäre:  <br>✓ ", 
                                paste(answerstrings_task()[position_of_correct_answer_r()], collapse = ", <br>✓ "),
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
  
  

}


# Create Shiny object
shinyApp(ui = ui, server = server)
